// *HEAPS* of unsafe code here :p

use crate::value::*;
use parking_lot::Mutex;
use std::{
    cell::OnceCell,
    collections::HashSet,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    ptr::{null_mut, NonNull},
    sync::atomic::AtomicBool,
};

use crate::{
    undefined_mut, ArgsT, Function, InternalObjectData, Object, Pointer, ValueInner, ValueT,
};

struct StackFrame {
    vars: Vec<NonNull<*mut ValueT>>,
}

struct Gc {
    statics: Vec<NonNull<*mut ValueT>>,
    stack_frames: Vec<StackFrame>,
    objects: OnceCell<HashSet<HeapValue>>,
}

static ENABLE_LOGGING: AtomicBool = AtomicBool::new(false);

static mut GC: Mutex<Gc> = Mutex::new(Gc {
    statics: vec![],
    stack_frames: vec![],
    objects: OnceCell::new(),
});

pub fn init() {
    unsafe {
        GC.lock().objects.get_or_init(|| HashSet::new());
    }
}

pub fn alloc<'a>(value: ValueT) -> &'a mut ValueT {
    let ptr = Box::leak(Box::new(value));
    unsafe {
        let mut gc = GC.lock();
        gc.objects.get_mut().unwrap().insert(HeapValue { v: ptr });
    }
    ptr
}

pub fn register_static(v: NonNull<*mut ValueT>) {
    unsafe {
        GC.lock().statics.push(v);
    }
}

#[no_mangle]
pub extern "C" fn swcjs_gc_register_static(v: NonNull<*mut ValueT>) {
    register_static(v)
}

/// Begin a new stack frame
#[no_mangle]
pub extern "C" fn swcjs_gc_begin_frame() {
    unsafe {
        GC.lock().stack_frames.push(StackFrame { vars: vec![] });
    }
}

/// End the current stack frame
#[no_mangle]
pub extern "C" fn swcjs_gc_end_frame() {
    unsafe {
        GC.lock().stack_frames.pop();
    }
}

/// Add a variable to the current stack frame.
#[no_mangle]
pub extern "C" fn swcjs_gc_stack_add(v: NonNull<*mut ValueT>) {
    unsafe {
        GC.lock().stack_frames.last_mut().unwrap().vars.push(v);
    }
}

/// Wrap heap pointer to implement hash and eq on the pointer
#[derive(Clone, Copy)]
struct HeapValue {
    v: *mut ValueT,
}

impl HeapValue {
    fn new(v: *mut ValueT) -> Option<Self> {
        if v == null_mut() || v == undefined_mut() {
            None
        } else {
            Some(Self { v })
        }
    }
}

impl Eq for HeapValue {}

impl PartialEq for HeapValue {
    fn eq(&self, other: &Self) -> bool {
        (self.v as *const ValueT as usize) == (other.v as *const ValueT as usize)
    }
}

impl Hash for HeapValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.v as *const ValueT as usize).hash(state)
    }
}

impl Deref for HeapValue {
    type Target = ValueT;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.v }
    }
}

impl DerefMut for HeapValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.v }
    }
}

struct GcState {
    marked: HashSet<HeapValue>,
}

impl GcState {
    fn run(&mut self, gc: &mut Gc) {
        for v in &gc.statics {
            self.mark(unsafe { *v.as_ptr() });
        }

        for frame in &gc.stack_frames {
            for v in &frame.vars {
                self.mark(unsafe { *v.as_ptr() });
            }
        }

        let objects = gc.objects.get_mut().unwrap();
        for obj in objects.extract_if(|v| !self.marked.contains(v)) {
            if ENABLE_LOGGING.load(std::sync::atomic::Ordering::Relaxed) {
                eprintln!("GC: Dropping: {:p}: {:?}", obj.v, unsafe { &*obj.v });
            }
            unsafe {
                drop(Box::from_raw(obj.v));
            }
        }
    }

    fn mark_all(&mut self, values: impl Iterator<Item = impl Deref<Target = *mut ValueT>>) {
        for value in values {
            self.mark(*value.deref());
        }
    }

    fn mark(&mut self, value: *mut ValueT) {
        if let Some(value) = HeapValue::new(value) {
            if self.marked.contains(&value) {
                return;
            }

            self.marked.insert(value);
            let ValueT { inner } = value.deref();

            match inner {
                ValueInner::Object(obj) => {
                    let Object {
                        constructor,
                        internal_data,
                        properties,
                    } = obj;
                    self.mark(*constructor);
                    self.mark_all(properties.values());
                    match internal_data {
                        InternalObjectData::Array(array) => {
                            self.mark_all(array.iter());
                        }
                        InternalObjectData::None => {}
                    }
                }
                ValueInner::Function(func) => {
                    let Function {
                        pointer: _,
                        capture,
                        this,
                        prototype,
                        properties,
                    } = func;
                    self.mark(*this);
                    self.mark(*prototype);
                    self.mark_all(capture.iter());
                    self.mark_all(properties.values());
                }
                ValueInner::Boolean(_) | ValueInner::Number(_) | ValueInner::String(_) => {}
            }
        }
    }
}

pub fn run_gc() {
    let mut gc = unsafe { GC.lock() };
    GcState {
        marked: HashSet::new(),
    }
    .run(&mut gc)
}

#[no_mangle]
pub extern "C" fn swcjs_gc_run() {
    run_gc();
}

pub(crate) extern "C" fn swcjs_gc(_args: &ArgsT) -> *mut ValueT {
    run_gc();
    undefined_mut()
}

pub(crate) extern "C" fn swcjs_gc_enable_logging(_args: &ArgsT) -> *mut ValueT {
    ENABLE_LOGGING.store(true, std::sync::atomic::Ordering::Relaxed);
    undefined_mut()
}

pub(crate) extern "C" fn swcjs_gc_disable_logging(_args: &ArgsT) -> *mut ValueT {
    ENABLE_LOGGING.store(false, std::sync::atomic::Ordering::Relaxed);
    undefined_mut()
}

pub(crate) extern "C" fn swcjs_gc_store_ptr(_args: &ArgsT) -> *mut ValueT {
    let pointer = _args.args[0] as usize;

    crate::swcjs_object! {
        // TODO: use bigint
        "ptr": make_string(format!("{}", pointer))
    }
}

pub(crate) extern "C" fn swcjs_gc_assert_saved(_args: &ArgsT) -> *mut ValueT {
    let pointer = Pointer::from(_args.args[0])
        .as_value()
        .expect("Expected value")
        .as_object()
        .expect("Expected object");

    let ptr = Pointer::from(*pointer.properties.get("ptr").expect("Expected ptr field"))
        .as_value()
        .expect("Expected ptr field not undefined")
        .as_string()
        .expect("Expected ptr field string");
    let ptr_usize = ptr
        .parse::<usize>()
        .expect("Expected ptr field to be a number");
    let ptr = HeapValue::new(ptr_usize as *mut ValueT).unwrap();
    unsafe {
        assert!(
            GC.lock().objects.get_mut().unwrap().contains(&ptr),
            "{ptr_usize} is not saved!"
        );
    }

    undefined_mut()
}

pub(crate) extern "C" fn swcjs_gc_assert_freed(_args: &ArgsT) -> *mut ValueT {
    let pointer = Pointer::from(_args.args[0])
        .as_value()
        .expect("Expected value")
        .as_object()
        .expect("Expected object");

    let ptr = Pointer::from(*pointer.properties.get("ptr").expect("Expected ptr field"))
        .as_value()
        .expect("Expected ptr field not undefined")
        .as_string()
        .expect("Expected ptr field string");
    let ptr_usize = ptr
        .parse::<usize>()
        .expect("Expected ptr field to be a number");
    let ptr = HeapValue::new(ptr_usize as *mut ValueT).unwrap();
    unsafe {
        assert!(
            !GC.lock().objects.get_mut().unwrap().contains(&ptr),
            "{ptr_usize} is not freed!"
        );
    }

    undefined_mut()
}
