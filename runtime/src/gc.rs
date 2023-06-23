// *HEAPS* of unsafe code here :p

use parking_lot::Mutex;
use std::{
    cell::OnceCell,
    collections::HashSet,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    ptr::{null_mut, NonNull},
};

use crate::{undefined_mut, ArgsT, ValueInner, ValueT};

struct StackFrame {
    vars: Vec<NonNull<*mut ValueT>>,
}

struct Gc {
    statics: Vec<NonNull<*mut ValueT>>,
    stack_frames: Vec<StackFrame>,
    objects: OnceCell<HashSet<HeapValue>>,
}

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
            println!("GC: Dropping");
            unsafe {
                drop(Box::from_raw(obj.v));
            }
        }
    }

    fn mark(&mut self, value: *mut ValueT) {
        if let Some(value) = HeapValue::new(value) {
            if self.marked.contains(&value) {
                return;
            }

            self.marked.insert(value);

            match &value.inner {
                ValueInner::Object(obj) => {
                    for v in obj.properties.values() {
                        self.mark(*v);
                    }
                }
                _ => {}
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

pub(crate) extern "C" fn swcjs_gc(_args: &ArgsT) -> *mut ValueT {
    run_gc();
    undefined_mut()
}
