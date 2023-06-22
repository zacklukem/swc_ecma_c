use std::sync::Mutex;

use crate::ValueT;

struct StackFrame {
    vars: Vec<*const *mut ValueT>,
}

struct Gc {
    statics: Vec<*mut ValueT>,
    stack_frame: Vec<StackFrame>,
}

static mut GC: Mutex<Gc> = Mutex::new(Gc {
    statics: vec![],
    stack_frame: vec![],
});

pub fn register_static(v: *mut ValueT) {
    unsafe {
        GC.lock().unwrap().statics.push(v);
    }
}

#[no_mangle]
pub extern "C" fn swcjs_gc_register_static(v: *mut ValueT) {
    register_static(v)
}

/// Begin a new stack frame
#[no_mangle]
pub extern "C" fn swcjs_gc_begin_frame() {
    unsafe {
        GC.lock()
            .unwrap()
            .stack_frame
            .push(StackFrame { vars: vec![] });
    }
}

/// End the current stack frame
#[no_mangle]
pub extern "C" fn swcjs_gc_end_frame() {
    unsafe {
        GC.lock().unwrap().stack_frame.pop();
    }
}

/// Add a variable to the current stack frame.
#[no_mangle]
pub extern "C" fn swcjs_gc_stack_add(v: *const *mut ValueT) {
    unsafe {
        GC.lock()
            .unwrap()
            .stack_frame
            .last_mut()
            .unwrap()
            .vars
            .push(v);
    }
}
