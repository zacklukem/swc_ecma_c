use crate::{gc, undefined_mut, ArgsT, Function, ValueT};
use std::ptr::NonNull;

#[no_mangle]
pub static mut swcjs_global_Object: *mut ValueT = undefined_mut();

extern "C" fn object_constructor(_args: &ArgsT) -> *mut ValueT {
    // TODO
    undefined_mut()
}

#[no_mangle]
pub static mut swcjs_global_Number: *mut ValueT = undefined_mut();

extern "C" fn number_constructor(_args: &ArgsT) -> *mut ValueT {
    // TODO
    undefined_mut()
}

#[no_mangle]
pub static mut swcjs_global_String: *mut ValueT = undefined_mut();

extern "C" fn string_constructor(_args: &ArgsT) -> *mut ValueT {
    // TODO
    undefined_mut()
}

#[no_mangle]
pub static mut swcjs_global_Function: *mut ValueT = undefined_mut();

extern "C" fn function_constructor(_args: &ArgsT) -> *mut ValueT {
    // TODO
    undefined_mut()
}

#[no_mangle]
pub static mut swcjs_global_Boolean: *mut ValueT = undefined_mut();

extern "C" fn boolean_constructor(_args: &ArgsT) -> *mut ValueT {
    // TODO
    undefined_mut()
}

pub fn init() {
    macro_rules! initialize_constructor {
        ($global: ident, $constructor: ident) => {
            unsafe {
                $global = crate::alloc(ValueT::Function(Function::internal($constructor)));
                gc::register_static(NonNull::from(&$global));
            }
        };
    }

    initialize_constructor!(swcjs_global_Object, object_constructor);
    initialize_constructor!(swcjs_global_Number, number_constructor);
    initialize_constructor!(swcjs_global_String, string_constructor);
    initialize_constructor!(swcjs_global_Function, function_constructor);
    initialize_constructor!(swcjs_global_Boolean, boolean_constructor);
}
