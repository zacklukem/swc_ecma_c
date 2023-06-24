use crate::{
    alloc, array::construct_array, gc, internal_function, undefined_mut, ArgsT, Function, Object,
    Pointer, ValueInner, ValueT,
};
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

#[no_mangle]
pub static mut swcjs_global_Array: *mut ValueT = undefined_mut();

extern "C" fn array_constructor(args: &ArgsT) -> *mut ValueT {
    construct_array(args.this, args.args.clone());
    undefined_mut()
}

extern "C" fn object_create(args: &ArgsT) -> *mut ValueT {
    let proto = Pointer::from(args.args[0]);

    let mut new_obj = Object::default();

    match proto {
        Pointer::Null => todo!(),
        Pointer::Value(ValueT {
            inner: ValueInner::Object(proto),
        }) => {
            new_obj.properties = proto.properties.clone();
        }
        // TODO: exception
        _ => panic!("Cannot create object with undefined prototype"),
    }

    alloc(ValueT::Object(new_obj))
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

    unsafe {
        let mut fun = Function::internal(object_constructor);
        fun.properties
            .insert("create".to_string(), internal_function(object_create));
        swcjs_global_Object = crate::alloc(ValueT::Function(fun));
    }
    initialize_constructor!(swcjs_global_Number, number_constructor);
    initialize_constructor!(swcjs_global_String, string_constructor);
    initialize_constructor!(swcjs_global_Function, function_constructor);
    initialize_constructor!(swcjs_global_Boolean, boolean_constructor);
    initialize_constructor!(swcjs_global_Array, array_constructor);
}
