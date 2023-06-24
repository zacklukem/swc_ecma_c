use crate::{
    alloc, class_function, undefined_mut, value::make_number, ArgsT, InternalObjectData, Object,
    Pointer, PointerMut, ValueT,
};

// Careful with this
unsafe fn extract_array_data<'a>(
    array_ptr: *mut ValueT,
) -> (&'a mut Vec<*mut ValueT>, &'a mut Object) {
    let array = PointerMut::from(array_ptr)
        .as_value()
        .expect("Must call as constructor")
        .as_object_mut()
        .unwrap();
    let arr_ptr = array as *mut Object;

    let values = array.internal_data.as_array_mut().unwrap();

    // EVIL RUST
    (values, &mut *arr_ptr)
}

fn update_fields(array: &mut Object) {
    let data = array.internal_data.as_array().unwrap();
    array
        .properties
        .insert("length".into(), make_number(data.len() as f64));
}

extern "C" fn array_at(args: &ArgsT) -> *mut ValueT {
    swcjs_expr_computed_member(args.this, args.args[0])
}

extern "C" fn array_push(args: &ArgsT) -> *mut ValueT {
    let (array, object) = unsafe { extract_array_data(args.this) };
    let value = args.args[0];
    // TODO: check spec

    array.push(value);

    update_fields(object);

    undefined_mut()
}

extern "C" fn array_pop(args: &ArgsT) -> *mut ValueT {
    let (array, object) = unsafe { extract_array_data(args.this) };
    // TODO: check spec

    let out = array.pop().unwrap_or(undefined_mut());
    update_fields(object);
    out
}

pub(crate) fn construct_array(array_ptr: *mut ValueT, values: Vec<*mut ValueT>) {
    let array = PointerMut::from(array_ptr)
        .as_value()
        .expect("Must call as constructor")
        .as_object_mut()
        .unwrap();

    array.constructor = unsafe { crate::global_constructors::swcjs_global_Array };

    array
        .properties
        .insert("length".into(), make_number(values.len() as f64));

    macro_rules! method {
        ($name: literal, $func: expr) => {
            array
                .properties
                .insert($name.into(), class_function($func, array_ptr));
        };
    }

    method!("at", array_at);
    method!("push", array_push);
    method!("pop", array_pop);

    array.internal_data = InternalObjectData::Array(values);
}

#[no_mangle]
pub unsafe extern "C" fn swcjs_lit_array(argc: u16, mut args: ...) -> *mut ValueT {
    let mut values = Vec::with_capacity(argc as usize);

    for _ in 0..argc {
        values.push(args.arg());
    }

    let array = alloc(ValueT::Object(Object::default()));

    construct_array(array, values);

    array
}

#[no_mangle]
pub extern "C" fn swcjs_expr_computed_member(
    value: *mut ValueT,
    index: *mut ValueT,
) -> *mut ValueT {
    // TODO: handle non-array types
    let (array, _) = unsafe { extract_array_data(value) };
    // TODO: return undefined if invalid type
    let index = *Pointer::from(index)
        .as_value()
        .unwrap()
        .as_number()
        .unwrap() as usize;

    if index >= array.len() {
        undefined_mut()
    } else {
        array[index]
    }
}
