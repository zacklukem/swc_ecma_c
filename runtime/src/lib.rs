#![crate_type = "staticlib"]
#![feature(c_variadic, hash_extract_if)]

use gc::alloc;
use libc::c_char;
use std::{
    collections::HashMap,
    ffi::CStr,
    ptr::{null, null_mut, NonNull},
};

pub mod binop;
pub mod gc;

pub struct ArgsT {
    pub args: Vec<*mut ValueT>,
    pub capture: &'static [*mut ValueT],
    pub this: *mut ValueT,
}

#[derive(Debug)]
pub struct ValueT {
    pub inner: ValueInner,
}

#[derive(Debug)]
pub enum ValueInner {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Function),
    Object(Object),
}

macro_rules! value_constructor {
    ($name: ident, $inner_ty: ty) => {
        #[allow(non_snake_case)]
        pub fn $name(n: $inner_ty) -> Self {
            Self {
                inner: ValueInner::$name(n),
            }
        }
    };
}

impl ValueT {
    value_constructor!(Number, f64);
    value_constructor!(String, String);
    value_constructor!(Boolean, bool);
    value_constructor!(Function, Function);
    value_constructor!(Object, Object);
}

#[derive(Debug, Default)]
pub struct Object {
    pub properties: HashMap<String, *mut ValueT>,
}

#[derive(Debug)]
pub struct Function {
    pub pointer: extern "C" fn(args: &ArgsT) -> *mut ValueT,
    pub capture: Vec<*mut ValueT>,
    pub this: *mut ValueT,
}

impl Function {
    fn internal(pointer: extern "C" fn(args: &ArgsT) -> *mut ValueT) -> Self {
        Self {
            pointer,
            capture: vec![],
            this: undefined_mut(),
        }
    }
}

pub const fn undefined<T>() -> *const T {
    1 as *const T
}

pub const fn undefined_mut<T>() -> *mut T {
    1 as *mut T
}

#[derive(Debug)]
enum Pointer<'a, T> {
    Null,
    Undefined,
    Value(&'a T),
}

#[derive(Debug)]
enum PointerMut<'a, T> {
    Null,
    Undefined,
    Value(&'a mut T),
}

impl<'a, T> From<*mut T> for PointerMut<'a, T> {
    fn from(ptr: *mut T) -> Self {
        if ptr == null_mut() {
            PointerMut::Null
        } else if ptr == undefined_mut() {
            PointerMut::Undefined
        } else {
            PointerMut::Value(unsafe { &mut *ptr })
        }
    }
}

impl<'a, T> From<*const T> for Pointer<'a, T> {
    fn from(ptr: *const T) -> Self {
        if ptr == null() {
            Pointer::Null
        } else if ptr == undefined() {
            Pointer::Undefined
        } else {
            Pointer::Value(unsafe { &*ptr })
        }
    }
}

impl<'a, T> From<*mut T> for Pointer<'a, T> {
    fn from(ptr: *mut T) -> Self {
        if ptr == null_mut() {
            Pointer::Null
        } else if ptr == undefined_mut() {
            Pointer::Undefined
        } else {
            Pointer::Value(unsafe { &*ptr })
        }
    }
}

///////

#[no_mangle]
pub static mut swcjs_global_console: *mut ValueT = undefined_mut();

#[no_mangle]
pub static mut swcjs_global___swcjs__: *mut ValueT = undefined_mut();

extern "C" fn console_log(args: &ArgsT) -> *mut ValueT {
    for (i, arg) in args.args.iter().cloned().enumerate() {
        if i != 0 {
            print!(" ");
        }
        match Pointer::from(arg) {
            Pointer::Null => {
                print!("null");
            }
            Pointer::Undefined => {
                print!("undefined");
            }
            Pointer::Value(value) => match &value.inner {
                ValueInner::Number(n) => {
                    print!("{}", n);
                }
                ValueInner::String(s) => {
                    print!("{}", s);
                }
                ValueInner::Boolean(b) => {
                    print!("{}", b);
                }
                ValueInner::Function(_) => {
                    print!("[Function]");
                }
                ValueInner::Object(_) => {
                    print!("[Object]");
                } //
            },
        }
    }

    println!();

    undefined_mut()
}

macro_rules! swcjs_object {
    ($($name: literal : $value: expr),*) => {
        {
            let mut object: Object = Default::default();
            $(
                object.properties.insert($name.to_string(), $value);
            )*
            alloc(ValueT::Object(object))
        }
    };
}

#[no_mangle]
pub extern "C" fn swcjs_initialize() {
    gc::init();

    init_console();
    init_swcjs();
}

fn init_swcjs() {
    let gc_fn = alloc(ValueT::Function(Function::internal(gc::swcjs_gc)));

    unsafe {
        swcjs_global___swcjs__ = swcjs_object! {
            "gc": gc_fn
        };
        gc::register_static(NonNull::from(&swcjs_global___swcjs__));
    }
}

fn init_console() {
    let console_log = alloc(ValueT::Function(Function::internal(console_log)));

    unsafe {
        swcjs_global_console = swcjs_object! {
            "log": console_log
        };
        gc::register_static(NonNull::from(&swcjs_global_console));
    }
}

#[no_mangle]
pub extern "C" fn swcjs_lit_str(s: *const c_char) -> *mut ValueT {
    debug_assert!(s != null());
    debug_assert!(s != undefined());
    let s = unsafe { std::ffi::CStr::from_ptr(s) };
    let s = s.to_string_lossy().to_string();

    alloc(ValueT::String(s))
}

#[no_mangle]
pub extern "C" fn swcjs_lit_num(v: f64) -> *mut ValueT {
    alloc(ValueT::Number(v))
}

#[no_mangle]
pub extern "C" fn swcjs_lit_bool(v: libc::c_uchar) -> *mut ValueT {
    if v == 0 {
        return alloc(ValueT::Boolean(false));
    } else {
        return alloc(ValueT::Boolean(true));
    }
}

#[no_mangle]
pub extern "C" fn swcjs_if_condition(con: *const ValueT) -> bool {
    let con = Pointer::from(con);
    match con {
        Pointer::Value(ValueT {
            inner: ValueInner::Boolean(b),
            ..
        }) => *b,
        _ => todo!(),
    }
}

#[no_mangle]
pub extern "C" fn swcjs_args_get_this(con: *const ArgsT) -> *mut ValueT {
    debug_assert!(con != null());
    debug_assert!(con != undefined());
    let con = unsafe { &*con };
    con.this
}

#[no_mangle]
pub extern "C" fn swcjs_args_nth(con: *const ArgsT, n: u16) -> *mut ValueT {
    debug_assert!(con != null());
    debug_assert!(con != undefined());
    let con = unsafe { &*con };
    con.args[n as usize]
}

#[no_mangle]
pub extern "C" fn swcjs_init_global_fn(fun: extern "C" fn(&ArgsT) -> *mut ValueT) -> *mut ValueT {
    // TODO: capture this from module
    alloc(ValueT::Function(Function::internal(fun)))
}

#[no_mangle]
pub unsafe extern "C" fn swcjs_expr_call(
    fun: *const ValueT,
    argc: u16,
    mut args: ...
) -> *mut ValueT {
    let mut args_list: Vec<*mut ValueT> = vec![];
    for _ in 0..argc {
        args_list.push(args.arg())
    }

    let fun = Pointer::from(fun);

    if let Pointer::Value(ValueT {
        inner: ValueInner::Function(fun),
        ..
    }) = fun
    {
        (fun.pointer)(&ArgsT {
            args: args_list,
            capture: &fun.capture,
            this: fun.this,
        })
    } else {
        // TODO: Throw TypeError
        panic!("{:?} is not a function", fun);
    }
}

extern "C" fn function_bind(args: &ArgsT) -> *mut ValueT {
    // TODO: handle no arg passed
    // TODO: handle more than 1 arg passed (currying)
    let new_this = args.args[0];
    let old_fun = Pointer::from(args.capture[0]);
    if let Pointer::Value(ValueT {
        inner: ValueInner::Function(old_fun),
        ..
    }) = old_fun
    {
        alloc(ValueT::Function(Function {
            pointer: old_fun.pointer,
            capture: old_fun.capture.clone(),
            this: new_this,
        }))
    } else {
        // Capture set by internal code
        unreachable!("{:?} is not a function", old_fun);
    }
}

#[no_mangle]
pub extern "C" fn swcjs_expr_member(val_raw: *mut ValueT, prop: *const c_char) -> *mut ValueT {
    let val = PointerMut::from(val_raw);
    let prop = unsafe { std::ffi::CStr::from_ptr(prop) };
    let prop = prop.to_string_lossy().to_string();

    if let PointerMut::Value(ValueT { inner, .. }) = val {
        match &inner {
            ValueInner::Object(obj) => obj
                .properties
                .get(&prop)
                .cloned()
                .unwrap_or(undefined_mut()),
            ValueInner::Function(_) => match prop.as_str() {
                "bind" => alloc(ValueT::Function(Function {
                    pointer: function_bind,
                    capture: vec![val_raw],
                    this: undefined_mut(),
                })),
                _ => todo!(),
            },
            _ => todo!(),
        }
    } else {
        panic!("{:?} is not an object", val);
    }
}

#[no_mangle]
pub extern "C" fn swcjs_expr_set_member(
    val: *mut ValueT,
    prop: *const c_char,
    new_value: *mut ValueT,
) -> *mut ValueT {
    let val = PointerMut::from(val);
    let prop = unsafe { std::ffi::CStr::from_ptr(prop) };
    let prop = prop.to_string_lossy().to_string();

    if let PointerMut::Value(ValueT {
        inner: ValueInner::Object(obj),
        ..
    }) = val
    {
        obj.properties.insert(prop, new_value);
        new_value
    } else {
        panic!("{:?} is not an object", val);
    }
}

#[no_mangle]
pub unsafe extern "C" fn swcjs_expr_init_obj(num_fields: u16, mut args: ...) -> *mut ValueT {
    let mut obj = Object::default();

    for _ in 0..num_fields {
        // unsafe: gotta trust the generated code
        let field_name = CStr::from_ptr(args.arg()).to_string_lossy();
        let field_value: *mut ValueT = args.arg();
        obj.properties.insert(field_name.to_string(), field_value);
    }

    alloc(ValueT::Object(obj))
}
