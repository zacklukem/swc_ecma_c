#![crate_type = "staticlib"]
#![feature(c_variadic)]
use libc::c_char;
use std::{
    collections::HashMap,
    ptr::{null, null_mut},
};

pub mod binop;

pub struct ArgsT {
    pub args: Vec<*mut ValueT>,
}

#[derive(Debug)]
pub enum ValueT {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Function),
    Object(Object),
}

#[derive(Debug, Default)]
pub struct Object {
    pub properties: HashMap<String, *mut ValueT>,
}

#[derive(Debug)]
pub struct Function {
    pub pointer: extern "C" fn(args: &ArgsT) -> *mut ValueT,
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

pub fn alloc<'a, T>(value: T) -> &'a mut T {
    Box::leak(Box::new(value))
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
pub static mut console: *mut ValueT = undefined_mut();

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
            Pointer::Value(ValueT::Number(n)) => {
                print!("{}", n);
            }
            Pointer::Value(ValueT::String(s)) => {
                print!("{}", s);
            }
            Pointer::Value(ValueT::Boolean(b)) => {
                print!("{}", b);
            }
            Pointer::Value(ValueT::Function(_)) => {
                print!("[Function]");
            }
            Pointer::Value(ValueT::Object(_)) => {
                print!("[Object]");
            } //
        }
    }

    println!();

    undefined_mut()
}

#[no_mangle]
pub extern "C" fn swcjs_initialize() {
    let console_log = alloc(ValueT::Function(Function {
        pointer: console_log,
    }));

    let mut console_object: Object = Default::default();

    console_object
        .properties
        .insert("log".to_string(), console_log);

    unsafe {
        console = alloc(ValueT::Object(console_object));
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
        Pointer::Value(ValueT::Boolean(b)) => *b,
        _ => todo!(),
    }
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
    alloc(ValueT::Function(Function { pointer: fun }))
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

    if let Pointer::Value(ValueT::Function(fun)) = fun {
        (fun.pointer)(&ArgsT { args: args_list })
    } else {
        // TODO: Throw TypeError
        panic!("{:?} is not a function", fun);
    }
}

#[no_mangle]
pub extern "C" fn swcjs_expr_member(val: *mut ValueT, prop: *const c_char) -> *mut ValueT {
    let val = PointerMut::from(val);
    let prop = unsafe { std::ffi::CStr::from_ptr(prop) };
    let prop = prop.to_string_lossy().to_string();

    if let PointerMut::Value(ValueT::Object(obj)) = val {
        obj.properties
            .get(&prop)
            .cloned()
            .unwrap_or(undefined_mut())
    } else {
        panic!("{:?} is not an object", val);
    }
}
