#![crate_type = "staticlib"]
#![feature(c_variadic)]
use gc::register_static;
use libc::c_char;
use std::{
    collections::HashMap,
    ptr::{null, null_mut},
};

pub mod binop;
pub mod gc;

pub struct ArgsT {
    pub args: Vec<*mut ValueT>,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
enum GcColor {
    White,
    Gray,
    Black,
}

#[derive(Debug)]
pub struct ValueT {
    gc_state: GcColor,
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
                gc_state: GcColor::White,
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

    pub(crate) fn is_white(&self) -> bool {
        self.gc_state == GcColor::White
    }

    pub(crate) fn is_gray(&self) -> bool {
        self.gc_state == GcColor::Gray
    }

    pub(crate) fn is_black(&self) -> bool {
        self.gc_state == GcColor::Black
    }

    pub(crate) fn set_white(&mut self) {
        self.gc_state = GcColor::White;
    }

    pub(crate) fn set_gray(&mut self) {
        self.gc_state = GcColor::Gray;
    }

    pub(crate) fn set_black(&mut self) {
        self.gc_state = GcColor::Black;
    }
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
pub static mut swcjs_global_console: *mut ValueT = undefined_mut();

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
        swcjs_global_console = alloc(ValueT::Object(console_object));
        gc::register_static(swcjs_global_console);
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

    if let Pointer::Value(ValueT {
        inner: ValueInner::Function(fun),
        ..
    }) = fun
    {
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

    if let PointerMut::Value(ValueT {
        inner: ValueInner::Object(obj),
        ..
    }) = val
    {
        obj.properties
            .get(&prop)
            .cloned()
            .unwrap_or(undefined_mut())
    } else {
        panic!("{:?} is not an object", val);
    }
}
