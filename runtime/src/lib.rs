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
pub mod global_constructors;
pub mod global_functions;

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
    ($as_name:ident, $name: ident, $inner_ty: ty) => {
        #[allow(non_snake_case)]
        pub fn $name(n: $inner_ty) -> Self {
            Self {
                inner: ValueInner::$name(n),
            }
        }

        pub fn $as_name(&self) -> Option<&$inner_ty> {
            if let ValueInner::$name(n) = &self.inner {
                Some(n)
            } else {
                None
            }
        }
    };
}

impl ValueT {
    value_constructor!(as_number, Number, f64);
    value_constructor!(as_string, String, String);
    value_constructor!(as_boolean, Boolean, bool);
    value_constructor!(as_function, Function, Function);
    value_constructor!(as_object, Object, Object);

    pub fn to_log_string(&self) -> String {
        match &self.inner {
            ValueInner::Number(n) => format!("{}", n),
            ValueInner::String(s) => format!("{}", s),
            ValueInner::Boolean(b) => format!("{}", b),
            ValueInner::Function(_) => format!("[Function]"),
            ValueInner::Object(_) => format!("[Object]"),
        }
    }
}

#[derive(Debug)]
pub struct Object {
    pub constructor: *mut ValueT,
    pub properties: HashMap<String, *mut ValueT>,
}

impl Default for Object {
    fn default() -> Self {
        Self {
            constructor: unsafe { global_constructors::swcjs_global_Object },
            properties: HashMap::new(),
        }
    }
}

impl Object {
    pub fn new(constructor: *mut ValueT) -> Self {
        Self {
            constructor,
            properties: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub pointer: extern "C" fn(args: &ArgsT) -> *mut ValueT,
    pub capture: Vec<*mut ValueT>,
    pub this: *mut ValueT,
    pub prototype: *mut ValueT,
    pub properties: HashMap<String, *mut ValueT>,
}

impl Function {
    fn internal(pointer: extern "C" fn(args: &ArgsT) -> *mut ValueT) -> Self {
        Self {
            pointer,
            capture: vec![],
            this: undefined_mut(),
            prototype: alloc(ValueT::Object(Object::default())),
            properties: HashMap::new(),
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

impl<'a, T> Pointer<'a, T> {
    pub fn as_value(self) -> Option<&'a T> {
        match self {
            Pointer::Value(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum PointerMut<'a, T> {
    Null,
    Undefined,
    Value(&'a mut T),
}

impl<'a, T> PointerMut<'a, T> {
    #[allow(unused)]
    pub fn unwrap_value(self) -> &'a mut T {
        match self {
            PointerMut::Value(v) => v,
            _ => panic!("Pointer is not a value"),
        }
    }
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
            let mut object: $crate::Object = Default::default();
            $(
                object.properties.insert($name.to_string(), $value);
            )*
            $crate::alloc(ValueT::Object(object))
        }
    };
}

pub(crate) use swcjs_object;

#[no_mangle]
pub extern "C" fn swcjs_initialize() {
    gc::init();

    global_constructors::init();
    global_functions::init();

    init_console();
    init_swcjs();
}

pub(crate) fn internal_function(fun: extern "C" fn(args: &ArgsT) -> *mut ValueT) -> *mut ValueT {
    alloc(ValueT::Function(Function::internal(fun)))
}

fn init_swcjs() {
    unsafe {
        swcjs_global___swcjs__ = swcjs_object! {
            "gc": internal_function(gc::swcjs_gc),
            "gc_enable_logging": internal_function(gc::swcjs_gc_enable_logging),
            "gc_disable_logging": internal_function(gc::swcjs_gc_disable_logging),
            "gc_store_ptr": internal_function(gc::swcjs_gc_store_ptr),
            "gc_assert_saved": internal_function(gc::swcjs_gc_assert_saved),
            "gc_assert_freed": internal_function(gc::swcjs_gc_assert_freed)
        };
        gc::register_static(NonNull::from(&swcjs_global___swcjs__));
    }
}

fn init_console() {
    let console_log = alloc(ValueT::Function(Function::internal(console_log)));

    unsafe {
        swcjs_global_console = swcjs_object! {
            "log": console_log,
            "assert": global_functions::swcjs_global_assert
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
    con.args.get(n as usize).cloned().unwrap_or(undefined_mut())
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

#[no_mangle]
pub unsafe extern "C" fn swcjs_expr_new(
    fun: *const ValueT,
    argc: u16,
    mut args: ...
) -> *mut ValueT {
    let mut args_list: Vec<*mut ValueT> = vec![];
    for _ in 0..argc {
        args_list.push(args.arg())
    }

    let fun = Pointer::from(fun);

    let new_this = alloc(ValueT::Object(Object::default()));

    if let Pointer::Value(ValueT {
        inner: ValueInner::Function(fun),
        ..
    }) = fun
    {
        (fun.pointer)(&ArgsT {
            args: args_list,
            capture: &fun.capture,
            this: new_this,
        });
        new_this
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
            properties: old_fun.properties.clone(),
            // V8 does this...
            prototype: undefined_mut(),
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
            ValueInner::Function(fun) => match prop.as_str() {
                "bind" => alloc(ValueT::Function(Function {
                    properties: HashMap::new(),
                    prototype: undefined_mut(),
                    pointer: function_bind,
                    capture: vec![val_raw],
                    this: undefined_mut(),
                })),
                "constructor" => unsafe { global_constructors::swcjs_global_Function },
                "prototype" => unsafe { global_constructors::swcjs_global_Function },
                prop => fun.properties.get(prop).cloned().unwrap_or(undefined_mut()),
            },
            ValueInner::Number(_) => match prop.as_str() {
                "constructor" => unsafe { global_constructors::swcjs_global_Number },
                _ => undefined_mut(),
            },
            ValueInner::String(_) => match prop.as_str() {
                "constructor" => unsafe { global_constructors::swcjs_global_String },
                _ => undefined_mut(),
            },
            ValueInner::Boolean(_) => match prop.as_str() {
                "constructor" => unsafe { global_constructors::swcjs_global_Boolean },
                _ => undefined_mut(),
            },
        }
    } else {
        undefined_mut()
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
