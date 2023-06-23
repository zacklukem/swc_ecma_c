use core::panic;
use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{BufWriter, Write};

use swc_common::errors::DiagnosticBuilder;
use swc_common::sync::Lrc;
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, SourceMap,
};
use swc_common::{SourceFile, Span};
use swc_ecma_ast::{Decl, FnDecl, Function, Module, ModuleItem, Pat, PropName, Stmt};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};

mod expr;
mod stmt;

pub fn main() {
    let cm: Lrc<SourceMap> = Default::default();

    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));

    let file = std::env::args().nth(1).expect("no file given");

    let fm = cm.new_source_file(
        FileName::Custom(file.clone()),
        std::fs::read_to_string(file).unwrap(),
    );

    let lexer = Lexer::new(
        // We want to parse ecmascript
        Syntax::Typescript(Default::default()),
        // EsVersion defaults to es5
        Default::default(),
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
    }

    let module = parser
        .parse_module()
        .map_err(|e| {
            // Unrecoverable fatal error occurred
            e.into_diagnostic(&handler).emit()
        })
        .expect("failed to parser module");

    CodegenContext::gen_module(fm, &mut std::io::stdout(), &module)
        .map_err(|e| e.into_diagnostic(&handler).emit())
        .unwrap();
}

#[derive(Debug)]
pub(crate) struct Closure {
    scopes: Vec<Scope>,
    closure_mappings: HashMap<String, (String, String)>,
}

impl Closure {
    fn resolve(&self, name: &str) -> Option<Cow<'static, str>> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.varmap.get(name) {
                return Some(v.clone());
            }
        }
        None
    }
}

#[derive(Debug)]
pub(crate) struct Scope {
    varmap: HashMap<String, Cow<'static, str>>,
    scope_end: BufWriter<Vec<u8>>,
}

pub struct CodegenContext {
    closure_stack: Vec<Closure>,
    _source_file: Lrc<SourceFile>,
    filename_prefix: String,
    varname_counter: usize,
    annon_fn_counter: usize,
    globals: HashMap<String, Cow<'static, str>>,
}

#[derive(Debug)]
pub(crate) enum CodegenError {
    Io(std::io::Error),
    InvalidIdentifier(Span),
}

impl CodegenError {
    fn into_diagnostic(self, handler: &Handler) -> DiagnosticBuilder {
        match self {
            CodegenError::Io(e) => {
                panic!("io error: {}", e);
            }
            CodegenError::InvalidIdentifier(span) => {
                handler.struct_span_err(span, "Invalid identifier")
            }
        }
    }
}

impl From<std::io::Error> for CodegenError {
    fn from(e: std::io::Error) -> Self {
        CodegenError::Io(e)
    }
}

macro_rules! internal_global {
    ($id: expr) => {
        concat!("swcjs_global_", $id)
    };
}

pub(crate) use internal_global;

macro_rules! internal_func {
    ($id: expr) => {
        concat!("swcjs_", $id)
    };
}

pub(crate) use internal_func;

macro_rules! internal_const {
    ($id: expr) => {
        concat!("SWCJS_", $id)
    };
}

pub(crate) use internal_const;

macro_rules! internal_type_ptr {
    ($id: expr) => {
        concat!("swcjs_", $id, "*")
    };
}

pub(crate) use internal_type_ptr;

pub(crate) const NULL: &str = internal_const!("NULL");
pub(crate) const UNDEFINED: &str = internal_const!("UNDEFINED");

pub(crate) const VALUE_TYPE: &str = internal_type_ptr!("ValueT");
pub(crate) const ARGS_TYPE: &str = internal_type_ptr!("ArgsT");

fn reformat_filename(filename: &FileName) -> String {
    match filename {
        FileName::Custom(s) => s.replace(&['/', '.'], "_"),
        _ => todo!(),
    }
}

macro_rules! scope {
    ($self: expr, None, $block: expr) => {{
        $self.enter_scope();
        $block;
        $self.exit_scope(None::<&mut Vec<u8>>)
    }};
    ($self: expr, $w: expr, $block: expr) => {{
        $self.enter_scope();
        $block;
        $self.exit_scope($w)
    }};
}

pub(crate) use scope;

macro_rules! scope_closure {
    ($self: expr, None, $block: expr) => {{
        $self.enter_closure_scope();
        $self.enter_scope();
        $block;
        $self.exit_scope(None::<&mut Vec<u8>>);
        $self.exit_closure_scope()
    }};
    ($self: expr, $w: expr, $block: expr) => {{
        $self.enter_closure_scope();
        $self.enter_scope();
        $block;
        $self.exit_scope($w);
        $self.exit_closure_scope()
    }};
}

pub(crate) use scope_closure;

pub(crate) struct CodegenBuffers {
    pub header: BufWriter<Vec<u8>>,
    pub fun_impl: BufWriter<Vec<u8>>,
    pub mod_init_fun: BufWriter<Vec<u8>>,
}

impl CodegenContext {
    fn new(source_file: Lrc<SourceFile>) -> Self {
        let filename_prefix = reformat_filename(&source_file.name);
        CodegenContext {
            closure_stack: vec![],
            _source_file: source_file,
            filename_prefix,
            varname_counter: 0,
            annon_fn_counter: 0,
            globals: HashMap::new(),
        }
    }

    fn scope_end(&mut self) -> Option<&mut impl Write> {
        Some(
            &mut self
                .closure_stack
                .last_mut()
                .unwrap()
                .scopes
                .last_mut()
                .unwrap()
                .scope_end,
        )
    }

    fn resolve_nth(&mut self, name: &str, i: isize) -> Option<(bool, Cow<'static, str>)> {
        if i < 0 {
            return self.globals.get(name).map(|v| (true, v.clone()));
        }

        if let Some(v) = self.closure_stack[i as usize].resolve(&name) {
            return Some((false, v));
        } else if let Some((_parent_name, my_var_name)) =
            self.closure_stack[i as usize].closure_mappings.get(name)
        {
            return Some((false, my_var_name.clone().into()));
        } else {
            if let Some((is_global, parent_name)) = self.resolve_nth(name, i - 1) {
                if is_global {
                    return Some((true, parent_name));
                } else {
                    let my_var_name = self.get_var_name(name.as_ref());
                    self.closure_stack[i as usize].closure_mappings.insert(
                        name.to_string(),
                        (parent_name.to_string(), my_var_name.clone().into()),
                    );
                    return Some((false, my_var_name.into()));
                }
            }
        }
        None
    }

    fn resolve(&mut self, name: &str) -> Option<Cow<'static, str>> {
        self.resolve_nth(name, self.closure_stack.len() as isize - 1)
            .map(|(_, v)| v)
    }

    fn get_var_name(&mut self, name: &str) -> String {
        let rename = format!("{}_{}", name, self.varname_counter).into();
        self.varname_counter += 1;
        rename
    }

    fn declare_global(&mut self, arg: String, rename: Cow<'static, str>) {
        self.globals.insert(arg, rename);
    }

    fn declare(&mut self, name: &str, rename: Cow<'static, str>) {
        self.closure_stack
            .last_mut()
            .unwrap()
            .scopes
            .last_mut()
            .unwrap()
            .varmap
            .insert(name.to_string(), rename);
    }

    fn declare_new(&mut self, name: &str) -> Cow<'static, str> {
        if self.closure_stack.is_empty() {
            panic!("no scope");
        }
        let rename: Cow<'static, str> = self.get_var_name(name).into();
        self.declare(name, rename.clone());
        rename
    }

    fn get_anonymous_fn_name(&mut self) -> String {
        let name = format!(
            "__{}_anon_fn_{}",
            self.filename_prefix, self.annon_fn_counter
        );
        self.annon_fn_counter += 1;
        name
    }

    fn enter_closure_scope(&mut self) {
        self.closure_stack.push(Closure {
            scopes: vec![],
            closure_mappings: HashMap::new(),
        })
    }

    fn exit_closure_scope(&mut self) -> Closure {
        self.closure_stack.pop().unwrap()
    }

    fn enter_scope(&mut self) {
        self.closure_stack.last_mut().unwrap().scopes.push(Scope {
            varmap: HashMap::new(),
            scope_end: BufWriter::new(Vec::new()),
        })
    }

    fn exit_scope(&mut self, w: Option<&mut impl Write>) {
        let scope = self.closure_stack.last_mut().unwrap().scopes.pop().unwrap();
        if let Some(w) = w {
            w.write_all(scope.scope_end.into_inner().unwrap().as_slice())
                .unwrap();
        }
    }

    fn gen_function_body(
        &mut self,
        fun_name: &str,
        buffers: &mut CodegenBuffers,
        function: &Function,
    ) -> Result<Vec<String>, CodegenError> {
        let mut fun_buffer = BufWriter::new(Vec::new());

        let mut fun_top = BufWriter::new(Vec::new());
        let mut fun_body = BufWriter::new(Vec::new());

        let scope = scope_closure!(self, None, {
            for (i, param) in function.params.iter().enumerate() {
                if let Pat::Ident(id) = &param.pat {
                    let sym_name = self.declare_new(&id.sym);
                    writeln!(
                        &mut fun_buffer,
                        "{VALUE_TYPE} {sym_name} = {args_nth}(__args__, {i});",
                        args_nth = internal_func!("args_nth"),
                    )?;
                    writeln!(
                        &mut fun_buffer,
                        "{gc_stack_add}(&{sym_name});",
                        gc_stack_add = internal_func!("gc_stack_add"),
                    )?;
                } else {
                    todo!()
                }
            }

            writeln!(
                &mut fun_buffer,
                "{VALUE_TYPE} __this__ = {args_get_this}(__args__);",
                args_get_this = internal_func!("args_get_this")
            )?;

            if let Some(body) = &function.body {
                scope!(self, Some(&mut fun_buffer), {
                    for stmt in &body.stmts {
                        self.gen_stmt(&mut fun_body, buffers, &mut fun_top, stmt)?;
                    }
                    writeln!(&mut fun_buffer, "{{")?;
                    fun_buffer.write_all(fun_top.into_inner().unwrap().as_slice())?;
                    fun_buffer.write_all(fun_body.into_inner().unwrap().as_slice())?;
                });
                writeln!(&mut fun_buffer, "}}")?;
            }
            writeln!(
                &mut fun_buffer,
                "{gc_end_frame}();",
                gc_end_frame = internal_func!("gc_end_frame"),
            )?;
            writeln!(&mut fun_buffer, "return {UNDEFINED};")?;
        });

        let closures = scope.closure_mappings.into_iter().collect::<Vec<_>>();

        writeln!(
            &mut buffers.header,
            "static {VALUE_TYPE} {fun_name}(const {ARGS_TYPE} __args__);"
        )?;

        writeln!(
            &mut buffers.fun_impl,
            "static {VALUE_TYPE} {fun_name}(const {ARGS_TYPE} __args__) {{",
        )?;

        writeln!(
            &mut buffers.fun_impl,
            "{gc_begin_frame}();",
            gc_begin_frame = internal_func!("gc_begin_frame"),
        )?;

        let mut closure_args = Vec::with_capacity(closures.len());

        for (i, (_, (parent_name, sym_name))) in closures.into_iter().enumerate() {
            closure_args.push(parent_name);

            writeln!(
                &mut buffers.fun_impl,
                "{VALUE_TYPE} {sym_name} = {args_closure_nth}(__args__, {i});",
                args_closure_nth = internal_func!("args_closure_nth"),
            )?;
            writeln!(
                &mut buffers.fun_impl,
                "{gc_stack_add}(&{sym_name});",
                gc_stack_add = internal_func!("gc_stack_add"),
            )?;
        }

        buffers
            .fun_impl
            .write_all(fun_buffer.into_inner().unwrap().as_slice())?;

        writeln!(&mut buffers.fun_impl, "}}")?;
        Ok(closure_args)
    }

    fn gen_top_level_function(
        &mut self,
        fun_decl: &FnDecl,
        buffers: &mut CodegenBuffers,
    ) -> Result<(), CodegenError> {
        let fun_name = format!("{}_{}", self.filename_prefix, fun_decl.ident.sym);
        self.declare_global(fun_decl.ident.sym.to_string(), fun_name.clone().into());

        self.gen_function_body(&format!("{fun_name}_impl"), buffers, &fun_decl.function)?;

        writeln!(
            &mut buffers.header,
            "{VALUE_TYPE} {fun_name} = {UNDEFINED};"
        )?;

        writeln!(
            &mut buffers.mod_init_fun,
            "{fun_name} = {init_global_fn}({fun_name}_impl);",
            init_global_fn = internal_func!("init_global_fn"),
        )?;
        writeln!(
            &mut buffers.mod_init_fun,
            "{gc_register_static}(&{fun_name});",
            gc_register_static = internal_func!("gc_register_static"),
        )?;
        Ok(())
    }

    fn init_globals(&mut self) {
        self.declare_global("console".into(), internal_global!("console").into());
        self.declare_global("__swcjs__".into(), internal_global!("__swcjs__").into());
        self.declare_global("Object".into(), internal_global!("Object").into());
        self.declare_global("Number".into(), internal_global!("Number").into());
        self.declare_global("String".into(), internal_global!("String").into());
        self.declare_global("Function".into(), internal_global!("Function").into());
        self.declare_global("Boolean".into(), internal_global!("Boolean").into());
        self.declare_global("assert".into(), internal_global!("assert").into());
    }

    fn gen_module(
        source_file: Lrc<SourceFile>,
        w: &mut impl Write,
        module: &Module,
    ) -> Result<(), CodegenError> {
        let mut this = CodegenContext::new(source_file);
        let mut main_fun_top = BufWriter::new(Vec::new());
        let mut main_fun = BufWriter::new(Vec::new());

        let mut buffers = CodegenBuffers {
            header: BufWriter::new(Vec::new()),
            fun_impl: BufWriter::new(Vec::new()),
            mod_init_fun: BufWriter::new(Vec::new()),
        };

        // GLOBAL SCOPE
        this.init_globals();
        // Main scope
        scope_closure!(this, None, {
            for item in &module.body {
                match item {
                    ModuleItem::Stmt(Stmt::Decl(Decl::Fn(fun_decl))) => {
                        this.gen_top_level_function(fun_decl, &mut buffers)?;
                    }
                    ModuleItem::Stmt(stmt) => {
                        this.gen_stmt(&mut main_fun, &mut buffers, &mut main_fun_top, stmt)?;
                    }
                    _ => {
                        todo!()
                    }
                }
            }
        });

        writeln!(w, "/* generated - do not edit */")?;
        writeln!(w, "#include <swcjs.h>")?;

        w.write_all(buffers.header.into_inner().unwrap().as_slice())?;

        w.write_all(buffers.fun_impl.into_inner().unwrap().as_slice())?;

        writeln!(w, "void {}__swcjs_mod_init__() {{", this.filename_prefix)?;
        writeln!(w, "swcjs_initialize();")?;
        w.write_all(buffers.mod_init_fun.into_inner().unwrap().as_slice())?;
        writeln!(w, "}}")?;

        writeln!(w, "int main() {{")?;
        writeln!(
            w,
            "{gc_begin_frame}();",
            gc_begin_frame = internal_func!("gc_begin_frame"),
        )?;
        writeln!(w, "{}__swcjs_mod_init__();", this.filename_prefix)?;
        writeln!(w, "{{")?;
        w.write_all(main_fun_top.into_inner().unwrap().as_slice())?;
        w.write_all(main_fun.into_inner().unwrap().as_slice())?;
        writeln!(w, "}}")?;
        writeln!(
            w,
            "{gc_end_frame}();",
            gc_end_frame = internal_func!("gc_end_frame"),
        )?;
        writeln!(w, "return 0;")?;
        writeln!(w, "}}")?;

        Ok(())
    }
}

pub(crate) fn prop_name_to_string(prop: &PropName) -> Cow<'_, str> {
    match prop {
        PropName::Ident(id) => Cow::Borrowed(&id.sym),
        PropName::Str(s) => Cow::Borrowed(&s.value),
        PropName::Num(n) => n.value.to_string().into(),
        PropName::BigInt(n) => n.value.to_string().into(),
        PropName::Computed(_) => todo!(),
    }
}
