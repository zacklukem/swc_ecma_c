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
use swc_ecma_ast::{
    BinaryOp, Callee, Decl, Expr, Lit, MemberProp, Module, ModuleItem, Pat, PatOrExpr, Stmt,
    VarDecl, VarDeclOrExpr,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};

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

    CodegenContext::new(fm)
        .gen_module(&mut std::io::stdout(), &module)
        .map_err(|e| e.into_diagnostic(&handler).emit())
        .unwrap();
}

struct Scope {
    varmap: HashMap<String, Cow<'static, str>>,
    scope_end: BufWriter<Vec<u8>>,
}

struct CodegenContext {
    scope_stack: Vec<Scope>,
    _source_file: Lrc<SourceFile>,
    filename_prefix: String,
    varname_counter: usize,
}

#[derive(Debug)]
enum CodegenError {
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

macro_rules! internal_func {
    ($id: expr) => {
        concat!("swcjs_", $id)
    };
}

macro_rules! internal_const {
    ($id: expr) => {
        concat!("SWCJS_", $id)
    };
}

macro_rules! internal_type_ptr {
    ($id: expr) => {
        concat!("swcjs_", $id, "*")
    };
}

const NULL: &str = internal_const!("NULL");
const UNDEFINED: &str = internal_const!("UNDEFINED");

const VALUE_TYPE: &str = internal_type_ptr!("ValueT");
const ARGS_TYPE: &str = internal_type_ptr!("ArgsT");

fn reformat_filename(filename: &FileName) -> String {
    match filename {
        FileName::Custom(s) => s.replace(&['/', '.'], "_"),
        _ => todo!(),
    }
}

macro_rules! scope {
    ($self: expr, None, $block: expr) => {
        $self.enter_scope();
        $block;
        $self.exit_scope(None::<&mut Vec<u8>>)
    };
    ($self: expr, $w: expr, $block: expr) => {
        $self.enter_scope();
        $block;
        $self.exit_scope($w)
    };
}

impl CodegenContext {
    fn new(source_file: Lrc<SourceFile>) -> Self {
        let filename_prefix = reformat_filename(&source_file.name);
        CodegenContext {
            scope_stack: vec![],
            _source_file: source_file,
            filename_prefix,
            varname_counter: 0,
        }
    }

    fn scope_end(&mut self) -> Option<&mut impl Write> {
        Some(&mut self.scope_stack.last_mut()?.scope_end)
    }

    fn resolve(&self, name: &str) -> Option<Cow<'static, str>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(v) = scope.varmap.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    fn declare(&mut self, name: &str, rename: Cow<'static, str>) {
        self.scope_stack
            .last_mut()
            .unwrap()
            .varmap
            .insert(name.to_string(), rename);
    }

    fn declare_new(&mut self, name: &str) -> Cow<'static, str> {
        if self.scope_stack.is_empty() {
            panic!("no scope");
        }
        if self.scope_stack.len() == 1 {
            let rename: Cow<'static, str> = format!("{}_{}", self.filename_prefix, name).into();
            self.declare(name, rename.clone());
            rename
        } else {
            let rename: Cow<'static, str> = format!("{}_{}", name, self.varname_counter).into();
            self.declare(name, rename.clone());
            self.varname_counter += 1;
            rename
        }
    }

    fn enter_scope(&mut self) {
        self.scope_stack.push(Scope {
            varmap: HashMap::new(),
            scope_end: BufWriter::new(Vec::new()),
        })
    }

    fn exit_scope(&mut self, w: Option<&mut impl Write>) {
        let scope = self.scope_stack.pop();
        if let (Some(scope), Some(w)) = (scope, w) {
            w.write_all(scope.scope_end.into_inner().unwrap().as_slice())
                .unwrap();
        }
    }

    fn gen_module(&mut self, w: &mut impl Write, module: &Module) -> Result<(), CodegenError> {
        let mut header = BufWriter::new(Vec::new());
        let mut fun_impl = BufWriter::new(Vec::new());
        let mut mod_init_fun = BufWriter::new(Vec::new());
        let mut main_fun_top = BufWriter::new(Vec::new());
        let mut main_fun = BufWriter::new(Vec::new());
        scope!(self, None, {
            self.declare("console", internal_global!("console").into());
            self.declare("__swcjs__", internal_global!("__swcjs__").into());

            for item in &module.body {
                match item {
                    ModuleItem::Stmt(Stmt::Decl(Decl::Fn(fun_decl))) => {
                        let fun_name = self.declare_new(&fun_decl.ident.sym);
                        let mut fun_top = BufWriter::new(Vec::new());
                        let mut fun_body = BufWriter::new(Vec::new());
                        writeln!(
                            header,
                            "static {VALUE_TYPE} {fun_name}_impl(const {ARGS_TYPE} __args__);"
                        )?;
                        writeln!(header, "{VALUE_TYPE} {fun_name} = {UNDEFINED};")?;
                        writeln!(
                            fun_impl,
                            "static {VALUE_TYPE} {fun_name}_impl(const {ARGS_TYPE} __args__) {{",
                        )?;
                        writeln!(
                            fun_impl,
                            "{gc_begin_frame}();",
                            gc_begin_frame = internal_func!("gc_begin_frame"),
                        )?;

                        scope!(self, None, {
                            for (i, param) in fun_decl.function.params.iter().enumerate() {
                                if let Pat::Ident(id) = &param.pat {
                                    let sym_name = self.declare_new(&id.sym);
                                    writeln!(
                                        fun_impl,
                                        "{VALUE_TYPE} {sym_name} = {args_nth}(__args__, {i});",
                                        args_nth = internal_func!("args_nth"),
                                    )?;
                                    writeln!(
                                        fun_impl,
                                        "{gc_stack_add}(&{sym_name});",
                                        gc_stack_add = internal_func!("gc_stack_add"),
                                    )?;
                                } else {
                                    todo!()
                                }
                            }

                            if let Some(body) = &fun_decl.function.body {
                                scope!(self, Some(&mut fun_impl), {
                                    for stmt in &body.stmts {
                                        self.gen_stmt(&mut fun_body, &mut fun_top, stmt)?;
                                    }
                                    writeln!(fun_impl, "{{")?;
                                    fun_impl.write_all(fun_top.into_inner().unwrap().as_slice())?;
                                    fun_impl
                                        .write_all(fun_body.into_inner().unwrap().as_slice())?;
                                });
                                writeln!(fun_impl, "}}")?;
                            }
                            writeln!(
                                fun_impl,
                                "{gc_end_frame}();",
                                gc_end_frame = internal_func!("gc_end_frame"),
                            )?;
                            writeln!(fun_impl, "return {UNDEFINED};")?;
                        });
                        writeln!(fun_impl, "}}")?;
                        writeln!(
                            mod_init_fun,
                            "{fun_name} = {init_global_fn}({fun_name}_impl);",
                            init_global_fn = internal_func!("init_global_fn"),
                        )?;
                        writeln!(
                            mod_init_fun,
                            "{gc_register_static}(&{fun_name});",
                            gc_register_static = internal_func!("gc_register_static"),
                        )?;
                    }
                    ModuleItem::Stmt(stmt) => {
                        self.gen_stmt(&mut main_fun, &mut main_fun_top, stmt)?;
                    }
                    _ => {
                        todo!()
                    }
                }
            }
        });

        writeln!(w, "/* generated - do not edit */")?;
        writeln!(w, "#include <swcjs.h>")?;

        w.write_all(header.into_inner().unwrap().as_slice())?;

        w.write_all(fun_impl.into_inner().unwrap().as_slice())?;

        writeln!(w, "void {}__swcjs_mod_init__() {{", self.filename_prefix)?;
        writeln!(w, "swcjs_initialize();")?;
        w.write_all(mod_init_fun.into_inner().unwrap().as_slice())?;
        writeln!(w, "}}")?;

        writeln!(w, "int main() {{")?;
        writeln!(
            w,
            "{gc_begin_frame}();",
            gc_begin_frame = internal_func!("gc_begin_frame"),
        )?;
        writeln!(w, "{}__swcjs_mod_init__();", self.filename_prefix)?;
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

    fn gen_expr(
        &mut self,
        w: &mut impl Write,
        fun_top: &mut impl Write,
        expr: &Expr,
    ) -> Result<(), CodegenError> {
        match expr {
            Expr::Lit(Lit::Str(s)) => write!(w, "{}(\"{}\")", internal_func!("lit_str"), s.value)?,
            Expr::Lit(Lit::Null(_s)) => write!(w, "{NULL}")?,
            Expr::Lit(Lit::Num(n)) => write!(w, "{}({})", internal_func!("lit_num"), n.value)?,
            Expr::Lit(Lit::Bool(b)) => write!(w, "{}({})", internal_func!("lit_bool"), b.value)?,
            Expr::Lit(Lit::BigInt(b)) => {
                write!(w, "{}(\"{}\")", internal_func!("lit_bigint"), b.value)?
            }
            Expr::Lit(Lit::Regex(_r)) => todo!(),
            Expr::Lit(Lit::JSXText(_j)) => todo!(),

            Expr::Ident(id) if &id.sym == "undefined" => {
                write!(w, "{UNDEFINED}")?;
            }
            Expr::Ident(id) => {
                // TODO: handle optional
                // TODO: sanatize idents
                let ident = self
                    .resolve(&id.sym)
                    .ok_or_else(|| CodegenError::InvalidIdentifier(id.span))?;
                write!(w, "{ident}")?
            }
            Expr::Assign(assign) => match &assign.left {
                PatOrExpr::Expr(_) => todo!(),
                PatOrExpr::Pat(p) => match p.as_ref() {
                    Pat::Ident(i) => {
                        write!(w, "(")?;
                        let ident = self
                            .resolve(&i.sym)
                            .ok_or_else(|| CodegenError::InvalidIdentifier(i.span))?;
                        write!(w, "{ident}")?;
                        write!(w, " = ")?;
                        self.gen_expr(w, fun_top, &assign.right)?;
                        write!(w, ")")?;
                    }
                    _ => todo!(),
                },
            },
            Expr::Call(call) => match &call.callee {
                Callee::Expr(expr) => {
                    write!(w, "{}(", internal_func!("expr_call"))?;
                    self.gen_expr(w, fun_top, expr)?;
                    write!(w, ",{}", call.args.len())?;
                    for arg in &call.args {
                        write!(w, ",")?;
                        self.gen_expr(w, fun_top, &arg.expr)?;
                    }
                    write!(w, ")")?;
                }
                _ => todo!(),
            },
            Expr::Bin(bin) => {
                let fun_name = match &bin.op {
                    BinaryOp::EqEq => internal_func!("bin_eqeq"),
                    BinaryOp::NotEq => internal_func!("bin_noteq"),
                    BinaryOp::EqEqEq => internal_func!("bin_eqeqeq"),
                    BinaryOp::NotEqEq => internal_func!("bin_noteqeq"),
                    BinaryOp::Lt => internal_func!("bin_lt"),
                    BinaryOp::LtEq => internal_func!("bin_lteq"),
                    BinaryOp::Gt => internal_func!("bin_gt"),
                    BinaryOp::GtEq => internal_func!("bin_gteq"),
                    BinaryOp::LShift => internal_func!("bin_lshift"),
                    BinaryOp::RShift => internal_func!("bin_rshift"),
                    BinaryOp::ZeroFillRShift => internal_func!("bin_zerofillrshift"),
                    BinaryOp::Add => internal_func!("bin_add"),
                    BinaryOp::Sub => internal_func!("bin_sub"),
                    BinaryOp::Mul => internal_func!("bin_mul"),
                    BinaryOp::Div => internal_func!("bin_div"),
                    BinaryOp::Mod => internal_func!("bin_mod"),
                    BinaryOp::BitOr => internal_func!("bin_bitor"),
                    BinaryOp::BitXor => internal_func!("bin_bitxor"),
                    BinaryOp::BitAnd => internal_func!("bin_bitand"),
                    BinaryOp::LogicalOr => internal_func!("bin_logicalor"),
                    BinaryOp::LogicalAnd => internal_func!("bin_logicaland"),
                    BinaryOp::In => internal_func!("bin_in"),
                    BinaryOp::InstanceOf => internal_func!("bin_instanceof"),
                    BinaryOp::Exp => internal_func!("bin_exp"),
                    BinaryOp::NullishCoalescing => internal_func!("bin_nullishcoalescing"),
                };
                write!(w, "{}(", fun_name)?;
                self.gen_expr(w, fun_top, &bin.left)?;
                write!(w, ",")?;
                self.gen_expr(w, fun_top, &bin.right)?;
                write!(w, ")")?;
            }
            Expr::Member(member) => {
                write!(w, "{}(", internal_func!("expr_member"))?;
                self.gen_expr(w, fun_top, &member.obj)?;
                write!(w, ",")?;
                match &member.prop {
                    MemberProp::Ident(id) => write!(w, "\"{}\"", id.sym)?,
                    _ => todo!(),
                }
                write!(w, ")")?;
            }
            t => todo!("{:?}", t),
        }
        Ok(())
    }

    fn gen_stmt(
        &mut self,
        w: &mut impl Write,
        fun_top: &mut impl Write,
        stmt: &Stmt,
    ) -> Result<(), CodegenError> {
        match stmt {
            Stmt::If(stmt) => {
                write!(w, "if ({}(", internal_func!("if_condition"))?;
                self.gen_expr(w, fun_top, &stmt.test)?;
                write!(w, ")) ")?;
                self.gen_stmt(w, fun_top, &stmt.cons)?;
                if let Some(alt) = &stmt.alt {
                    write!(w, "else\n")?;
                    self.gen_stmt(w, fun_top, alt)?;
                }
            }
            Stmt::While(stmt) => {
                write!(w, "while ({}(", internal_func!("if_condition"))?;
                self.gen_expr(w, fun_top, &stmt.test)?;
                write!(w, ")) ")?;
                self.gen_stmt(w, fun_top, &stmt.body)?;
            }
            Stmt::Decl(Decl::Var(var)) => {
                self.gen_vardecl(w, fun_top, var)?;
            }
            Stmt::Block(stmt) => {
                scope!(self, Some(w), {
                    write!(w, "{{\n")?;
                    for stmt in &stmt.stmts {
                        self.gen_stmt(w, fun_top, stmt)?;
                    }
                });
                write!(w, "}}\n")?;
            }
            Stmt::For(for_stmt) => {
                match &for_stmt.init {
                    Some(VarDeclOrExpr::Expr(e)) => self.gen_expr(w, fun_top, e)?,
                    Some(VarDeclOrExpr::VarDecl(v)) => self.gen_vardecl(w, fun_top, v)?,
                    None => (),
                }
                write!(w, "while ({}(", internal_func!("if_condition"))?;
                if let Some(test) = &for_stmt.test {
                    self.gen_expr(w, fun_top, test)?;
                } else {
                    write!(w, "1")?;
                }
                writeln!(w, ")) {{")?;
                self.gen_stmt(w, fun_top, &for_stmt.body)?;
                if let Some(update) = &for_stmt.update {
                    self.gen_expr(w, fun_top, update)?;
                    writeln!(w, ";")?;
                }
                writeln!(w, "}}")?;

                //
            }

            Stmt::Expr(expr_stmt) => {
                self.gen_expr(w, fun_top, &expr_stmt.expr)?;
                write!(w, ";\n")?;
            }
            Stmt::Return(ret_stmt) => {
                // TODO: save return value to previous stack?
                writeln!(
                    w,
                    "{gc_end_frame}();",
                    gc_end_frame = internal_func!("gc_end_frame"),
                )?;
                write!(w, "return ")?;
                if let Some(expr) = &ret_stmt.arg {
                    self.gen_expr(w, fun_top, expr)?;
                } else {
                    write!(w, "{UNDEFINED}")?;
                }
                writeln!(w, ";")?;
            }
            _ => {
                todo!()
            }
        }
        Ok(())
    }

    fn gen_vardecl(
        &mut self,
        w: &mut impl Write,
        fun_top: &mut impl Write,
        var: &VarDecl,
    ) -> Result<(), CodegenError> {
        Ok(for decl in &var.decls {
            if let Pat::Ident(id) = &decl.name {
                let ident = self.declare_new(&id.sym);
                writeln!(fun_top, "{VALUE_TYPE} {ident} = {UNDEFINED};")?;
                writeln!(self.scope_end().unwrap(), "{ident} = {UNDEFINED};")?;
                writeln!(
                    fun_top,
                    "{gc_stack_add}(&{ident});",
                    gc_stack_add = internal_func!("gc_stack_add"),
                )?;

                if let Some(init) = &decl.init {
                    write!(w, "{} = ", ident)?;
                    self.gen_expr(w, fun_top, init)?;
                }
                writeln!(w, ";")?;
            } else {
                todo!()
            }
        })
    }
}
