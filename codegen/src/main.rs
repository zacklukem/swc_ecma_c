use std::io::{BufWriter, Write};

use swc_common::sync::Lrc;
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, SourceMap,
};
use swc_ecma_ast::{
    BinaryOp, Callee, Decl, Expr, Lit, MemberProp, Module, ModuleItem, Pat, PatOrExpr, Stmt,
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

    CodegenContext {}
        .gen_module(&mut std::io::stdout(), &module)
        .unwrap();
}

struct CodegenContext {}

#[derive(Debug)]
enum CodegenError {
    Io(std::io::Error),
}

impl From<std::io::Error> for CodegenError {
    fn from(e: std::io::Error) -> Self {
        CodegenError::Io(e)
    }
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

impl CodegenContext {
    fn gen_module(&mut self, w: &mut impl Write, module: &Module) -> Result<(), CodegenError> {
        let mut header = BufWriter::new(Vec::new());
        let mut fun_impl = BufWriter::new(Vec::new());
        let mut mod_init_fun = BufWriter::new(Vec::new());

        let mut main_fun = BufWriter::new(Vec::new());

        for item in &module.body {
            match item {
                ModuleItem::Stmt(Stmt::Decl(Decl::Fn(fun_decl))) => {
                    writeln!(
                        header,
                        "{VALUE_TYPE} {}_impl({ARGS_TYPE} __args__);",
                        fun_decl.ident.sym
                    )?;
                    writeln!(header, "{VALUE_TYPE} {} = {UNDEFINED};", fun_decl.ident.sym)?;
                    writeln!(
                        fun_impl,
                        "{VALUE_TYPE} {}_impl({ARGS_TYPE} __args__) {{",
                        fun_decl.ident.sym
                    )?;
                    if let Some(body) = &fun_decl.function.body {
                        writeln!(fun_impl, "{{")?;
                        for stmt in &body.stmts {
                            self.gen_stmt(&mut fun_impl, stmt)?;
                        }
                        writeln!(fun_impl, "}}")?;
                    }
                    writeln!(fun_impl, "return {UNDEFINED};")?;
                    writeln!(fun_impl, "}}")?;
                    writeln!(
                        mod_init_fun,
                        "{fun_name} = {init_global_fn}({fun_name}_impl);",
                        fun_name = fun_decl.ident.sym,
                        init_global_fn = internal_func!("init_global_fn"),
                    )?;
                }
                ModuleItem::Stmt(stmt) => {
                    self.gen_stmt(&mut main_fun, stmt)?;
                }
                _ => {
                    todo!()
                }
            }
        }

        writeln!(w, "/* generated - do not edit */")?;
        writeln!(w, "#include <swcjs.h>")?;

        w.write_all(header.into_inner().unwrap().as_slice())?;

        w.write_all(fun_impl.into_inner().unwrap().as_slice())?;

        writeln!(w, "void mod_init() {{")?;
        writeln!(w, "swcjs_initialize();")?;
        w.write_all(mod_init_fun.into_inner().unwrap().as_slice())?;
        writeln!(w, "}}")?;

        writeln!(w, "int main() {{")?;
        writeln!(w, "mod_init();")?;
        writeln!(w, "{{")?;
        w.write_all(main_fun.into_inner().unwrap().as_slice())?;
        writeln!(w, "}}")?;
        writeln!(w, "return 0;")?;
        writeln!(w, "}}")?;

        Ok(())
    }

    fn gen_expr(&mut self, w: &mut impl Write, expr: &Expr) -> Result<(), CodegenError> {
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
                write!(w, "{}", id.sym)?
            }
            Expr::Assign(assign) => {
                write!(w, "(")?;
                match &assign.left {
                    PatOrExpr::Expr(_) => todo!(),
                    PatOrExpr::Pat(p) => match p.as_ref() {
                        Pat::Ident(i) => write!(w, "{}", i.sym)?,
                        _ => todo!(),
                    },
                }
                write!(w, " = ")?;
                self.gen_expr(w, &assign.right)?;
                write!(w, ")")?;
            }
            Expr::Call(call) => match &call.callee {
                Callee::Expr(expr) => {
                    write!(w, "{}(", internal_func!("expr_call"))?;
                    self.gen_expr(w, expr)?;
                    write!(w, ",{}", call.args.len())?;
                    for arg in &call.args {
                        write!(w, ",")?;
                        self.gen_expr(w, &arg.expr)?;
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
                self.gen_expr(w, &bin.left)?;
                write!(w, ",")?;
                self.gen_expr(w, &bin.right)?;
                write!(w, ")")?;
            }
            Expr::Member(member) => {
                write!(w, "{}(", internal_func!("expr_member"))?;
                self.gen_expr(w, &member.obj)?;
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

    fn gen_stmt(&mut self, w: &mut impl Write, stmt: &Stmt) -> Result<(), CodegenError> {
        match stmt {
            Stmt::If(stmt) => {
                write!(w, "if ({}(", internal_func!("if_condition"))?;
                self.gen_expr(w, &stmt.test)?;
                write!(w, ")) ")?;
                self.gen_stmt(w, &stmt.cons)?;
                if let Some(alt) = &stmt.alt {
                    write!(w, "else\n")?;
                    self.gen_stmt(w, alt)?;
                }
            }
            Stmt::While(stmt) => {
                write!(w, "while ({}(", internal_func!("if_condition"))?;
                self.gen_expr(w, &stmt.test)?;
                write!(w, ")) ")?;
                self.gen_stmt(w, &stmt.body)?;
            }
            Stmt::Decl(Decl::Var(var)) => {
                // TODO: ident sanitization
                // TODO: handle kind
                // TODO: handle declare
                for decl in &var.decls {
                    if let Pat::Ident(id) = &decl.name {
                        write!(w, "{VALUE_TYPE} {} = ", id.sym)?;
                        if let Some(init) = &decl.init {
                            self.gen_expr(w, init)?;
                        } else {
                            write!(w, "{UNDEFINED}")?;
                        }
                        write!(w, ";\n")?;
                    } else {
                        todo!()
                    }
                }
            }
            Stmt::Block(stmt) => {
                write!(w, "{{\n")?;
                for stmt in &stmt.stmts {
                    self.gen_stmt(w, stmt)?;
                }
                write!(w, "}}\n")?;
            }

            Stmt::Expr(expr_stmt) => {
                self.gen_expr(w, &expr_stmt.expr)?;
                write!(w, ";\n")?;
            }
            _ => {
                todo!()
            }
        }
        Ok(())
    }
}
