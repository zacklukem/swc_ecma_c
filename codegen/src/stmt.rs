use std::io::Write;

use swc_ecma_ast::{Decl, Pat, Stmt, VarDecl, VarDeclOrExpr};

use crate::{
    internal_func, scope, CodegenBuffers, CodegenContext, CodegenError, UNDEFINED, VALUE_TYPE,
};

impl CodegenContext {
    pub(crate) fn gen_stmt(
        &mut self,
        w: &mut impl Write,
        buffers: &mut CodegenBuffers,
        fun_top: &mut impl Write,
        stmt: &Stmt,
    ) -> Result<(), CodegenError> {
        match stmt {
            Stmt::If(stmt) => {
                write!(w, "if ({}(", internal_func!("if_condition"))?;
                self.gen_expr(w, buffers, fun_top, &stmt.test)?;
                write!(w, ")) ")?;
                self.gen_stmt(w, buffers, fun_top, &stmt.cons)?;
                if let Some(alt) = &stmt.alt {
                    write!(w, "else\n")?;
                    self.gen_stmt(w, buffers, fun_top, alt)?;
                }
            }
            Stmt::While(stmt) => {
                write!(w, "while ({}(", internal_func!("if_condition"))?;
                self.gen_expr(w, buffers, fun_top, &stmt.test)?;
                write!(w, ")) ")?;
                self.gen_stmt(w, buffers, fun_top, &stmt.body)?;
            }
            Stmt::Decl(Decl::Var(var)) => {
                self.gen_vardecl(w, buffers, fun_top, var)?;
            }
            Stmt::Block(stmt) => {
                scope!(self, Some(w), {
                    write!(w, "{{\n")?;
                    for stmt in &stmt.stmts {
                        self.gen_stmt(w, buffers, fun_top, stmt)?;
                    }
                });
                self.gen_gc_run(w)?;
                write!(w, "}}\n")?;
            }
            Stmt::For(for_stmt) => {
                match &for_stmt.init {
                    Some(VarDeclOrExpr::Expr(e)) => self.gen_expr(w, buffers, fun_top, e)?,
                    Some(VarDeclOrExpr::VarDecl(v)) => self.gen_vardecl(w, buffers, fun_top, v)?,
                    None => (),
                }
                write!(w, "while ({}(", internal_func!("if_condition"))?;
                if let Some(test) = &for_stmt.test {
                    self.gen_expr(w, buffers, fun_top, test)?;
                } else {
                    write!(w, "1")?;
                }
                writeln!(w, ")) {{")?;
                self.gen_stmt(w, buffers, fun_top, &for_stmt.body)?;
                if let Some(update) = &for_stmt.update {
                    self.gen_expr(w, buffers, fun_top, update)?;
                    writeln!(w, ";")?;
                }
                writeln!(w, "}}")?;

                //
            }

            Stmt::Expr(expr_stmt) => {
                self.gen_expr(w, buffers, fun_top, &expr_stmt.expr)?;
                write!(w, ";\n")?;
            }
            Stmt::Return(ret_stmt) => {
                // TODO: save return value to previous stack for gc
                writeln!(
                    w,
                    "{gc_end_frame}();",
                    gc_end_frame = internal_func!("gc_end_frame"),
                )?;
                write!(w, "return ")?;
                if let Some(expr) = &ret_stmt.arg {
                    self.gen_expr(w, buffers, fun_top, expr)?;
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
        buffers: &mut CodegenBuffers,
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
                    self.gen_expr(w, buffers, fun_top, init)?;
                }
                writeln!(w, ";")?;
            } else {
                todo!()
            }
        })
    }
}
