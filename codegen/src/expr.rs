use std::io::Write;

use swc_ecma_ast::{BinaryOp, Callee, Expr, Lit, MemberProp, Pat, PatOrExpr, Prop, PropOrSpread};

use crate::{
    internal_func, prop_name_to_string, CodegenBuffers, CodegenContext, CodegenError, NULL,
    UNDEFINED,
};

impl CodegenContext {
    pub(crate) fn gen_expr(
        &mut self,
        w: &mut impl Write,
        buffers: &mut CodegenBuffers,
        fun_top: &mut impl Write,
        expr: &Expr,
    ) -> Result<(), CodegenError> {
        match expr {
            Expr::This(_) => write!(w, "__this__")?,
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
                        self.gen_expr(w, buffers, fun_top, &assign.right)?;
                        write!(w, ")")?;
                    }
                    Pat::Expr(expr) => match expr.as_ref() {
                        Expr::Member(member) => {
                            write!(
                                w,
                                "{expr_set_member}(",
                                expr_set_member = internal_func!("expr_set_member")
                            )?;
                            self.gen_expr(w, buffers, fun_top, &member.obj)?;
                            write!(w, ",")?;
                            match &member.prop {
                                MemberProp::Ident(id) => write!(w, "\"{}\"", id.sym)?,
                                _ => todo!(),
                            }
                            write!(w, ",")?;
                            self.gen_expr(w, buffers, fun_top, &assign.right)?;
                            write!(w, ")")?;
                        }
                        _ => todo!(),
                    },
                    p => todo!("{:?}", p),
                },
            },
            Expr::Call(call) => match &call.callee {
                Callee::Expr(expr) => {
                    write!(w, "{}(", internal_func!("expr_call"))?;
                    self.gen_expr(w, buffers, fun_top, expr)?;
                    write!(w, ",{}", call.args.len())?;
                    for arg in &call.args {
                        write!(w, ",")?;
                        self.gen_expr(w, buffers, fun_top, &arg.expr)?;
                    }
                    write!(w, ")")?;
                }
                _ => todo!(),
            },
            Expr::New(new) => {
                write!(w, "{}(", internal_func!("expr_new"))?;
                self.gen_expr(w, buffers, fun_top, &new.callee)?;
                write!(w, ",{}", new.args.as_ref().map_or(0, Vec::len))?;
                if let Some(args) = &new.args {
                    for arg in args {
                        write!(w, ",")?;
                        self.gen_expr(w, buffers, fun_top, &arg.expr)?;
                    }
                }
                write!(w, ")")?;
            }
            Expr::Bin(bin) if matches!(bin.op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) => {
                let op = match &bin.op {
                    BinaryOp::LogicalAnd => "&&",
                    BinaryOp::LogicalOr => "||",
                    _ => unreachable!(),
                };
                write!(
                    w,
                    "{lit_bool}({if_condition}(",
                    lit_bool = internal_func!("lit_bool"),
                    if_condition = internal_func!("if_condition")
                )?;
                self.gen_expr(w, buffers, fun_top, &bin.left)?;
                write!(
                    w,
                    ") {op} {if_condition}(",
                    if_condition = internal_func!("if_condition")
                )?;
                self.gen_expr(w, buffers, fun_top, &bin.right)?;
                write!(w, "))")?;
            }
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
                    BinaryOp::In => internal_func!("bin_in"),
                    BinaryOp::InstanceOf => internal_func!("bin_instanceof"),
                    BinaryOp::Exp => internal_func!("bin_exp"),
                    BinaryOp::NullishCoalescing => internal_func!("bin_nullishcoalescing"),
                    // Special case
                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd => unreachable!(),
                };
                write!(w, "{}(", fun_name)?;
                self.gen_expr(w, buffers, fun_top, &bin.left)?;
                write!(w, ",")?;
                self.gen_expr(w, buffers, fun_top, &bin.right)?;
                write!(w, ")")?;
            }
            Expr::Member(member) => {
                write!(w, "{}(", internal_func!("expr_member"))?;
                self.gen_expr(w, buffers, fun_top, &member.obj)?;
                write!(w, ",")?;
                match &member.prop {
                    MemberProp::Ident(id) => write!(w, "\"{}\"", id.sym)?,
                    _ => todo!(),
                }
                write!(w, ")")?;
            }
            Expr::Object(obj) => {
                write!(w, "{}({}", internal_func!("expr_init_obj"), obj.props.len())?;
                for prop in &obj.props {
                    if let PropOrSpread::Prop(prop) = prop {
                        if let Prop::KeyValue(prop) = prop.as_ref() {
                            write!(w, ",\"{}\",", prop_name_to_string(&prop.key))?;
                            self.gen_expr(w, buffers, fun_top, &prop.value)?;
                        } else {
                            todo!();
                        }
                    } else {
                        todo!();
                    }
                }
                write!(w, ")")?;
            }
            Expr::Fn(fn_expr) => {
                // todo: handle ident
                // todo: handle closure

                let fun_name = self.get_anonymous_fn_name();

                let closures = self.gen_function_body(&fun_name, buffers, &fn_expr.function)?;

                write!(
                    w,
                    "{init_anon_fn}({fun_name},{num_closures}",
                    init_anon_fn = internal_func!("init_anon_fn"),
                    num_closures = closures.len()
                )?;
                for closure in closures {
                    write!(w, ",{}", closure)?;
                }
                write!(w, ")")?;
            }
            t => todo!("{:?}", t),
        }
        Ok(())
    }
}
