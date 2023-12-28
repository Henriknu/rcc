use core::panic;
use std::fmt::Write;

use crate::ast::{Expr, Literal, Stmt};

const REGISTER_LIST: [&str; 4] = ["%r8", "%r9", "%r10", "%r11"];

type Reg = usize;

pub struct CodeGen {
    free_registers: [bool; 4],
    out: String,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn gen(&mut self, stmts: &[Stmt]) -> String {
        self.preamble();

        for stmt in stmts {
            self.gen_stmt(stmt);
        }

        self.postabmle();

        self.out.clone()
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::LocalDecl(_) => todo!(),
            Stmt::ExprStmt(_) => todo!(),
            Stmt::PrintStmt(print_stmt) => {
                let value = self.gen_expr(&print_stmt.expr);
                self.print_value(value);
            }
        }
    }

    fn gen_expr(&mut self, expr: &Expr) -> Reg {
        match expr {
            Expr::BinaryExpression(binary) => {
                let left = self.gen_expr(&binary.left);
                let right = self.gen_expr(&binary.right);

                match binary.op {
                    crate::ast::BinaryOp::Add => self.add(left, right),
                    crate::ast::BinaryOp::Sub => self.sub(left, right),
                    crate::ast::BinaryOp::Mul => self.mul(left, right),
                    crate::ast::BinaryOp::Div => self.div(left, right),
                }
            }
            Expr::Literal(lit) => {
                let Literal::Int(value) = lit;
                self.load(*value)
            }
            Expr::Var(_) => todo!(),
            Expr::Call(_) => todo!(),
        }
    }

    fn add(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        writeln!(
            &mut self.out,
            "\taddq\t{}, {}",
            REGISTER_LIST[reg1], REGISTER_LIST[reg2]
        )
        .unwrap();

        self.free_reg(reg1);

        reg2
    }

    fn sub(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        writeln!(
            &mut self.out,
            "\tsubq\t{}, {}",
            REGISTER_LIST[reg2], REGISTER_LIST[reg1]
        )
        .unwrap();

        self.free_reg(reg2);

        reg1
    }

    fn mul(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        writeln!(
            &mut self.out,
            "\timulq\t{}, {}",
            REGISTER_LIST[reg1], REGISTER_LIST[reg2]
        )
        .unwrap();

        self.free_reg(reg1);

        reg2
    }

    fn div(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        writeln!(&mut self.out, "\tmovq\t{}, %rax", REGISTER_LIST[reg1]).unwrap(); // Move dividend to rax
        writeln!(&mut self.out, "\tcqo").unwrap(); // Extend to 8 bytes using cqo
        writeln!(&mut self.out, "\tidivq\t{}", REGISTER_LIST[reg2]).unwrap(); // Do actual division
        writeln!(&mut self.out, "\tmovq\t%rax, {}", REGISTER_LIST[reg1]).unwrap(); // Move result (quotient) into reg2

        self.free_reg(reg2);

        reg1
    }

    fn load(&mut self, value: isize) -> Reg {
        let reg = self.alloc_reg();

        self.out
            .push_str(&format!("\tmovq\t${}, {}\n", value, REGISTER_LIST[reg]));

        reg
    }

    fn print_value(&mut self, reg: Reg) {
        writeln!(self.out, "\tmovq\t{}, %rdi", REGISTER_LIST[reg]).unwrap();
        writeln!(self.out, "\tcall\tprintint").unwrap();
    }

    fn preamble(&mut self) {
        self.free_all_regs();

        self.out.push_str("\t.text\n");
        self.out.push_str(".LC0:\n");
        self.out.push_str("\t.string\t\"%d\\n\"\n");
        self.out.push_str("printint:\n");
        self.out.push_str("\tpushq\t%rbp\n");
        self.out.push_str("\tmovq\t%rsp, %rbp\n");
        self.out.push_str("\tsubq\t$16, %rsp\n");
        self.out.push_str("\tmovl\t%edi, -4(%rbp)\n");
        self.out.push_str("\tmovl\t-4(%rbp), %eax\n");
        self.out.push_str("\tmovl\t%eax, %esi\n");
        self.out.push_str("\tleaq	.LC0(%rip), %rdi\n");
        self.out.push_str("\tmovl	$0, %eax\n");
        self.out.push_str("\tcall	printf@PLT\n");
        self.out.push_str("\tnop\n");
        self.out.push_str("\tleave\n");
        self.out.push_str("\tret\n\n");
        self.out.push_str("\t.globl\tmain\n");
        self.out.push_str("\t.type\tmain, @function\n");
        self.out.push_str("main:\n");
        self.out.push_str("\tpushq\t%rbp\n");
        self.out.push_str("\tmovq	%rsp, %rbp\n\n");
    }

    fn postabmle(&mut self) {
        self.out.push_str("\n\tmovl	$0, %eax\n");
        self.out.push_str("\tpopq	%rbp\n");
        self.out.push_str("\tret\n");
    }

    fn alloc_reg(&mut self) -> usize {
        for (i, reg) in self.free_registers.iter_mut().enumerate() {
            if *reg {
                *reg = false;
                return i;
            }
        }

        panic!("Could not allocate register")
    }

    fn free_all_regs(&mut self) {
        for reg in self.free_registers.iter_mut() {
            *reg = true;
        }
    }

    fn free_reg(&mut self, reg: usize) {
        if self.free_registers[reg] {
            panic!("Try to free not taken register")
        }

        self.free_registers[reg] = true;
    }
}

impl Default for CodeGen {
    fn default() -> Self {
        Self {
            free_registers: [true; 4],
            out: String::new(),
        }
    }
}
