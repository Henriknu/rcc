use core::panic;
use std::fmt::Write;

use crate::{
    ast::{AssignmentStmt, Expr, IfStmt, Literal, Stmt},
    parser::Program,
};

const REGISTER_LIST: [&str; 4] = ["%r8", "%r9", "%r10", "%r11"];
const BYTE_REGISTER_LIST: [&str; 4] = ["%r8b", "%r9b", "%r10b", "%r11b"];

type Reg = usize;
type Label = usize;

pub struct CodeGen<'s> {
    program: &'s Program<'s>,
    free_registers: [bool; 4],
    out: String,
    label: Label,
}

impl<'s> CodeGen<'s> {
    pub fn new(program: &'s Program) -> Self {
        Self {
            program,
            free_registers: Default::default(),
            out: String::new(),
            label: 0,
        }
    }

    pub fn gen(&mut self) -> String {
        for symbol in self.program.symbol_table.iter() {
            self.gen_glob_sym(symbol)
        }

        self.preamble();

        for stmt in &self.program.stmts {
            self.gen_stmt(stmt);
        }

        self.postabmle();

        self.out.clone()
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl(_) => {
                // VarDecl only revolves around creating the global symbol, currently done in parser.
            }
            Stmt::ExprStmt(_) => todo!(),
            Stmt::PrintStmt(print_stmt) => {
                let value = self.gen_expr(&print_stmt.expr);
                self.print_value(value);
                self.free_reg(value);
            }
            Stmt::AssignmentStmt(AssignmentStmt { name, expr }) => {
                let value = self.gen_expr(expr);
                self.store_glob(value, name.literal(self.program.source));
                self.free_reg(value);
            }
            Stmt::IfStmt(if_stmt) => self.if_stmt(if_stmt),
            Stmt::BlockStmt(block_stmt) => {
                for sub_stmt in &block_stmt.body {
                    self.gen_stmt(sub_stmt);
                }
            }
        }
    }

    fn if_stmt(&mut self, stmt: &IfStmt) {
        let else_label = self.inc_label();
        let after_label = self.inc_label();
        let else_format = self.format_label(else_label);
        let after_format = self.format_label(after_label);

        // TODO: Support anything other than comparisons in if stmts.

        let Expr::BinaryExpression(binary_expr) = &stmt.condition else {
            todo!("Truthy values in if statements not supported :)");
        };

        if !binary_expr.op.is_comparison() {
            todo!("Currently only allow comparisons in if statements :)");
        }

        let left = self.gen_expr(&binary_expr.left);
        let right = self.gen_expr(&binary_expr.right);

        // cmpq right, left
        writeln!(
            &mut self.out,
            "\tcmpq\t{}, {}",
            REGISTER_LIST[right], REGISTER_LIST[left]
        )
        .unwrap();

        // Do correct jump instr, based on op. Is gonna be reverse comparison.
        let jump_instr = match binary_expr.op {
            crate::ast::BinaryOp::Equals => "jne",
            crate::ast::BinaryOp::NotEquals => "je",
            crate::ast::BinaryOp::GreaterOrEquals => "jl",
            crate::ast::BinaryOp::Greater => "jle",
            crate::ast::BinaryOp::LessOrEquals => "jg",
            crate::ast::BinaryOp::Less => "jge",
            _ => unreachable!(),
        };

        // Jump over if block if condition was false.
        // <jump_instr> <else_label>
        writeln!(&mut self.out, "\t{jump_instr}\t{}", else_format).unwrap();

        self.gen_stmt(&stmt.body);

        // After if, jump to after the else.
        // jmp <after_label>
        writeln!(&mut self.out, "\tjmp\t{}", after_format).unwrap();

        self.write_label(else_label);

        if let Some(else_) = &stmt.else_ {
            self.gen_stmt(else_);
        }

        self.write_label(after_label);
    }

    fn gen_expr(&mut self, expr: &Expr) -> Reg {
        match expr {
            Expr::BinaryExpression(binary) => {
                let left = self.gen_expr(&binary.left);
                let right = self.gen_expr(&binary.right);

                // handlers free left register, right is dst.
                match binary.op {
                    crate::ast::BinaryOp::Add => self.add(left, right),
                    crate::ast::BinaryOp::Sub => self.sub(left, right),
                    crate::ast::BinaryOp::Mul => self.mul(left, right),
                    crate::ast::BinaryOp::Div => self.div(left, right),
                    crate::ast::BinaryOp::Equals => self.equals(left, right),
                    crate::ast::BinaryOp::NotEquals => self.not_equals(left, right),
                    crate::ast::BinaryOp::GreaterOrEquals => self.greater_or_equals(left, right),
                    crate::ast::BinaryOp::Greater => self.greater(left, right),
                    crate::ast::BinaryOp::LessOrEquals => self.less_or_equals(left, right),
                    crate::ast::BinaryOp::Less => self.less(left, right),
                }
            }
            Expr::Literal(lit) => {
                let Literal::Int(value) = lit;
                self.load_int(*value)
            }
            Expr::Var(var) => self.load_glob(var.name.literal(self.program.source)),
            Expr::Call(_) => todo!(),
        }
    }

    fn equals(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        self._compare(reg1, reg2, "sete")
    }

    fn not_equals(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        self._compare(reg1, reg2, "setne")
    }

    fn greater(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        self._compare(reg1, reg2, "setg")
    }

    fn less(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        self._compare(reg1, reg2, "setl")
    }

    fn greater_or_equals(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        self._compare(reg1, reg2, "setge")
    }

    fn less_or_equals(&mut self, reg1: Reg, reg2: Reg) -> Reg {
        self._compare(reg1, reg2, "setle")
    }

    fn _compare(&mut self, reg1: Reg, reg2: Reg, set_instr: &str) -> Reg {
        // cmpq reg2, reg1
        writeln!(
            &mut self.out,
            "\tcmpq\t{}, {}",
            REGISTER_LIST[reg2], REGISTER_LIST[reg1]
        )
        .unwrap();

        // <set_instr> breg2
        writeln!(
            &mut self.out,
            "\t{}\t{}",
            set_instr, BYTE_REGISTER_LIST[reg2]
        )
        .unwrap();

        // movzbq reg2, breg2
        // NB: Zeroes out everything above the lower byte.
        writeln!(
            &mut self.out,
            "\tmovzbq\t{},{}",
            BYTE_REGISTER_LIST[reg2], REGISTER_LIST[reg2]
        )
        .unwrap();

        self.free_reg(reg1);

        reg2
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

    fn load_glob(&mut self, name: &str) -> Reg {
        let reg = self.alloc_reg();

        self.out
            .push_str(&format!("\tmovq\t{}(%rip), {}\n", name, REGISTER_LIST[reg]));

        reg
    }

    fn store_glob(&mut self, reg: Reg, name: &str) -> Reg {
        self.out
            .push_str(&format!("\tmovq\t{}, {}(%rip)\n", REGISTER_LIST[reg], name));

        reg
    }

    /// Declares a global symbol, using the .comm directive.
    ///
    /// .comm name, size, alignment
    fn gen_glob_sym(&mut self, symbol: &str) {
        self.out.push_str(&format!("\t.comm\t{},8,8\n", symbol));
    }

    fn load_int(&mut self, value: isize) -> Reg {
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

    fn format_label(&self, label: Label) -> String {
        format!("L{label}")
    }

    fn write_label(&mut self, label: Label) {
        let label = self.format_label(label);

        writeln!(&mut self.out, "{label}:").unwrap()
    }

    fn inc_label(&mut self) -> Label {
        let label = self.label;

        self.label += 1;

        label
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
