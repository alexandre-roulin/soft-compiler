use crate::data::{Program, Statement, Function, Expression, UnOp, BiOp};
use std::borrow::Borrow;

struct Assembly {
	asm: Vec<String>
}

pub struct Generator;

impl Assembly {
	pub fn new() -> Assembly {
		Assembly { asm: vec![] }
	}

	fn push(&mut self, s: String) {
		self.asm.push(s);
	}

	fn push_str(&mut self, s: &str) {
		self.asm.push(s.to_string());
	}

	fn push_asm(&mut self, mut asm: Assembly) {
		self.asm.append(&mut asm.asm);
	}

	fn concatenate(&mut self) -> String {
		self.asm.iter_mut().for_each(|s| s.push('\n'));
		self.asm.join("")
	}
}

impl Generator {
	pub fn new() -> Generator {
		Generator {}
	}
	pub fn generate(&self, program: &Program) -> String {
		let mut asm = Assembly::new();

		asm.push_str("	.intel_syntax noprefix");
		asm.push_str("	.text");
		for function in &program.functions {
			asm.push_asm(self.generate_function(function));
		}
		asm.concatenate()
	}

	fn generate_function(&self, function: &Function) -> Assembly {
		let mut asm = Assembly::new();
		asm.push(format!("	.globl	{}", function.name));
		asm.push(format!("	.type	{}, @function", function.name));
		asm.push(format!("{}:", function.name));
		asm.push_str("	push	rbp");
		asm.push_str("	mov	rbp, rsp");
		for statement in &function.statements {
			asm.push_asm(self.generate_statement(statement));
		}

		asm
	}

	fn generate_statement(&self, statement: &Statement) -> Assembly {
		let mut asm = Assembly::new();

		match statement {
			Statement::Return(expr) => {
				asm.push_asm(self.generate_expression(expr));
				asm.push_str("	pop	rbp"); //Todo a verifier
				asm.push(format!("\tret"));
			}
		}
		asm
	}

	fn generate_expression(&self, expression: &Expression) -> Assembly {
		let mut asm = Assembly::new();
		match expression {
			Expression::Int(nu) => {
				asm.push(format!("\tmov eax, {}", nu));
			}
			Expression::UnaryOperator(un_op, expr) => {
				asm.push_asm(self.generate_expression(expr.borrow()));
				asm.push_asm(self.generate_unary_expression(&un_op));
			}
			Expression::BinaryOperator(e1, op, e2) => {
				asm.push_asm(self.generate_expression(if *op == BiOp::Division || *op == BiOp::Minus { e2.borrow() } else { e1.borrow() }));
				asm.push(format!("\tpush rax"));
				asm.push_asm(self.generate_expression(if *op == BiOp::Division || *op == BiOp::Minus { e1.borrow() } else { e2.borrow() }));
				asm.push(format!("\tpop rcx"));
				asm.push_asm(self.generate_binary_operator(op));
			}
		}

		asm
	}

	fn generate_binary_operator(&self, op: &BiOp) -> Assembly {
		let mut asm = Assembly::new();

		match op {
			BiOp::Addition => { asm.push_str("\tadd rax, rcx"); }
			BiOp::Multiplication => { asm.push_str("\timul rax, rcx"); }
			BiOp::Division => {
				asm.push_str("\txor rdx, rdx");
				asm.push_str("\tidiv rcx");
			}
			BiOp::Minus => { asm.push_str("	sub rax, rcx") }
		}

		asm
	}

	fn generate_unary_expression(&self, operator: &UnOp) -> Assembly {
		let mut asm = Assembly::new();
		match operator {
			UnOp::Negation => {
				asm.push(format!("\tneg eax"));
			}
			UnOp::Bitwise => {
				asm.push(format!("\tnot eax"));
			}
			UnOp::LogicalNegation => {
				asm.push(format!("\tcmp eax, 0"));
				asm.push(format!("\tmov eax, 0"));
				asm.push(format!("\tsete al"));
			}
		}
		asm
	}
}



























