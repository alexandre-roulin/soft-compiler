use crate::data::{Program, Statement, Expression, Function};

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
        Generator {  }
    }
    pub fn generate(&self, program: &Program) -> String {
        let mut asm = Assembly::new();

        for function in &program.functions {
            asm.push_asm(self.generate_function(function));
        }
        asm.concatenate()
    }

    fn generate_function(&self, function: &Function) -> Assembly {
        let mut asm = Assembly::new();
        asm.push(format!("\t.globl {}", function.name));
        asm.push(format!("{}:", function.name));
        for statement in &function.statements {
            asm.push_asm(self.generate_statement(statement));
        }
        asm
    }

    fn generate_statement(&self, statement: &Statement) -> Assembly {
        let mut asm = Assembly::new();

        match statement {
            Statement::Return(Expression::Int(nu)) => {
                asm.push(format!("\tmovl    ${}, %eax", nu));
                asm.push(format!("\tret"));
            }
            _ => {}
        }
        asm
    }
}



























