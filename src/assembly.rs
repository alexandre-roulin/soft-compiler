use crate::data::{
    BiOp, Compound, Declare, Expression, Function, Program, Statement, UnOp, Variable,
};
use std::borrow::Borrow;
use std::collections::HashMap;

struct Assembly {
    asm: Vec<String>,
}

pub struct Generator {
    count: u32,
    scope_manager: ScopeManager,
}

struct ScopeManager {
    scopes: Vec<Scope>,
    offset: i32,
}

impl ScopeManager {
    fn new() -> ScopeManager {
        ScopeManager {
            scopes: vec![Scope::new()],
            offset: 0,
        }
    }

    fn new_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn drop(&mut self) {
        self.scopes.pop();
        self.offset = if let Some(scope) = self.scopes.last() {
            if let Some((_, offset)) = scope.map.iter().max() {
                *offset
            } else {
                0
            }
        } else {
            0
        }
    }

    fn add_variable(&mut self, variable: &Variable) {
        if let Some(scope) = self.scopes.last_mut() {
            let size_offset: i32 = variable.size.into();

            self.offset -= size_offset;
            scope.add_variable(variable.name.clone(), self.offset);
        } else {
            panic!("no scope");
        }
    }

    fn get_offset(&self, variable: &String) -> &i32 {
        let reverse_iterator = self.scopes.iter().rev();
        for item in reverse_iterator {
            if let Ok(offset) = item.get_offset(variable) {
                return offset;
            }
        }
        panic!("no scope");
    }

    fn len(&self) -> usize {
        let mut len = 0;
        self.scopes.iter().for_each(|s| len += s.len());
        len
    }
}

#[derive(Debug)]
struct Scope {
    map: HashMap<String, i32>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            map: Default::default(),
        }
    }

    fn add_variable(&mut self, name: String, offset: i32) {
        if self.map.contains_key(&name) {
            panic!("2 variable with same name : {}", name)
        }
        self.map.insert(name, offset);
    }

    fn get_offset(&self, variable: &String) -> Result<&i32, ()> {
        if let Some(offset) = self.map.get(variable) {
            Ok(offset)
        } else {
            Err(())
        }
    }

    fn len(&self) -> usize {
        self.map.len()
    }
}

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
        Generator {
            count: 0,
            scope_manager: ScopeManager::new(),
        }
    }

    pub fn create_label(&mut self, label: &str) -> String {
        self.count += 1;
        format!("_{}{}", label, self.count)
    }
}

impl Generator {
    pub fn generate(&mut self, program: &Program) -> String {
        let mut asm = Assembly::new();

        asm.push_str("	.intel_syntax noprefix");
        asm.push_str("	.text");
        for function in &program.functions {
            asm.push_asm(self.generate_function(function));
        }
        asm.concatenate()
    }

    fn generate_function(&mut self, function: &Function) -> Assembly {
        let mut asm = Assembly::new();
        asm.push(format!("	.globl	{}", function.name));
        asm.push(format!("	.type	{}, @function", function.name));
        asm.push(format!("{}:", function.name));
        asm.push_str("	push	rbp");
        asm.push_str("	mov	rbp, rsp");
        asm.push(format!(
            "\tsub rsp, {}",
            self.calculate_stack_size(&function.compounds) + 8
        ));

        self.scope_manager.new_scope();
        for compound in &function.compounds {
            asm.push_asm(self.generate_compound(compound));
            asm.push_str("");
        }

        self.scope_manager.drop();
        asm
    }

    fn calculate_stack_size(&self, compounds: &Vec<Compound>) -> i32 {
        let mut stack_size = 0;
        for compound in compounds {
            match compound {
                Compound::Declare(_) => {
                    stack_size += 8;
                }
                Compound::Statement(Statement::ForDecl(_, _, _, vec)) => {
                    if let Statement::Compound(v) = vec.borrow() {
                        stack_size += self.calculate_stack_size(v);
                    }
                    stack_size += 8;
                }
                Compound::Statement(Statement::For(_, _, _, vec)) => {
                    if let Statement::Compound(v) = vec.borrow() {
                        stack_size += self.calculate_stack_size(v);
                    }
                }
                Compound::Statement(Statement::For(_, _, _, vec)) => {
                    if let Statement::Compound(v) = vec.borrow() {
                        stack_size += self.calculate_stack_size(v);
                    }
                }
                Compound::Statement(Statement::Do(vec, _)) => {
                    stack_size += self.calculate_stack_size(vec);
                }
                Compound::Statement(Statement::If(_, vec, else_)) => {
                    if let Statement::Compound(v) = vec.borrow() {
                        stack_size += self.calculate_stack_size(v);
                    }
                    if let Some(else_) = else_ {
                        if let Statement::Compound(v) = vec.borrow() {
                            stack_size += self.calculate_stack_size(v);
                        }
                    }
                }
                _ => {}
            }
        }
        stack_size
    }

    fn generate_compound(&mut self, compound: &Compound) -> Assembly {
        let mut asm = Assembly::new();
        match compound {
            Compound::Statement(statement) => {
                asm.push_asm(self.generate_statement(statement));
                asm.push_str("");
            }
            Compound::Declare(declare) => {
                asm.push_asm(self.generate_declaration(declare));
            }
        }
        asm
    }

    fn generate_declaration(&mut self, declaration: &Declare) -> Assembly {
        let mut asm = Assembly::new();
        if let Declare::Declare(variable, expression) = declaration {
            self.scope_manager.add_variable(variable);
            let from;
            if let Some(expression) = expression {
                asm.push_asm(self.generate_expression(expression));
                from = "eax";
            } else {
                from = "0";
            }
            asm.push(format!(
                "\tmov DWORD PTR {}[rbp], {}",
                self.scope_manager.get_offset(&variable.name),
                from
            ))
        }
        asm
    }

    fn generate_statement(&mut self, statement: &Statement) -> Assembly {
        let mut asm = Assembly::new();

        match statement {
            Statement::Return(expr) => {
                asm.push_asm(self.generate_expression(expr));
                asm.push_str("\tleave");
                asm.push_str("\tret");
            }
            Statement::Expression(expression) => {
                if let Some(expression) = expression {
                    asm.push_asm(self.generate_expression(expression));
                }
            }
            Statement::If(cond, body, else_statement) => {
                let post_conditional = self.create_label("post_conditional");
                let else_conditional = self.create_label("else_conditional");

                asm.push_asm(self.generate_expression(cond));
                asm.push_str("\tcmp eax, 0");
                asm.push(format!(
                    "\tje {}",
                    if else_statement.is_some() {
                        &else_conditional
                    } else {
                        &post_conditional
                    }
                ));

                asm.push_asm(self.generate_statement(body));
                asm.push(format!("\tjmp {}", post_conditional));

                if let Some(else_statement) = else_statement {
                    asm.push(format!("{}:", else_conditional));
                    asm.push_asm(self.generate_statement(else_statement));
                    asm.push(format!("\tjmp {}", post_conditional));
                }
                asm.push(format!("{}:", post_conditional));
            }
            Statement::Compound(c) => {
                self.scope_manager.new_scope();

                for compound in c {
                    asm.push_asm(self.generate_compound(compound));
                }
                self.scope_manager.drop();
            }
            Statement::For(initial, condition, post_expression, body) => {
                let loop_ = self.create_label("loop");
                let cond = self.create_label("condition");

                if let Some(initial) = initial {
                    asm.push_asm(self.generate_expression(initial));
                }
                asm.push(format!("\tjmp {}", cond));
                asm.push(format!("{}:", loop_));
                asm.push_asm(self.generate_statement(body));

                if let Some(post_expression) = post_expression {
                    asm.push_asm(self.generate_expression(post_expression));
                }

                asm.push(format!("{}:", cond));

                asm.push_asm(self.generate_expression(condition));
                asm.push_str("\tcmp eax, 0");
                asm.push(format!("\tjne {}", loop_));
            }
            Statement::ForDecl(declare, condition, post_expression, body) => {
                let loop_ = self.create_label("loop");
                let cond = self.create_label("condition");

                self.scope_manager.new_scope();
                asm.push_asm(self.generate_declaration(declare));

                asm.push(format!("\tjmp {}", cond));
                asm.push(format!("{}:", loop_));
                asm.push_asm(self.generate_statement(body));

                if let Some(post_expression) = post_expression {
                    asm.push_asm(self.generate_expression(post_expression));
                }

                asm.push(format!("{}:", cond));

                asm.push_asm(self.generate_expression(condition));
                asm.push_str("\tcmp eax, 0");
                asm.push(format!("\tjne {}", loop_));
                self.scope_manager.drop();
            }
            Statement::While(condition, body) => {
                let loop_ = self.create_label("loop");
                let cond = self.create_label("condition");

                asm.push(format!("\tjmp {}", cond));
                asm.push(format!("{}:", loop_));
                asm.push_asm(self.generate_statement(body));

                asm.push(format!("{}:", cond));

                asm.push_asm(self.generate_expression(condition));
                asm.push_str("\tcmp eax, 0");
                asm.push(format!("\tjne {}", loop_));
            }
            Statement::Do(body, condition) => {
                let loop_ = self.create_label("loop");
                let cond = self.create_label("condition");

                asm.push(format!("\tjmp {}", cond));
                asm.push(format!("{}:", loop_));
                for compound in body {
                    asm.push_asm(self.generate_compound(compound));
                }

                asm.push(format!("{}:", cond));

                asm.push_asm(self.generate_expression(condition));
                asm.push_str("\tcmp eax, 0");
                asm.push(format!("\tjne {}", loop_));
            }
            Statement::Break => {

            }
            Statement::Continue => {

            }
        }
        asm
    }

    fn generate_expression(&mut self, expression: &Expression) -> Assembly {
        let mut asm = Assembly::new();
        //println!("{:?}", expression);
        match expression {
            Expression::Int(nu) => {
                asm.push(format!("\tmov eax, {}", nu));
            }
            Expression::Variable(v) => {
                asm.push(format!(
                    "\tmov eax, DWORD PTR {}[rbp]",
                    self.scope_manager.get_offset(v)
                ));
            }
            Expression::UnaryOperator(un_op, expr) => {
                asm.push_asm(self.generate_expression(expr.borrow()));
                asm.push_asm(self.generate_unary_expression(&un_op));
            }
            Expression::BinaryOperator(e1, _op @ BiOp::LogicalOr, e2) => {
                let clause = self.create_label("clause");
                let end = self.create_label("end");

                //First expression
                asm.push_asm(self.generate_expression(e1.borrow()));
                asm.push_str("\ttest eax, eax");
                asm.push(format!("\tje {}", clause));
                asm.push_str("\tmov eax, 1");
                asm.push(format!("\tjmp {}", end));

                //Second expression
                asm.push(format!("{}:", clause));
                asm.push_asm(self.generate_expression(e2.borrow()));
                asm.push_str("\ttest eax, eax");
                asm.push_str("\tmov eax, 0");
                asm.push_str("\tsetne al");
                asm.push(format!("{}:", end));
            }
            Expression::BinaryOperator(e1, _op @ BiOp::LogicalAnd, e2) => {
                let clause = self.create_label("clause");
                let end = self.create_label("end");

                //First expression
                asm.push_asm(self.generate_expression(e1.borrow()));
                asm.push_str("\ttest eax, eax");
                asm.push(format!("\tjne {}", clause));
                asm.push(format!("\tjmp {}", end));

                //Second expression
                asm.push(format!("{}:", clause));
                asm.push_asm(self.generate_expression(e2.borrow()));
                asm.push_str("\ttest eax, eax");
                asm.push_str("\tmov eax, 0");
                asm.push_str("\tsetne al");
                asm.push(format!("{}:", end));
            }
            Expression::BinaryOperator(e1, op @ BiOp::Division, e2)
            | Expression::BinaryOperator(e1, op @ BiOp::Minus, e2)
            | Expression::BinaryOperator(e1, op @ BiOp::Modulus, e2) => {
                asm.push_asm(self.generate_expression(e2.borrow()));
                asm.push(format!("\tpush rax"));
                asm.push_asm(self.generate_expression(e1.borrow()));
                asm.push(format!("\tpop rcx"));
                asm.push_asm(self.generate_binary_operator(op));
            }
            Expression::BinaryOperator(e1, _op @ BiOp::Assign, e2) => {
                asm.push_asm(self.generate_expression(e2.borrow()));
                if let Expression::Variable(v) = e1.borrow() {
                    asm.push(format!(
                        "\tmov  DWORD PTR {}[rbp], eax",
                        self.scope_manager.get_offset(v)
                    ));
                } else {
                    panic!("")
                }
            }
            Expression::BinaryOperator(e1, op, e2) => {
                asm.push_asm(self.generate_expression(e1.borrow()));
                asm.push(format!("\tpush rax"));
                asm.push_asm(self.generate_expression(e2.borrow()));
                asm.push(format!("\tpop rcx"));
                asm.push_asm(self.generate_binary_operator(op));
            }
            Expression::Assign(variable, expression) => {
                asm.push_asm(self.generate_expression(expression));
                asm.push(format!(
                    "\tmov DWORD PTR {}[rbp], eax",
                    self.scope_manager.get_offset(variable)
                ));
            }
            Expression::CondExp(condition, body, else_) => {
                let post_conditional = self.create_label("post_conditional");
                let else_conditional = self.create_label("else_conditional");

                asm.push_asm(self.generate_expression(condition));
                asm.push_str("\tcmp eax, 0");
                asm.push(format!("je {}", else_conditional));
                asm.push_asm(self.generate_expression(body));
                asm.push(format!("jmp {}", post_conditional));

                asm.push(format!("{}:", else_conditional));
                asm.push_asm(self.generate_expression(else_));
                asm.push(format!("jmp {}", post_conditional));
                asm.push(format!("{}:", post_conditional));
            }
            _ => {
                println!("NOT EVALUATED >>> {:?}", expression);
            }
        }

        asm
    }

    fn generate_binary_operator(&self, op: &BiOp) -> Assembly {
        let mut asm = Assembly::new();

        match op {
            BiOp::Addition => {
                asm.push_str("\tadd rax, rcx");
            }
            BiOp::Multiplication => {
                asm.push_str("\timul rax, rcx");
            }
            BiOp::Division => {
                asm.push_str("\tmov rdx, 0");
                asm.push_str("\tidiv rcx");
            }
            BiOp::Minus => asm.push_str("\tsub rax, rcx"),
            BiOp::Equal => {
                asm.push_str("\tcmp ecx, eax");
                asm.push_str("\tmov eax, 0");
                asm.push_str("\tsete al");
            }
            BiOp::NotEqual => {
                asm.push_str("\tcmp ecx, eax");
                asm.push_str("\tmov eax, 0");
                asm.push_str("\tsetne al");
            }
            BiOp::LessThan => {
                asm.push_str("\tcmp ecx, eax");
                asm.push_str("\tmov eax, 0");
                asm.push_str("\tsetl al");
            }
            BiOp::LessOrEqual => {
                asm.push_str("\tcmp ecx, eax");
                asm.push_str("\tmov eax, 0");
                asm.push_str("\tsetle al");
            }
            BiOp::GreaterThan => {
                asm.push_str("\tcmp ecx, eax");
                asm.push_str("\tmov eax, 0");
                asm.push_str("\tsetg al");
            }
            BiOp::GreaterOrEqual => {
                asm.push_str("\tcmp ecx, eax");
                asm.push_str("\tmov eax, 0");
                asm.push_str("\tsetge al");
            }
            //			BiOp::Assign => {}
            BiOp::BitwiseAND => {
                asm.push_str("\tand eax, rcx");
            }
            BiOp::BitwiseOR => {
                asm.push_str("\tor eax, rcx");
            }
            BiOp::BitwiseXOR => {
                asm.push_str("\txor eax, rcx");
            }
            BiOp::Assign => {
                asm.push_str("\tmov rcx, rax");
            }

            BiOp::BitwiseShiftLeft => {}
            BiOp::BitwiseShiftRight => {}
            BiOp::Modulus => {
                asm.push_str("\tmov rdx, 0");
                asm.push_str("\tidiv rcx");
                asm.push_str("\tmov rax, rdx")
            }
            _ => {
                //                println!("Here ???");
            }
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
