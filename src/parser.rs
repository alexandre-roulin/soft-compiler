use bnf::*;
use std::fmt;
use std::slice::Iter;
use crate::tokenizer::Token;
use std::rc::Rc;

//        let right_hand_terms: Vec<&Term> = self.grammar
//            .productions_iter()
//            .flat_map(|prod| prod.rhs_iter())
//            .flat_map(|expr| expr.terms_iter())
//            .collect();
type IterToken<'a> = Iter<'a, Token>;

pub struct Parser<'a> {
    iter: IterToken<'a>,
    prod: ProdutionRule,
    grammar: Grammar,
    cur_token: Option<&'a Token>,
}

pub enum ProdutionRule {
    Program,
    Function,
    Statement,
    Exp,
}

impl Parser<'_> {
    pub fn new(iter: IterToken) -> Result<Parser, Error> {
        Ok(Parser {
            prod: ProdutionRule::Program,
            grammar: self::Parser::bnf()?,
            iter: iter.clone(),
            cur_token: None,
        })
    }

    pub fn parse(&mut self) -> bool {
        self.cur_token = self.iter.next();
        self.parse_non_terminal(Term::Nonterminal("function".to_string()));
        self.iter.next().is_none()
    }

    fn parse_non_terminal(&mut self, init_term: Term) {
        let vec_expr = self.get_rhs(&init_term);
        let mut b = false;
        for expr in vec_expr {
//            println!("Find Expr {:?}", expr);
            for term in expr.terms_iter() {
//                println!("Find Term {:?}", term);
                let e = match term {
                    Term::Terminal(_) => b = self.parse_terminal(term.clone()),
                    Term::Nonterminal(_) => {
                        self.parse_non_terminal(term.clone());
                    }
                };
            }
            if b { break; } else { return;}
        }
    }

    fn parse_terminal(&mut self, term: Term) -> bool {
        if let Term::Terminal(s) = term {
//            println!("Current Token : {:?}", self.iter);
            if s == self.cur_token.unwrap().to_string() {
                self.cur_token = self.iter.next();
//                println!("__!__ Match Token && Term {:?}\n", self.cur_token);
                return true;
            }
        }
        false
    }
    fn get_rhs(&mut self, term: &Term) -> Vec<Expression> {
        self.grammar
            .productions_iter_mut()
            .filter(|p| &p.lhs == term)
            .flat_map(|prod| prod.rhs_iter())
            .cloned()
            .collect()
    }
    fn bnf() -> Result<Grammar, Error> {
        let input = "
    <program> ::= <function>
    <function> ::= \"int\" \"identifier\" \"OpenParentheses\" \"CloseParentheses\" \"OpenBrace\" <statement> \"CloseBrace\"
    <statement> ::= \"return\" <exp> \"Semicolon\"
    <exp> ::= \"Constant\" | \"identifier\"";
        input.parse()
//    let grammar: Result<Grammar, _> = ;
//    match grammar {
//        Ok(g) => {
//            println!("grammar {:#?}", g);
//            for p in g.productions_iter() {
//                println!("p.iter {:?}", p.lhs);
//            }
//
//            let left_hand_terms: Vec<&Term> = g
//                .productions_iter()
//                .map(|ref prod| &prod.lhs)
//                .collect();
//            println!("left_hand_terms{:#?}", left_hand_terms);
//            let right_hand_terms: Vec<&Term> = g
//                .productions_iter()
//                .flat_map(|prod| prod.rhs_iter())
//                .flat_map(|expr| expr.terms_iter())
//                .collect();
//            println!("right_hand_terms{:#?}", right_hand_terms);
//        }
//        Err(e) => println!("Failed to make grammar from String: {}", e),collect
    }
}


