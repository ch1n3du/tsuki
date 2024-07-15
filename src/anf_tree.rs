// TODO: Learn how to implement
enum Expr {
    Int(i32),
    Identifier(String),
    Plus(Box<Self>, Box<Self>),
    Minus(Box<Self>, Box<Self>),
    Let {
        name: String,
        value: Box<Self>,
        then: Box<Self>,
    },
}

fn regular_expr_to_anf(expr: Expr) -> ANFExpr {
    match expr {
        Expr::Int(int) => todo!(),
        Expr::Identifier(ident) => todo!(),
        Expr::Plus(left, right) => todo!(),
        Expr::Minus(left, right) => todo!(),
        Expr::Let { name, value, then } => todo!(),
    }
}

/// Atomic Expressions
enum ImmediateExpr {
    Int(i32),
    Identifier(String),
}

/// Non-atomic Expressions
enum CompoundExpr {
    ImmediateExpr(ImmediateExpr),
    Plus(ImmediateExpr, ImmediateExpr),
}

enum ANFExpr {
    Let {
        name: String,
        value: CompoundExpr,
        then: Box<Self>,
    },
    CompoundExpr(CompoundExpr),
}

/*
(5 +  4) - 2

ANFExpr::Let {
    name: "x",
    value: CompoundExpr::ImmExpr(ImmediateExpr::Int(5))
    then: ANFExpr::CompoundExpr(
    )
}

Minus(Plus(Int(5), Int(4)), Int(2))

---------------------=>

let v = 5 + 4 in
v-2

ANFExpr::Let {
    name: "v",
    value: ComplexExpr::Plus(
             ImmediateExpr::Int(5),
             ImmediateExpr::Int(4)
           ),
    then: ComplexExpr::Minus(
            ImmediateExpr::Identifier("v"),
            ImmediateExpr::Int(2)
          )
}

===============================================================

fn fib(n) -> Int {
    if n < 2 {
        1
    } else {
        fib(n-1) + fib(n-2)
    }
}

fn fib(n) -> Int {
    cond_0 = n < 2
    let if_0 = if (n<2) 1 else (fib(n-1) + fib)
}

*/
