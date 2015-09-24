//
//  NumberExpression.swift
//  tibasic
//
//  Created by Michael Welch on 8/11/15.
//  Copyright © 2015 Michael Welch. All rights reserved.
//

import SwiftParsing

//:     expr             ::= string string_tail
//:     string_tail      ::= concat_op string string_tail
//:                        | epsilon
//:     string           ::= relation relation_tail
//:     relation_tail    ::= eq_op relation relation_tail
//:                        | neq_op relation relation_tail
//:                        | lte_op relation relation_tail
//:                        | gte_op relation relation_tail
//:                        | lt_op relation relation_tail
//:                        | gt_op relation relation_tail
//:                        | epsilon
//:     relation         ::= term term_tail
//:     term_tail        ::= plus_op term term_tail
//:                        | sub_op term term_tail
//:                        | epsilon
//:     term             ::= factor factor_tail
//:     factor_tail      ::= mult_op factor factor_tail
//:                        | div_op factor factor_tail
//:                        | epsilon
//:     factor           ::= basic basic_tail
//:     basic_tail       ::= exp_op basic basic_tail
//:                        | epsilon
//:     basic            ::= ( expr )
//:                        | id
//:                        | num_literal
//:                        | string_literal
//:                        | sub_op expr
//:                        | plus_op expr
//:     plus_op          ::= +
//:     sub_op           ::= -
//:     mult_op          ::= *
//:     div_op           ::= /
//:     exp_op           ::= ^
//:     concat_op        ::= &
//:     eq_op            ::= =
//:     neq_op           ::= <>
//:     lte_op           ::= <=
//:     gte_op           ::= >=
//:     lt_op            ::= <
//:     gt_op            ::= >
//:     epsilon          ::= 𝜖 (Empty string)


indirect enum Expression {
    case Expr(Expression, Expression)
    case ConcatTail(Expression, Expression)
    case StringExpr(Expression, Expression)
    case Equal(Expression, Expression)
    case NotEqual(Expression, Expression)
    case LessThanEqual(Expression, Expression)
    case GreaterThanEqual(Expression, Expression)
    case LessThan(Expression, Expression)
    case GreaterThan(Expression, Expression)
    case Relational(Expression, Expression)
    case AddTermTail(Expression, Expression)
    case SubtractTermTail(Expression, Expression)
    case Term(Expression, Expression)
    case MultiplyFactorTail(Expression, Expression)
    case DivideFactorTail(Expression, Expression)
    case Factor(Expression, Expression)
    case BasicTail(Expression, Expression)
    case Paren(Expression)
    case Identifier(String)
    case NumberLiteral(NSDecimalNumber)
    case StringLiteral(String)
    case Negate(Expression)
    case Epsilon
}

//: Declare curried functions for creating Expressions as these work best
//: with the applicative operators we'll be using

let createExpression = curry(Expression.Expr)
let createAdd = curry(Expression.AddTermTail)
let createSub = curry(Expression.SubtractTermTail)
let createTerm = curry(Expression.Term)
let createMult = curry(Expression.MultiplyFactorTail)
let createDiv = curry(Expression.DivideFactorTail)
let createFactor = curry(Expression.Factor)
let createBasic = curry(Expression.BasicTail)

func expr_func() -> ParserOf<Expression> {
    return createExpression <§> string_expr <*> string_tail
}
let expr = Parser.lazy(expr_func())


//: Now just start defining parsers for each line of the grammar starting at the bottom

let concat = { (a:Character,b:Character) -> String in String(a) + String(b) }
let epsilon = Parser.success(Expression.Epsilon)
let exp_op = Parser.string("^").token()
let div_op = Parser.string("/").token()
let mult_op = Parser.string("*").token()
let sub_op = Parser.string("-").token()
let plus_op = Parser.string("+").token()
let lparen = Parser.string("(").token()
let rparen = Parser.string(")").token()
let concat_op = Parser.string("&").token()
let eq_op = Parser.string("=").token()
let lt_op = Parser.string("<").token()
let gt_op = Parser.string(">").token()
let neq_op = Parser.sequence(Parser.char("<"), Parser.char(">"), concat).token()
let lte_op = Parser.sequence(Parser.char("<"), Parser.char("="), concat).token()
let gte_op = Parser.sequence(Parser.char(">"), Parser.char("="), concat).token()

// MARK: Strings
func string_tail_func() -> ParserOf<Expression> {
    return parse_tail(concat_op, string_expr, string_tail, Expression.StringExpr)
}
let string_tail = Parser.lazy(string_tail_func())
let string_expr = Parser.sequence(relational, relational_tail, Expression.StringExpr)

func parse_tail<P:ParserType where P.TokenType==Expression>(op:ParserOf<String>, _ e1:ParserOf<Expression>, _ e2:P, _ ctor:(Expression, Expression) -> Expression) -> ParserOf<Expression> {
    return Parser.sequence(op, e1, e2, {(_,x1,x2) in ctor(x1,x2)})
}

// MARK: Relations
func parse_rel_tail(op:ParserOf<String>, _ ctor:(Expression,Expression) -> Expression) -> ParserOf<Expression> {
    return parse_tail(op, relational, relational_tail, ctor)
}
func relational_tail_func() -> ParserOf<Expression> {
    return parse_rel_tail(eq_op, Expression.Equal)
    <|> parse_rel_tail(neq_op, Expression.NotEqual)
    <|> parse_rel_tail(lte_op, Expression.LessThanEqual)
    <|> parse_rel_tail(gte_op, Expression.GreaterThanEqual)
    <|> parse_rel_tail(lt_op, Expression.LessThan)
    <|> parse_rel_tail(gt_op, Expression.GreaterThan)
    <|> epsilon

}
let relational_tail = Parser.lazy(relational_tail_func())
let relational = Parser.sequence(term, term_tail, Expression.Relational)

// MARK: Terms
func term_tail_func() -> ParserOf<Expression> {
    return createAdd <§> (plus_op *> term) <*> Parser.lazy(term_tail)
        <|> createSub <§> (sub_op *> term) <*> Parser.lazy(term_tail)
        <|> epsilon
}
let term_tail = term_tail_func()
let term = createTerm <§> factor <*> factor_tail

// MARK: Factors
func factor_tail_func() -> ParserOf<Expression> {
    return createMult <§> (mult_op *> factor) <*> factor_tail
        <|> createDiv <§> (div_op *> factor) <*> factor_tail
        <|> epsilon
}
let factor_tail = Parser.lazy(factor_tail_func())
let factor = createFactor <§> basic <*> basic_tail

// MARK: Basic Expressions
func basic_tail_func() -> ParserOf<Expression> {
    return parse_tail(exp_op, basic, basic_tail, Expression.BasicTail)
}
let basic_tail = Parser.lazy(basic_tail_func())
let basic = paren_expr <|> identifier <|> number_literal <|> string_literal <|> unaryNegate <|> unaryPlus


let paren_expr = Parser.sequence(lparen, expr, rparen, { (_,e,_) in e })
//let identifier = Parser.identifier.map(Expression.Identifier)
let identifier_first_char = Parser.letter <|> Parser.char("@")
    <|> Parser.char("[") <|> Parser.char("]") <|> Parser.char("_") <|> Parser.char("\\")
let identifier_char = Parser.alphanum <|> Parser.char("@") <|> Parser.char("_")
let identifier_string = Parser.sequence(identifier_first_char, identifier_char.repeatMany()) { (c,s) in
    String(c) + String(s)
}
let identifier = Parser.sequence(identifier_string, Parser.string("$").optional("")) { (s1,s2) in
    Expression.Identifier(s1+s2)
}


//let num_literal = Parser.natural.map(Expression.NumberLiteral)

let digitsOptional = Parser.digit.repeatMany().map {String($0)}
let digits = Parser.digit.repeatOneOrMore().map {String($0)}
let fractional = Parser.sequence(Parser.string("."), digits.optional("0"), {$0 + $1}).optional("")
let exponent_char = Parser.string("E")
let exponent_sign = Parser.string("-").optional("")
let exponent = Parser.sequence(exponent_char, exponent_sign, digits) { (c,s,d) in c + s + d }
let significand = Parser.sequence(digits, fractional) {$0+$1}
let number_literal = Parser.sequence(significand, exponent) { (s,e) in
    Expression.NumberLiteral(NSDecimalNumber(string: s+e))
}


let string_literal_contents = ((Parser.string("\"\"") <|> (Parser.satisfy { $0 != "\"" }).map {String($0)})).repeatMany().join()
let string_literal = Parser.sequence(Parser.string("\""), string_literal_contents, Parser.string("\"")) {
    (_,s,_) in Expression.StringLiteral(s)
}.token()

//let string_literal = {x in { y in { z in (z,y,z) } }} <$> Parser.char("\"")
// Parser.char("\""), (Parser.string("\"\"") <|> Parser.sat {$0 != "\""}).repeatMany(), Parser.char("\"")
let unaryNegate = Parser.sequence(sub_op, expr, {(_,e) in Expression.Negate(e)})
let unaryPlus = Parser.sequence(plus_op, expr, {(_,e) in e})



//
//extension Expression {
//    static func eval(expr1:Expression, inExpression expr2:Expression, withStore store:[String:Int]) -> Int? {
//        let e1 = expr1.eval(withMemory:store)
//        return expr2.eval(withLeftHandSide:e1, andMemory:store)
//    }
//
//    func eval(withMemory store:[String:Int]) -> Int? {
//
//        switch self {
//        case .Expression(let expr1, let expr2):
//            return Expression.eval(expr1, inExpression: expr2, withStore:store)
//
//        case .Term(let expr1, let expr2):
//            return Expression.eval(expr1, inExpression: expr2, withStore:store)
//
//        case .Factor(let expr1, let expr2):
//            return Expression.eval(expr1, inExpression: expr2, withStore:store)
//
//        case .Paren(let e):
//            return e.eval(withMemory: store)
//
//        case .Identifier(let id):
//            return store[id]
//
//        case .Literal(let num):
//            return num
//
//        case .Negate(let expr):
//            return { -$0 } <§> expr.eval(withMemory: store)
//
//
//        case .AddTermTail(_), .SubtractTermTail(_), .MultiplyFactorTail(_), .DivideFactorTail(_),
//        .BasicTail(_), .Epsilon:
//            return nil
//
//        }
//    }
//
//    func eval(withLeftHandSide accumulator:Int?, andMemory store:[String:Int]) -> Int? {
//
//        switch self {
//
//        case .Expression(_), .Term(_), .Factor(_), .Paren(_), .Identifier, .Literal, .Negate:
//            return nil
//
//        case .AddTermTail(let expr1, let expr2):
//            let sum:Int -> Int -> Int = { x in { y in x + y } }
//            return sum <§> accumulator <*> Expression.eval(expr1, inExpression: expr2, withStore: store)
//
//        case .SubtractTermTail(let expr1, let expr2):
//            let diff:Int -> Int -> Int = { x in { y in x - y } }
//            return diff <§> accumulator <*> Expression.eval(expr1, inExpression: expr2, withStore: store)
//
//        case .MultiplyFactorTail(let expr1, let expr2):
//            let product:Int -> Int -> Int = { x in { y in x * y } }
//            return product <§> accumulator <*> Expression.eval(expr1, inExpression: expr2, withStore: store)
//
//        case .DivideFactorTail(let expr1, let expr2):
//            let quotient:Int -> Int -> Int = { x in { y in x / y } }
//            return quotient <§> accumulator <*> Expression.eval(expr1, inExpression: expr2, withStore: store)
//
//        case .BasicTail(let expr1, let expr2):
//            let exp:Int -> Int -> Int = { x in { y in Int(pow(Double(x), Double(y))) } }
//            return exp <§> accumulator <*> Expression.eval(expr1, inExpression: expr2, withStore: store)
//
//        case .Epsilon:
//            return accumulator
//
//
//        }
//    }
//
//    var graph : String {
//        switch self {
//        case .Expression(let e1, let e2): return "Expression(\(e1.graph), \(e2.graph))"
//        case .AddTermTail(let e1, let e2): return "AddTermTail(\(e1.graph), \(e2.graph))"
//        case .SubtractTermTail(let e1, let e2): return "SubtractTermTail(\(e1.graph), \(e2.graph))"
//        case .Term(let e1, let e2): return "Term(\(e1.graph), \(e2.graph))"
//        case .MultiplyFactorTail(let e1, let e2): return "MultiplyFactorTail(\(e1.graph), \(e2.graph))"
//        case .DivideFactorTail(let e1, let e2): return "DivideFactorTail(\(e1.graph), \(e2.graph))"
//        case .Factor(let e1, let e2): return "Factor(\(e1.graph), \(e2.graph))"
//        case .BasicTail(let e1, let e2): return "ExponentOperandTail(\(e1.graph), \(e2.graph))"
//        case .Paren(let e): return "Paren(\(e.graph))"
//        case .Identifier(let s): return "\"\(s)\""
//        case .Literal(let n): return String(n)
//        case .Negate(let n): return "Negate(\(n))"
//        case .Epsilon: return "𝜖"
//
//        }
//    }
//
//
//    var description : String {
//        switch self {
//        case .Expression(let e1, let e2): return e1.description + e2.description
//        case .AddTermTail(let e1, let e2): return "+" + e1.description + e2.description
//        case .SubtractTermTail(let e1, let e2): return "-" + e1.description + e2.description
//        case .Term(let e1, let e2): return e1.description + e2.description
//        case .MultiplyFactorTail(let e1, let e2): return "*" + e1.description + e2.description
//        case .DivideFactorTail(let e1, let e2): return "/" + e1.description + e2.description
//        case .Factor(let e1, let e2): return e1.description + e2.description
//        case .BasicTail(let e1, let e2): return "^" + e1.description + e2.description
//        case .Paren(let e): return "(\(e.description))"
//        case .Identifier(let s): return s
//        case .Literal(let n): return n.description
//        case .Negate(let e): return "-" + e.description
//        case .Epsilon: return ""
//        }
//    }
//}



