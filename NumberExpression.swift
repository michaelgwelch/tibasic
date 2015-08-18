//
//  NumberExpression.swift
//  tibasic
//
//  Created by Michael Welch on 8/11/15.
//  Copyright © 2015 Michael Welch. All rights reserved.
//

import Foundation

public indirect enum NumExpression {
    case Id(String)
    case Expr(NumExpression, NumExpression)
    case NumberLiteral(Int)
    case Term(NumExpression, NumExpression)
    case TermTailPlus(NumExpression, NumExpression)
    case TermTailSub(NumExpression, NumExpression)
    case Paren(NumExpression)
    case Factor(NumExpression, NumExpression)
    case FactTailMult(NumExpression, NumExpression)
    case FactTailDiv(NumExpression, NumExpression)
    case Negate(NumExpression)
    case Epsilon
    case Exp(NumExpression, NumExpression)
//    case Negate(NumericExpression)
//    case Multiply(NumericExpression, NumericExpression)
//    case Divide(NumericExpression, NumericExpression)
//    case Add(NumericExpression, NumericExpression)
//    case Subtract(NumericExpression, NumericExpression)

    static let CreateExpr = curry(Expr)
    static let CreateExp = curry(Exp)
    static let CreateMult = curry(FactTailMult)
    static let CreateDiv = curry(FactTailDiv)
    static let CreateTerm = curry(Term)
    static let CreatePlus = curry(TermTailPlus)
    static let CreateSub = curry(TermTailSub)
    static let CreateFact = curry(Factor)
//    static let CreateAdd = curry(Add)
//    static let CreateSub = curry(Subtract)
}

extension NumExpression : CustomStringConvertible {
    public var description:String {
        switch self {
        case .Id(let s): return s
        case .NumberLiteral(let i): return String(i)
        case .Epsilon: return ""
        case .Negate(let n): return "-" + n.description
        case .Factor(let e1, let e2): return e1.description + e2.description
        case .FactTailMult(let e1, let e2): return "*" + e1.description + e2.description
        case .FactTailDiv(let e1, let e2): return "/" + e1.description + e2.description
        case .Term(let e1, let e2): return e1.description + e2.description
        case .TermTailPlus(let e1, let e2): return "+" + e1.description + e2.description
        case .TermTailSub(let e1, let e2): return "-" + e1.description + e2.description
        case .Exp(let e1, let e2): return "^" + e1.description + e2.description
        case .Expr(let e1, let e2): return e1.description + e2.description
        case .Paren(let e): return "(" + e.description + ")"
        }
    }
}

let lparen = char("(")
let rparen = char(")")

let multop = char("*")
let divop = char("/")
let plusop = char("+")
let subop = char("-")
let expop = char("^")
public let number = nat


public let num_expression = NumExpression.CreateExpr <§> term <*> term_tail()

func term_tail() -> Parser<NumExpression> {
    return NumExpression.CreatePlus <§> (plusop *> term) <*> term_tail()
      <|> NumExpression.CreateSub <§> (subop *> term) <*> term_tail()
      <|> epsilon
}

let term = NumExpression.CreateTerm <§> factor <*> factor_tail()

func factor_tail() -> Parser<NumExpression> {
    return NumExpression.CreateMult <§> factor <*> factor_tail()
      <|> NumExpression.CreateDiv <§> factor <*> factor_tail()
      <|> epsilon
}

let factor = NumExpression.CreateFact <§> exp_operand <*> exp_operand_tail()

func exp_operand_tail() -> Parser<NumExpression> {
    return NumExpression.CreateExp <§> (expop *> exp_operand) <*> exp_operand_tail()
      <|> epsilon
}

public let exp_operand:Parser<NumExpression> = (lparen *> num_expression <* rparen)
  <|> NumExpression.NumberLiteral <§> number
  <|> NumExpression.Id <§> identifier
  <|> (plusop *> num_expression)
  <|> NumExpression.Negate <§> num_expression

let epsilon = success(NumExpression.Epsilon)