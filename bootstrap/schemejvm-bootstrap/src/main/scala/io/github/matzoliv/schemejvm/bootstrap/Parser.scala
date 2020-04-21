package io.github.matzoliv.schemejvm.bootstrap

import io.github.matzoliv.schemejvm.runtime.Symbol

import scala.annotation.tailrec

sealed trait Node {
  def sExp: Object
}

case class Literal(value: Object) extends Node {
  def sExp = value
}

case class Program(body: List[Node]) extends Node {
  def sExp = S.Cons(S.Symbol("begin"), S.List(body.map(_.sExp):_*))
}

case class Define(name: Symbol, value: Node) extends Node {
  def sExp = S.List(S.Symbol("define"), name, value.sExp)
}

case class DefineLambda(name: Symbol, lambda: Lambda) extends Node {
  def sExp = S.Cons(S.Symbol("define"), S.Cons(S.List.createWithTail(name::lambda.args, lambda.restArg.getOrElse(null)), S.List(lambda.body.map(_.sExp):_*)))
}

case class DefineMacro(name: Symbol, lambda: Lambda) extends Node {
  def sExp = S.Cons(S.Symbol("define-macro"), S.Cons(S.List.createWithTail(name::lambda.args, lambda.restArg.getOrElse(null)), S.List(lambda.body.map(_.sExp):_*)))
}

case class Lambda(args: List[Symbol], restArg: Option[Symbol], body: List[Node], freeVariables: Option[Set[String]] = None) extends Node {
  def sExp = S.Cons(S.Symbol("lambda"), S.Cons(S.List.createWithTail(args, restArg.getOrElse(null)), S.List(body.map(_.sExp):_*)))
}

case class SetBang(name: String, value: Node) extends Node {
  def sExp = S.List(S.Symbol("set!"), S.Symbol(name), value.sExp)
}

case class If(condition: Node, trueBranch: Node, falseBranch: Option[Node]) extends Node {
  def sExp = S.List(S.Symbol("if")::condition.sExp::trueBranch.sExp::falseBranch.toList.map(_.sExp):_*)
}

case class Quasiquote(value: Object) extends Node {
  def sExp = S.List(S.Symbol("quasiquote"), value)
}

case class Quote(value: Object) extends Node {
  def sExp = S.List(S.Symbol("quote"), value)
}

case class VariableLookup(name: String) extends Node {
  def sExp = S.Symbol(name)
}

case class FunctionApplication(procedure: Node, args: List[Node]) extends Node {
  def sExp = S.List(procedure.sExp::args.map(_.sExp):_*)
}

case class TypeTaggedNode(className: String, node: Node) {
  def sExp = S.List(className, node.sExp)
}

case class NewObject(className: String, args: List[TypeTaggedNode]) extends Node {
  def sExp = S.Cons(S.Symbol("new"), S.Cons(className, S.List(args.map(_.sExp):_*)))
}

case class MethodCall(target: TypeTaggedNode, methodName: String, args: List[TypeTaggedNode]) extends Node {
  def sExp = S.Cons(S.Symbol("call"), S.Cons(target.sExp, S.Cons(S.Symbol(methodName), S.List(args.map(_.sExp):_*))))
}

case class FieldAccess(target: TypeTaggedNode, fieldName: String) extends Node {
  def sExp = S.List(S.Symbol("field"), target.sExp, S.Symbol(fieldName))
}

case object Null extends Node {
  def sExp = null
}

class InvalidSyntax(expression: Object)
  extends Exception(s"Syntax is invalid: $expression")

object SymbolOrStringExpression {
  def unapply[T](o: Object): Option[String] = o match {
    case S.Symbol(value) => Some(value)
    case value: String => Some(value)
    case _ => None
  }
}

object TypeTaggedExpression {
  def unapply[T](o: Object): Option[(String, Object)] = o match {
    case S.List(List(SymbolOrStringExpression(className), exp)) => Some((className, exp))
    case _ => None
  }
}

object ListOfTypeTaggedExpression {
  def unapply[T](o: Object): Option[List[(String, Object)]] = o match {
    case S.List(xs) if xs.forall { case TypeTaggedExpression((_, _)) => true; case _ => false } =>
      Some(xs.collect { case TypeTaggedExpression((className, sexp)) => (className, sexp) })
    case _ => None
  }
}

object ArgumentsNameList {
  def unapply[T](o: Object): Option[(List[Symbol], Option[Symbol])] = {
    @tailrec
    def loop(o: Object, acc: List[Symbol]): Option[(List[Symbol], Option[Symbol])] = o match {
      case S.Cons((car: Symbol, cdr)) => loop(cdr, car::acc)
      case rest: Symbol => Some((acc.reverse, Some(rest)))
      case null => Some((acc.reverse, None))
      case _ => None
    }
    loop(o, Nil)
  }
}

object Parser {


  def parse(input: Object): Node = {
    try {
      parseInner(input)
    } catch {
      case e: Throwable =>
        println(s"Could not parse expression $input")
        throw e
    }
  }

  def parseInner(input: Object): Node = input match {
    case S.List(List(S.Symbol("define"), S.Symbol(name), expression)) =>
      Define(S.Symbol(name), parseInner(expression))

    case S.Cons(S.Symbol("define"), S.Cons(S.Cons(S.Symbol(name), ArgumentsNameList((args, restArg))), S.List(expressions))) =>
      DefineLambda(S.Symbol(name), Lambda(args, restArg, expressions.map(parse)))

    case S.Cons(S.Symbol("define-macro"), S.Cons(S.Cons(S.Symbol(name), ArgumentsNameList((args, restArg))), S.List(expressions))) =>
      DefineMacro(S.Symbol(name), Lambda(args, restArg, expressions.map(parse)))

    case S.Cons(S.Symbol("lambda"), S.Cons(ArgumentsNameList((args, restArg)), S.List(body))) =>
      Lambda(args, restArg, body.map(parse))

    case S.Cons(S.Symbol("begin"), S.List(expressions)) =>
      Program(expressions.map(parse))

    case S.List(List(S.Symbol("set!"), S.Symbol(name), expression)) =>
      SetBang(name, parseInner(expression))

    case S.List(List(S.Symbol("if"), condition, trueBranch)) =>
      If(parseInner(condition), parseInner(trueBranch), None)

    case S.List(List(S.Symbol("if"), condition, trueBranch, falseBranch)) =>
      If(parseInner(condition), parseInner(trueBranch), Some(parseInner(falseBranch)))

    case S.List(List(S.Symbol("quote"), expression)) =>
      Quote(expression)

    case S.List(List(S.Symbol("quasiquote"), expression)) =>
      Quasiquote(expression)

    case S.Cons(S.Symbol("new"), S.Cons(SymbolOrStringExpression(className), ListOfTypeTaggedExpression(args))) =>
      NewObject(className, args.map { case (className, node) => TypeTaggedNode(className, parseInner(node)) })

    case S.Cons(S.Symbol("call"), S.Cons(TypeTaggedExpression(className, target), S.Cons(S.Symbol(methodName), ListOfTypeTaggedExpression(args)))) =>
      MethodCall(TypeTaggedNode(className, parseInner(target)), methodName, args.map { case (className, node) => TypeTaggedNode(className, parseInner(node)) })

    case S.List(List(S.Symbol("field"), TypeTaggedExpression(className, target), S.Symbol(fieldName))) =>
      FieldAccess(TypeTaggedNode(className, parseInner(target)), fieldName)

    case S.Cons(procedure, S.List(args)) => FunctionApplication(parseInner(procedure), args.map(parseInner))

    case S.List(Nil) => Null

    case S.Symbol(name) => VariableLookup(name)

    case _ => Literal(input)
  }

}
