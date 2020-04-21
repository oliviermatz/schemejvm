package io.github.matzoliv.schemejvm.bootstrap

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParserTests extends FlatSpec with Matchers {

  def shouldParseTo(s: String, node: Node) = {
    "Parser" should s"parse $s to $node" in {
      val sExp = Reader.read(s)
      val ast = Parser.parse(sExp)
      ast shouldEqual node
      ast.sExp shouldEqual sExp
    }
  }

  shouldParseTo(
    "(lambda () #t)",
    Lambda(
      List(),
      None,
      List(Literal(S.Bool(true))))
  )

  shouldParseTo(
    "(lambda (x y) #t)",
    Lambda(
      List(S.Symbol("x"), S.Symbol("y")),
      None,
      List(Literal(S.Bool(true))))
  )

  shouldParseTo(
    "(lambda (x y . rest) #t)",
    Lambda(
      List(S.Symbol("x"), S.Symbol("y")),
      Some(S.Symbol("rest")),
      List(Literal(S.Bool(true))))
  )

  shouldParseTo(
    "(define x 15)",
    Define(
      S.Symbol("x"),
      Literal(S.Int(15)))
  )

  shouldParseTo(
    "(define (fn arg1 arg2) 42)",
    DefineLambda(
      S.Symbol("fn"),
      Lambda(
        List(S.Symbol("arg1"), S.Symbol("arg2")),
        None,
        List(Literal(S.Int(42)))))
  )

  shouldParseTo(
    "(define (fn arg1 arg2 . rest) 42)",
    DefineLambda(
      S.Symbol("fn"),
      Lambda(
        List(S.Symbol("arg1"), S.Symbol("arg2")),
        Some(S.Symbol("rest")),
        List(Literal(S.Int(42)))))
  )

  shouldParseTo(
    "(define-macro (fn arg1 arg2) 42)",
    DefineMacro(
      S.Symbol("fn"),
      Lambda(
        List(S.Symbol("arg1"), S.Symbol("arg2")),
        None,
        List(Literal(S.Int(42)))))
  )

  shouldParseTo(
    "(define-macro (fn arg1 arg2 . rest) 42)",
    DefineMacro(
      S.Symbol("fn"),
      Lambda(
        List(S.Symbol("arg1"), S.Symbol("arg2")),
        Some(S.Symbol("rest")),
        List(Literal(S.Int(42)))))
  )

  shouldParseTo(
    "(append x y)",
    FunctionApplication(
      VariableLookup("append"),
      List(VariableLookup("x"), VariableLookup("y")))
  )

  shouldParseTo(
    "(if (= 1 1) #t #f)",
    If(
      FunctionApplication(
        VariableLookup("="),
        List(Literal(S.Int(1)), Literal(S.Int(1)))),
      Literal(S.Bool(true)),
      Some(Literal(S.Bool(false))))
  )

  shouldParseTo(
    "(if (= 1 1) #t)",
    If(
      FunctionApplication(
        VariableLookup("="),
        List(Literal(S.Int(1)), Literal(S.Int(1)))),
      Literal(S.Bool(true)),
      None)
  )

  shouldParseTo(
    "(set! x 15)",
    SetBang("x", Literal(S.Int(15)))
  )

  shouldParseTo(
    "(begin 1 2 3)",
    Program(List(
      Literal(S.Int(1)),
      Literal(S.Int(2)),
      Literal(S.Int(3))
    ))
  )

  shouldParseTo(
    "'a",
    Quote(S.Symbol("a"))
  )

  shouldParseTo(
    "(quote a)",
    Quote(S.Symbol("a"))
  )

  shouldParseTo(
    "`a",
    Quasiquote(S.Symbol("a"))
  )

  shouldParseTo(
    "(quasiquote a)",
    Quasiquote(S.Symbol("a"))
  )

  shouldParseTo(
    "(new \"com.github.matzoliv.schemejvm.runtime.Pair\" (\"java.lang.Object\" 1) (\"java.lang.Object\" 2))",
    NewObject(
      "com.github.matzoliv.schemejvm.runtime.Pair",
      List(
        TypeTaggedNode("java.lang.Object", Literal(S.Int(1))),
        TypeTaggedNode("java.lang.Object", Literal(S.Int(2)))))
  )

  shouldParseTo(
    "(call (\"com.github.matzoliv.schemejvm.runtime.Pair\" '(1)) setCar (\"java.lang.Object\" 2))",
    MethodCall(
      TypeTaggedNode("com.github.matzoliv.schemejvm.runtime.Pair", Quote(S.List(S.Int(1)))),
      "setCar",
      List(TypeTaggedNode("java.lang.Object", Literal(S.Int(2))))
    )
  )
}
