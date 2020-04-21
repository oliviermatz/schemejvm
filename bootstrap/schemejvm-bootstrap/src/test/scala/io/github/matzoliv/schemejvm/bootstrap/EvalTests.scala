package io.github.matzoliv.schemejvm.bootstrap

import org.scalactic.TolerantNumerics

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EvalTests extends FlatSpec with Matchers {

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(1e-4f)

  def evalString(s: String) = Eval.eval(Parser.parse(Reader.read(s)), EvalContext(Ref(Stdlib.declarations)))

  def shouldEvalTo(s: String, value: Object) = {
    "Eval" should s"eval $s to $value" in {
      evalString(s) shouldEqual value
    }
  }

  shouldEvalTo(
    "(begin 1 2 3)",
    S.Int(3)
  )

  shouldEvalTo(
    "((lambda () 20))",
    S.Int(20)
  )

  shouldEvalTo("(* 2 2)", S.Int(4))
  shouldEvalTo("(* 2.2 2)", S.Double(4.4))
  shouldEvalTo("(* 2 2.2)", S.Double(4.4))
  shouldEvalTo("(- 4 2)", S.Int(2))

  shouldEvalTo("(apply < '(4 2))", S.Bool(false))

  "Eval" should s"eval (- 2.2 2) to 0.2" in {
    evalString("(- 2.2 2)") === 0.2
  }

  "Eval" should s"eval (- 2 2.2) to 0.2" in {
    evalString("(- 2 2.2)") === 0.2
  }

  shouldEvalTo(
    "(begin (define x 10) x)",
    S.Int(10)
  )

  shouldEvalTo(
    """
      |(begin
      |   (define x 10)
      |   (define y 20)
      |   (+ x y))
    """.stripMargin,
    S.Int(30)
  )

  shouldEvalTo(
    """
      |(begin
      |   (define (list . xs) xs)
      |   (list 1 2 3))
    """.stripMargin,
    S.List(S.Int(1), S.Int(2), S.Int(3))
  )

  shouldEvalTo(
    """
      |(begin
      |  (define (inc x)
      |    (+ x 1))
      |  (inc 1))
    """.stripMargin,
    S.Int(2)
  )

  shouldEvalTo(
    """
      |(begin
      |  (define fn
      |    (begin
      |      (define x 10)
      |      (lambda () (* x x))))
      |  (fn))
    """.stripMargin,
    S.Int(100)
  )

  shouldEvalTo(
    """
      |(begin
      |  (define (dec x)
      |     (display "hello")
      |     (if (> x 0)
      |       (dec (- x 1))
      |       x))
      |  (dec 10))
    """.stripMargin,
    S.Int(0)
  )

  shouldEvalTo(
    """
      |(begin
      |  (define x 10)
      |  (begin
      |    (define y 5))
      |  (+ x y))
    """.stripMargin,
    S.Int(15)
  )

  shouldEvalTo("'(1 2)", S.List(S.Int(1), S.Int(2)))

  shouldEvalTo("`(1 ,@'(2 3) 4)", S.List(S.Int(1), S.Int(2), S.Int(3), S.Int(4)))

  shouldEvalTo("`(1 (2 ,@'(3 4)) 5)", S.List(S.Int(1), S.List(S.Int(2), S.Int(3), S.Int(4)), S.Int(5)))

  shouldEvalTo("`(1 ,(+ 1 1) 3)", S.List(S.Int(1), S.Int(2), S.Int(3)))

  shouldEvalTo("`,(+ 1 1)", S.Int(2))

  shouldEvalTo("`(1 ,(+ 1 1) . 3)", S.Cons(S.Int(1), S.Cons(S.Int(2), S.Int(3))))

  shouldEvalTo("`(1 2 . ,(+ 1 2))", S.Cons(S.Int(1), S.Cons(S.Int(2), S.Int(3))))

  // Tail recursion optimization case
  shouldEvalTo(
    """
      |(begin
      |  (define (dec x)
      |     (if (> x 0)
      |       (dec (- x 1))
      |       x))
      |  (dec 100000))
    """.stripMargin,
    S.Int(0)
  )

  shouldEvalTo(
    """
      |((lambda (x)
      |  `(1 2 ,(+ x ((lambda (y) y) (+ x 1)))))
      |  10)
    """.stripMargin,
    S.List(S.Int(1), S.Int(2), S.Int(21))
  )
}
