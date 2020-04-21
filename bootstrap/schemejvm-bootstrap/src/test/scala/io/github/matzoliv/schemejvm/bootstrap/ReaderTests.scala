package io.github.matzoliv.schemejvm.bootstrap

import io.github.matzoliv.schemejvm.runtime.{Void, Eof}
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReaderTests extends FlatSpec with Matchers {

  def shouldReadTo(s: String, o: Object) = {
    "Reader" should s"read $s to $o" in {
      Reader.read(s) shouldEqual o
    }
  }

  shouldReadTo("#t", S.Bool(true))
  shouldReadTo("#f", S.Bool(false))
  shouldReadTo("hello", S.Symbol("hello"))
  shouldReadTo("#!void", Void.value)
  shouldReadTo("#!eof", Eof.value)
  shouldReadTo("\"hello\"", "hello")
  shouldReadTo("123", S.Int(123))
  shouldReadTo("123.123", S.Double(123.123))
  shouldReadTo("123.", S.Double(123.0))
  shouldReadTo("+", S.Symbol("+"))
  shouldReadTo("-", S.Symbol("-"))
  shouldReadTo("<", S.Symbol("<"))
  shouldReadTo(">", S.Symbol(">"))
  shouldReadTo("<=", S.Symbol("<="))
  shouldReadTo(">=", S.Symbol(">="))
  shouldReadTo("#\\a", new Character('a'))
  shouldReadTo("\"\"", "")
  shouldReadTo("\"\\\"\"", "\"")
  shouldReadTo("\"  \"", "  ")
  // shouldReadTo("\"\\n\"", "\n")
  shouldReadTo("(\"foo\" \"bar\")", S.List("foo", "bar"))

  shouldReadTo("()", null)
  shouldReadTo("(1 2)", S.Cons(S.Int(1), S.Cons(S.Int(2), null)))
  shouldReadTo("(1 . 2)", S.Cons(S.Int(1), S.Int(2)))
  shouldReadTo("(1 2 . 3)", S.Cons(S.Int(1), S.Cons(S.Int(2), S.Int(3))))
  shouldReadTo("(1 (2 3) ((4 5) 6))", S.List(S.Int(1), S.List(S.Int(2), S.Int(3)), S.List(S.List(S.Int(4), S.Int(5)), S.Int(6))))

  shouldReadTo("'1", S.List(S.Symbol("quote"), S.Int(1)))
  shouldReadTo("'(1 2)", S.List(S.Symbol("quote"), S.List(S.Int(1), S.Int(2))))
  shouldReadTo(",@(append a b)", S.List(S.Symbol("unquote-splicing"), S.List(S.Symbol("append"), S.Symbol("a"), S.Symbol("b"))))
  shouldReadTo(",(+ 1 1)", S.List(S.Symbol("unquote"), S.List(S.Symbol("+"), S.Int(1), S.Int(1))))
  shouldReadTo("`(let ,(append a b) 1)", S.List(S.Symbol("quasiquote"), S.List(S.Symbol("let"), S.List(S.Symbol("unquote"), S.List(S.Symbol("append"), S.Symbol("a"), S.Symbol("b"))), S.Int(1))))

  shouldReadTo("(1 2) (3 4 5)", S.Cons(S.Int(1), S.Cons(S.Int(2), null)))

  "Reader" should s"readMultiple (1 2) (3 4 5) to ${List(S.List(S.Int(1), S.Int(2)), S.List(S.Int(3), S.Int(4), S.Int(5)))}" in {
    Reader.readMultiple("(1 2)\n(3 4 5)") shouldEqual List(S.List(S.Int(1), S.Int(2)), S.List(S.Int(3), S.Int(4), S.Int(5)))
  }
}
