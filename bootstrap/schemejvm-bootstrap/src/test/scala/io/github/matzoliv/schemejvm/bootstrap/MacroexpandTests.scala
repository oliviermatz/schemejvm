package io.github.matzoliv.schemejvm.bootstrap

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MacroexpandTests extends FlatSpec with Matchers {

  def evalString(s: String) = Eval.eval(Parser.parse(Reader.read(s)), EvalContext(Ref(Stdlib.declarations)))

  def shouldMacroexpandTo(s: String, expanded: String) = {
    "Macroexpand" should s"expand $s to $expanded" in {
      val result = Macroexpand.macroexpand(
        Parser.parse(Reader.read(s)),
        MacroexpandContext(
          macros = Ref(Map.empty),
          evalContext = EvalContext(Ref(Stdlib.declarations))
        )
      )

      assert(
        result == Parser.parse(Reader.read(expanded)),
        s"${result.sExp} was not equal to ${expanded}"
      )
    }
  }

  shouldMacroexpandTo(
    """
      |(begin
      |  (define-macro (square x)
      |    `(* ,x ,x))
      |  (square x))
    """.stripMargin,
    """
      |(begin
      |  #!void
      |  (* x x))
    """.stripMargin
  )

}
