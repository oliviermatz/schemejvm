package io.github.matzoliv.schemejvm.bootstrap

import java.io._

import scala.annotation.tailrec
import scala.util.parsing.input.StreamReader
import scala.collection.JavaConverters._

object Repl {

  def expandAndEval(node: Node, macroexpandCtx: MacroexpandContext, evalCtx: EvalContext): Object = {
    val newMacros = Macroexpand.collectMacroDefinitions(Map.empty, node, macroexpandCtx)
    macroexpandCtx.withMacros(newMacros)
    val expandedNode = Macroexpand.macroexpand(node, macroexpandCtx)

    val toEval = Program(List(expandedNode))
    evalCtx.addBindings(Eval.getBindings(toEval.body))
    Eval.evalProgramWithFullEnv(toEval, evalCtx)
  }

  def readNextExpression() = {
    val in = new BufferedReader(new InputStreamReader(System.in))

    @tailrec
    def readUntilSuccess(acc: String): Object = {
      val line = in.readLine()
      val input = acc + "\n" + line
      val reader = new InputStreamReader(new ByteArrayInputStream(input.getBytes("UTF8")))

      Reader.parseAll(Reader.expression, reader) match {
        case Reader.Success(result, _) => result
        case Reader.Failure(_, _) => readUntilSuccess(input + "\n")
        case Reader.Error(msg, _) =>  throw new ReadFailedException(msg)
      }
    }

    readUntilSuccess("")
  }

  @tailrec
  def readEvalLoop(macroexpandCtx: MacroexpandContext, evalCtx: EvalContext): Unit = {
    print("> ")
    try {
      val expression = readNextExpression()
      val node = Parser.parse(expression)
      val result = expandAndEval(node, macroexpandCtx, evalCtx)
      println(result)
    } catch {
      case exn: Throwable =>
        println(s"${exn.getClass.getName}: ${exn.getMessage()}")
        exn.getStackTrace().foreach(println(_))
        evalCtx.addBindings(Map("*exn*" -> Ref(exn)))
    }

    readEvalLoop(macroexpandCtx, evalCtx)
  }

  def main(args: Array[String])  = {
    val evalContext = EvalContext(Ref(Stdlib.declarations))
    val macroexpandContext = MacroexpandContext(Ref(Map.empty), evalContext)

    val loadFile = Ref[Object](BuiltinProcedure((args, _) => args match {
      case (filename: String) :: Nil =>
        val fileReader = new FileReader(filename)
        try {
          val reader = StreamReader(fileReader)
          val all = Reader.readMultiple(reader).map(Parser.parse)
          expandAndEval(Program(all), macroexpandContext, evalContext)
        } finally {
          fileReader.close()
        }
      case _ => throw new InvalidNumberOfArguments("load", args.length)
    }))
    evalContext.addBindings(Map("load" -> loadFile))

    def prepareMacroexpandContext(node: Node): MacroexpandContext = {
      val ctx = macroexpandContext.snapshot
      val newMacros = Macroexpand.collectMacroDefinitions(Map.empty, node, macroexpandContext.snapshot)
      ctx.withMacros(newMacros)
      ctx
    }

    val macroexpand = Ref[Object](BuiltinProcedure((args, _) => args match {
      case x :: Nil =>
        val node = Parser.parse(x)
        val ctx = prepareMacroexpandContext(node)
        Macroexpand.macroexpand(node, ctx).sExp

      case _ => throw new InvalidNumberOfArguments("macroexpand", args.length)
    }))
    evalContext.addBindings(Map("macroexpand" -> macroexpand))

    val macroexpand1 = Ref[Object](BuiltinProcedure((args, ctx) => args match {
      case x :: Nil =>
        val node = Parser.parse(x)
        val ctx = prepareMacroexpandContext(node)
        Macroexpand.macroexpand1(node, ctx, Ref(false)).sExp

      case _ => throw new InvalidNumberOfArguments("macroexpand-1", args.length)
    }))
    evalContext.addBindings(Map("macroexpand-1" -> macroexpand1))

    readEvalLoop(macroexpandContext, evalContext)
  }

}
