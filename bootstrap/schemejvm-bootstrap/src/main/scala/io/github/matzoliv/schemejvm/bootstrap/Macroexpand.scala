package io.github.matzoliv.schemejvm.bootstrap

import io.github.matzoliv.schemejvm.runtime._

import scala.annotation.tailrec

case class MacroexpandContext(macros: Ref[Map[String, Procedure]],
                              evalContext: EvalContext)
{
  def withMacros(macros: Map[String, Procedure]) =
    this.macros.value = this.macros.value ++ macros

  def snapshot =
    MacroexpandContext(
      macros = Ref(macros.value),
      evalContext = evalContext.createSnapshot()
    )
}

object Macroexpand {

  def macroexpand1Quasiquoted(value: Object, env: MacroexpandContext, hasExpanded: Ref[Boolean]): Object = value match {
    case S.List(List(S.Symbol("unquote"), unquoted)) =>
      S.List(S.Symbol("unquote"), macroexpand1(Parser.parse(unquoted), env, hasExpanded).sExp)

    case S.List(List(S.Symbol("unquote-splicing"), unquoted)) =>
      S.List(S.Symbol("unquote-splicing"), macroexpand1(Parser.parse(unquoted), env, hasExpanded).sExp)

    case pair @ S.Cons((_, _)) =>
      macroexpand1QuasiquotedPair(List.empty, pair, env, hasExpanded)

    case _ => value
  }

  @tailrec
  def macroexpand1QuasiquotedPair(acc: List[Object], value: Object, env: MacroexpandContext, hasExpanded: Ref[Boolean]): Object = value match {
    case S.Cons((car, cdr)) =>
      macroexpand1QuasiquotedPair(
        macroexpand1Quasiquoted(car, env, hasExpanded)::acc,
        cdr,
        env,
        hasExpanded)

    case value =>
      S.List.createWithTail(acc.reverse, macroexpand1Quasiquoted(value, env, hasExpanded))
  }

  def collectMacroDefinitions(acc: Map[String, Procedure], node: Node, ctx: MacroexpandContext): Map[String, Procedure] = node match {
    case DefineMacro(S.Symbol(name), lambda) =>
      val expandedLambda = lambda.copy(
        body = macroexpand(Program(lambda.body), ctx).asInstanceOf[Program].body
      )
      acc + ((name, Eval.evalLambda(expandedLambda, ctx.evalContext, Some(name))))
    case Program(body) =>
      body.foldLeft(acc) { case (acc, x) =>
        collectMacroDefinitions(acc, x, ctx)
      }
    case _ => acc
  }

  def macroexpand1Program(program: Program, ctx: MacroexpandContext, hasExpanded: Ref[Boolean]): Program = {
    val newMacros = collectMacroDefinitions(Map.empty, program, ctx)
    ctx.withMacros(newMacros)
    macroexpand1ProgramWithFullEnv(program, ctx, hasExpanded)
  }

  def macroexpand1ProgramWithFullEnv(program: Program, env: MacroexpandContext, hasExpanded: Ref[Boolean]): Program = {
    def loop(program: Program, env: MacroexpandContext): Program =
      Program(program.body.map {
        case p: Program => loop(p, env)
        case node => macroexpand1(node, env, hasExpanded)
      })
    loop(program, env)
  }

  def macroexpand1(node: Node, ctx: MacroexpandContext, hasExpanded: Ref[Boolean]): Node = node match {
    case p: Program => macroexpand1Program(p, ctx, hasExpanded)

    case Define(name, value) => Define(name, macroexpand1(value, ctx, hasExpanded))

    case DefineLambda(name, Lambda(args, rest, body, _)) =>
      DefineLambda(name, Lambda(args, rest, macroexpand1Program(Program(body), ctx, hasExpanded).body))

    case _: DefineMacro => Literal(Void.value)

    case Lambda(args, rest, body, _) =>
      Lambda(args, rest, macroexpand1Program(Program(body), ctx, hasExpanded).body)

    case SetBang(name , value) =>
      SetBang(name, macroexpand1(value, ctx, hasExpanded))

    case If(condition, trueBranch, falseBranch) =>
      If(
        macroexpand1(condition, ctx, hasExpanded),
        macroexpand1(trueBranch, ctx, hasExpanded),
        falseBranch.map(macroexpand1(_, ctx, hasExpanded))
      )

    case Quasiquote(x) => Quasiquote(macroexpand1Quasiquoted(x, ctx, hasExpanded))

    case Null => null

    case FunctionApplication(VariableLookup(name), args) if ctx.macros.value.contains(name) =>
      hasExpanded.value = true
      Parser.parse(ctx.macros.value(name).execute(args.map(_.sExp), ctx.evalContext))

    case FunctionApplication(procedure, args) =>
      FunctionApplication(
        macroexpand1(procedure, ctx, hasExpanded),
        args.map(macroexpand1(_, ctx, hasExpanded))
      )

    case NewObject(className, args) =>
      NewObject(className, args.map { case x => x.copy(node = macroexpand1(x.node, ctx, hasExpanded)) })

    case MethodCall(target, methodName, args) =>
      MethodCall(
        target.copy(node = macroexpand1(target.node, ctx, hasExpanded)),
        methodName,
        args.map { case x => x.copy(node = macroexpand1(x.node, ctx, hasExpanded)) })

    case FieldAccess(target, fieldName) =>
      FieldAccess(
        target.copy(node = macroexpand1(target.node, ctx, hasExpanded)),
        fieldName)

    case _ => node
  }

  def macroexpand(node: Node, env: MacroexpandContext): Node = {

    @tailrec
    def loop(node: Node, env: MacroexpandContext): Node = {
      val hasExpanded = Ref(false)

      val postExpansionNode = macroexpand1(node, env, hasExpanded)

      if (hasExpanded.value) {
        loop(postExpansionNode, env)
      } else {
        postExpansionNode
      }
    }

    loop(node, env)
  }

}
