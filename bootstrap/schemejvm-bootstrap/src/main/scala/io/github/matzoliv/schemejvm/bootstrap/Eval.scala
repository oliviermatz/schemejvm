package io.github.matzoliv.schemejvm.bootstrap

import java.util.concurrent.atomic.AtomicInteger

import io.github.matzoliv.schemejvm.runtime._

import scala.annotation.tailrec

case class Ref[T](var value: T)

case class EvalContext(bindings: Ref[Map[String, Ref[Object]]],
                       tailPosition: Boolean,
                       inTailRecursion: Boolean,
                       parentFunctionCalls: List[Node],
                       debug: Ref[Boolean]) {
  def addBindings(newBindings: Map[String, Ref[Object]]) =
    bindings.value = bindings.value ++ newBindings

  def lookup(name: String) =
    bindings.value.get(name).getOrElse(throw new UndefinedVariableException(name))

  def createSnapshot() =
    copy(bindings = Ref(bindings.value))

  def withParentFunctionCall(parent: Node) =
    copy(parentFunctionCalls = parent::parentFunctionCalls)
}

object EvalContext {
  def apply(bindings: Ref[Map[String, Ref[Object]]]): EvalContext =
    EvalContext(bindings, tailPosition = false, inTailRecursion = false, parentFunctionCalls = Nil, debug = Ref(false))
}

class UndefinedVariableException(name: String)
  extends Exception(s"$name is undefined")

class ApplyNonProcedure(value: Object)
  extends Exception(s"$value is not a procedure")

class InvalidTypeNumericalOperation(value: Object)
  extends Exception(s"$value must be a number")

class InvalidNumberOfArguments(name: String, n: Int)
  extends Exception(s"$name: invalid number of arguments $n")

class InvalidUnquoteSplicingInNonListExpression()
  extends Exception()

class InvalidUnquoteSplicingNonListResult()
  extends Exception()

sealed trait Procedure {
  def execute(args: List[Object], ctx: EvalContext): Object
}

case class Trampoline(cont: () => Object)

case class UserDefinedProcedure(argNames: Seq[String],
                                restArgName: Option[String],
                                program: Program,
                                capturedEnv: Map[String, Ref[Object]],
                                tag: String)
  extends Procedure {

  override def execute(argValues: List[Object], ctx: EvalContext): Object = {
    val bindings = restArgName match {
      case Some(restArgName) =>
        argNames.zip(argValues.take(argNames.length).map(Ref[Object])).toMap ++
          Map(restArgName -> Ref[Object](S.List(argValues.drop(argNames.length):_*))) ++
          capturedEnv

      case None =>
        argNames.zip(argValues.map(Ref[Object])).toMap ++ capturedEnv
    }

    Eval.eval(program, ctx.copy(bindings = Ref(bindings), tailPosition = true))
  }

  override def toString: String = s"#[procedure $tag]"
}

case class BuiltinProcedure(fn: (List[Object], EvalContext) => Object) extends Procedure {
  override def execute(args: List[Object], evalContext: EvalContext): AnyRef = fn(args, evalContext)
}

object Eval {

  def evalQuasiquoted(value: Object, ctx: EvalContext): Object = value match {
    case S.List(List(S.Symbol("unquote"), unquoted)) =>
      Eval.eval(Parser.parse(unquoted), ctx)

    case S.List(List(S.Symbol("unquote-splicing"), _)) =>
      throw new InvalidUnquoteSplicingInNonListExpression()

    case pair @ S.Cons((_, _)) =>
      evalQuasiquotedPair(List.empty, pair, ctx)

    case _ => value
  }

  @tailrec
  def evalQuasiquotedPair(acc: List[Object], value: Object, ctx: EvalContext): Object = value match {
    case S.Cons((S.List(List(S.Symbol("unquote-splicing"), spliced)), cdr)) =>
      Eval.eval(Parser.parse(spliced), ctx) match {
        case S.List(result) =>
          evalQuasiquotedPair(result.reverse ++ acc, cdr, ctx)
        case _ => throw new InvalidUnquoteSplicingNonListResult()
      }

    case S.List(List(S.Symbol("unquote"), _)) =>
      S.List.createWithTail(acc.reverse, evalQuasiquoted(value, ctx))

    case S.Cons((car, cdr)) =>
      evalQuasiquotedPair(evalQuasiquoted(car, ctx)::acc, cdr, ctx)

    case value =>
      S.List.createWithTail(acc.reverse, evalQuasiquoted(value, ctx))
  }

  def collectDefinedNames(body: List[Node]): Set[String] = body.foldLeft(Set.empty[String]) {
    case (acc, Define(S.Symbol(name), _)) => acc + name
    case (acc, DefineLambda(S.Symbol(name), _)) => acc + name
    case (acc, inner: Program) => acc ++ collectDefinedNames(inner.body)
    case (acc, _) => acc
  }

  def getBindings(body: List[Node]) =
    collectDefinedNames(body).map(_ -> Ref[Object](Void.value)).toMap

  def getFreeVariablesQuasiquoted(bound: Set[String], value: Object): (Set[String], Object) = value match {
    case S.List(List(S.Symbol("unquote"), unquoted)) =>
      val (freeVariables, newUnquoted) = getFreeVariables(bound, Parser.parse(unquoted))
      (freeVariables, S.List(S.Symbol("unquote"), newUnquoted.sExp))

    case S.List(List(S.Symbol("unquote-splicing"), unquotedSpliced)) =>
      val (freeVariables, newUnquoteSpliced) = getFreeVariables(bound, Parser.parse(unquotedSpliced))
      (freeVariables, S.List(S.Symbol("unquote-splicing"), newUnquoteSpliced.sExp))

    case pair @ S.Cons((_, _)) =>
      getFreeVariablesQuasiquotedPair(bound, Nil, Set.empty, pair)

    case x => (Set.empty[String], x)
  }

  @tailrec
  def getFreeVariablesQuasiquotedPair(bound: Set[String], acc: List[Object], freeVariables: Set[String], value: Object): (Set[String], Object) = value match {
    case S.Cons((car, cdr)) =>
      val (newFreeVariables, newCar) = getFreeVariablesQuasiquoted(bound, car)
      getFreeVariablesQuasiquotedPair(bound, newCar::acc, freeVariables ++ newFreeVariables, cdr)

    case value =>
      val (newFreeVariables, newValue) = getFreeVariablesQuasiquoted(bound, value)
      (newFreeVariables ++ freeVariables, S.List.createWithTail(acc.reverse, newValue))
  }

  def getFreeVariablesFromLambda(bound: Set[String], lambda: Lambda): (Set[String], Lambda) = lambda match {
    case Lambda(args: List[Symbol], rest, body: List[Node], None) =>
      val locallyBoundArgs = args.map(_.getName()) ++ rest.map(_.getName())
      val locallyBoundDefines = collectDefinedNames(body)
      val (newLambdaFreeVariables, newBody) = getFreeVariablesFromNodes((locallyBoundArgs ++ locallyBoundDefines).toSet, body)
      (newLambdaFreeVariables -- bound, lambda.copy(body = newBody, freeVariables = Some(newLambdaFreeVariables)))

    case Lambda(_, _, _, Some(freeVariables)) =>
      (freeVariables -- bound, lambda)
  }

  def getFreeVariablesFromNodes(bound: Set[String], nodes: List[Node]): (Set[String], List[Node]) = {
    val (freeVariables, newNodes) = nodes.foldLeft((Set.empty[String], Seq.empty[Node])) {
      case ((freeVariables, newNodes), node) =>
        val (newFreeVariables, newNode) = getFreeVariables(bound, node)
        ((freeVariables ++ newFreeVariables), (newNodes :+ newNode))
    }
    (freeVariables, newNodes.toList)
  }

  def getFreeVariables(bound: Set[String], node: Node): (Set[String], Node) = node match {
    case Program(body) =>
      val locallyBound = collectDefinedNames(body) ++ bound
      val (freeVariables, newBody) = getFreeVariablesFromNodes(locallyBound, body)
      (freeVariables, Program(newBody))

    case Define(name, value) =>
      // (define ...) form can only be situated in a program (begin ...).
      // Thus the defined name is already taken into account in the bound name set.
      val (freeVariables, newValue) = getFreeVariables(bound, value)
      (freeVariables, Define(name, newValue))

    case DefineLambda(name, lambda: Lambda) =>
      val (freeVariables, newLambda) = getFreeVariablesFromLambda(bound, lambda)
      (freeVariables, DefineLambda(name, newLambda))

    case _: DefineMacro => ???

    case lambda: Lambda => getFreeVariablesFromLambda(bound, lambda)

    case SetBang(name, value: Node) =>
      val (freeVariables, newValue) = getFreeVariables(bound, value)
      val freeVariablesWithName =
        if (bound.contains(name)) {
          Set.empty[String]
        } else {
          Set(name)
        } ++ freeVariables

      (freeVariablesWithName, SetBang(name, newValue))

    case If(condition: Node, trueBranch: Node, falseBranch: Option[Node]) =>
      val (conditionFreeVariables, newCondition) = getFreeVariables(bound, condition)
      val (trueBranchFreeVariables, newTrueBranch) = getFreeVariables(bound, trueBranch)
      val (falseBranchFreeVariables: Set[String], maybeNewFalseBranch: Option[Node]) = falseBranch.map(getFreeVariables(bound, _))
        .map(x => (x._1, Some(x._2)))
        .getOrElse((Set.empty, None))
      val freeVariables = conditionFreeVariables ++ trueBranchFreeVariables ++ falseBranchFreeVariables
      (freeVariables, If(newCondition, newTrueBranch, maybeNewFalseBranch))

    case Quasiquote(quasiquoted) =>
      val (freeVariables, newQuasiquoted) = getFreeVariablesQuasiquoted(bound, quasiquoted)
      (freeVariables, Quasiquote(newQuasiquoted))

    case x: Quote => (Set.empty, x)

    case Null => (Set.empty, Null)

    case FunctionApplication(procedure: Node, args: List[Node]) =>
      val (procedureFreeVariables, newProcedure) = getFreeVariables(bound, procedure)
      val (argsFreeVariables, newArgs) = getFreeVariablesFromNodes(bound, args)
      (procedureFreeVariables ++ argsFreeVariables, FunctionApplication(newProcedure, newArgs))

    case NewObject(className, typeTaggedArgs) =>
      val (freeVariables, newArgs) = getFreeVariablesFromNodes(bound, typeTaggedArgs.map(_.node))
      val newNode = NewObject(
        className,
        typeTaggedArgs
          .map(_.className)
          .zip(newArgs)
          .map { case (className, arg) => TypeTaggedNode(className, arg) }
      )
      (freeVariables, newNode)

    case MethodCall(TypeTaggedNode(className, target), methodName, typeTaggedArgs) =>
      val (targetFreeVariables, newTarget) = getFreeVariables(bound, target)
      val (argsFreeVariables, newArgs) = getFreeVariablesFromNodes(bound, typeTaggedArgs.map(_.node))
      val newNode = MethodCall(
        TypeTaggedNode(className, newTarget),
        methodName,
        typeTaggedArgs
          .map(_.className)
          .zip(newArgs)
          .map { case (className, arg) => TypeTaggedNode(className, arg) }
      )
      (argsFreeVariables ++ targetFreeVariables, newNode)

    case FieldAccess(TypeTaggedNode(className, target), fieldName) =>
      val (freeVariables, newTarget) = getFreeVariables(bound, target)
      (freeVariables, FieldAccess(TypeTaggedNode(className, newTarget), fieldName))

    case VariableLookup(name: String) =>
      val freeVariables = if (bound.contains(name)) {
        Set.empty[String]
      } else {
        Set(name)
      }
      (freeVariables, VariableLookup(name))

    case value: Literal =>
      (Set.empty, value)
  }

  private def lastLambdaId = new AtomicInteger()

  def evalLambda(lambda: Lambda, ctx: EvalContext, name: Option[String]): Procedure = {
    val (freeVariables, newLambda) = getFreeVariablesFromLambda(Set.empty, lambda)
    val captures = freeVariables.toSeq.map { x => x -> ctx.lookup(x) }.toMap

    UserDefinedProcedure(
      lambda.args.map(_.getName()),
      lambda.restArg.map(_.getName()),
      Program(newLambda.body),
      captures,
      name.getOrElse(s"lambda#${lastLambdaId.getAndIncrement()}")
    )
  }

  def evalProgramWithFullEnv(program: Program, ctx: EvalContext): Object = {
    def evalInProgram(node: Node, ctx: EvalContext): Object = node match {
      case p: Program => evalProgramWithFullEnv(p, ctx)
      case node => eval(node, ctx)
    }

    val last = program.body.last
    val exceptLast = program.body.take(program.body.length - 1)

    exceptLast.foreach(evalInProgram(_, ctx.copy(tailPosition = false)))
    evalInProgram(last, ctx)
  }

  @tailrec
  def executeTrampoline(cont: () => Object): Object = {
    cont() match {
      case Trampoline(cont) => executeTrampoline(cont)
      case result => result
    }
  }

  def eval(node: Node, ctx: EvalContext): Object = {
    node match {
      case Literal(value) => value

      case p: Program =>
        ctx.addBindings(getBindings(p.body))
        evalProgramWithFullEnv(p, ctx)

      case Define(S.Symbol(name), value) =>
        assert(ctx.bindings.value.contains(name))
        ctx.lookup(name).value = eval(value, ctx)
        Void.value

      case DefineLambda(S.Symbol(name), lambda: Lambda) =>
        assert(ctx.bindings.value.contains(name))
        ctx.lookup(name).value = evalLambda(lambda, ctx, Some(name))
        Void.value

      case _: DefineMacro => ???

      case x: Lambda => evalLambda(x, ctx, name = None)

      case SetBang(name: String, value: Node) =>
        ctx.lookup(name).value = eval(value, ctx.copy(tailPosition = false))
        Void.value

      case If(condition: Node, trueBranch: Node, falseBranch: Option[Node]) =>
        if (eval(condition, ctx.copy(tailPosition = false)) != S.Bool(false)) {
          eval(trueBranch, ctx)
        } else {
          falseBranch.map(eval(_, ctx)).getOrElse(Void.value)
        }

      case Quasiquote(value) => evalQuasiquoted(value, ctx.copy(tailPosition = false))

      case Quote(value) => value

      case VariableLookup(name: String) => ctx.lookup(name).value

      case Null => throw new InvalidSyntax(null)

      case FunctionApplication(procedure: Node, args: List[Node]) =>
        if (ctx.debug.value) {
          println(">" * ctx.parentFunctionCalls.length + " " + node.sExp.toString)
        }

        val cont: () => Object = () => {
          eval(procedure, ctx) match {
            case procedure: Procedure =>
              val evaledArgs = args.map(Eval.eval(_, ctx.copy(tailPosition = false)))
              procedure.execute(
                evaledArgs,
                ctx
                  .withParentFunctionCall(node)
                  .copy(inTailRecursion = ctx.tailPosition)
              )

            case x => throw new ApplyNonProcedure(x)
          }
        }

        if (ctx.tailPosition && ctx.inTailRecursion) {
          Trampoline(cont)
        } else if (ctx.tailPosition) {
          executeTrampoline(cont)
        } else {
          cont()
        }

      case NewObject(className, args) =>
        val evaledArgs = args.map { x => Eval.eval(x.node, ctx.copy(tailPosition = false)) }
        val klass = ReflectionUtils.getClassForName(className)
        val argTypes: Array[Class[_]] = args.map { x => ReflectionUtils.getClassForName(x.className) }.toArray
        val ctor = klass.getConstructor(argTypes: _*)
        ctor.newInstance(evaledArgs: _*).asInstanceOf[Object]

      case MethodCall(target, methodName, args) =>
        val evaledArgs = args.map { x => Eval.eval(x.node, ctx.copy(tailPosition = false)) }
        val evaledTarget = Eval.eval(target.node, ctx)
        val klass = ReflectionUtils.getClassForName(target.className)
        val argTypes: Array[Class[_]] = args.map { x => ReflectionUtils.getClassForName(x.className) }.toArray
        val ctor = klass.getMethod(methodName, argTypes: _*)
        ctor.invoke(evaledTarget, evaledArgs: _*)

      case FieldAccess(target, fieldName) =>
        val evaledTarget = Eval.eval(target.node, ctx.copy(tailPosition = false))
        val klass = ReflectionUtils.getClassForName(target.className)
        val field = klass.getField(fieldName)
        field.get(evaledTarget)
    }
  }
}
