package io.github.matzoliv.schemejvm.bootstrap

import io.github.matzoliv.schemejvm.runtime._

object Stdlib {

  val declarations = Map(
    "+" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case Nil => S.Int(0)
      case head::tail =>
        tail.foldLeft(head) {
          case (sum: java.lang.Long, arg: java.lang.Long) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Float) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Double) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Integer) => (sum + arg).asInstanceOf[Object]

          case (sum: java.lang.Double, arg: java.lang.Long) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Float) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Double) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Integer) => (sum + arg).asInstanceOf[Object]

          case (sum: java.lang.Integer, arg: java.lang.Long) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Float) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Double) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Integer) => (sum + arg).asInstanceOf[Object]

          case (sum: java.lang.Float, arg: java.lang.Long) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Float) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Double) => (sum + arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Integer) => (sum + arg).asInstanceOf[Object]

          case x => throw new InvalidTypeNumericalOperation(x)
        }
    })),

    "-" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case Nil => S.Int(0)
      case head::tail =>
        tail.foldLeft(head) {
          case (sum: java.lang.Long, arg: java.lang.Long) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Float) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Double) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Integer) => (sum - arg).asInstanceOf[Object]

          case (sum: java.lang.Double, arg: java.lang.Long) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Float) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Double) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Integer) => (sum - arg).asInstanceOf[Object]

          case (sum: java.lang.Integer, arg: java.lang.Long) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Float) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Double) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Integer) => (sum - arg).asInstanceOf[Object]

          case (sum: java.lang.Float, arg: java.lang.Long) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Float) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Double) => (sum - arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Integer) => (sum - arg).asInstanceOf[Object]

          case x => throw new InvalidTypeNumericalOperation(x)
        }
    })),

    "*" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case Nil => S.Int(0)
      case head::tail =>
        tail.foldLeft(head) {
          case (sum: java.lang.Long, arg: java.lang.Long) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Float) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Double) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Integer) => (sum * arg).asInstanceOf[Object]

          case (sum: java.lang.Double, arg: java.lang.Long) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Float) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Double) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Integer) => (sum * arg).asInstanceOf[Object]

          case (sum: java.lang.Integer, arg: java.lang.Long) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Float) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Double) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Integer) => (sum * arg).asInstanceOf[Object]

          case (sum: java.lang.Float, arg: java.lang.Long) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Float) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Double) => (sum * arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Integer) => (sum * arg).asInstanceOf[Object]

          case x => throw new InvalidTypeNumericalOperation(x)
        }
    })),

    "/" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case Nil => S.Int(0)
      case head::tail =>
        tail.foldLeft(head) {
          case (sum: java.lang.Long, arg: java.lang.Long) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Float) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Double) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Integer) => (sum / arg).asInstanceOf[Object]

          case (sum: java.lang.Double, arg: java.lang.Long) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Float) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Double) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Integer) => (sum / arg).asInstanceOf[Object]

          case (sum: java.lang.Integer, arg: java.lang.Long) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Float) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Double) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Integer) => (sum / arg).asInstanceOf[Object]

          case (sum: java.lang.Float, arg: java.lang.Long) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Float) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Double) => (sum / arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Integer) => (sum / arg).asInstanceOf[Object]

          case x => throw new InvalidTypeNumericalOperation(x)
        }
    })),

    ">" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case fst::snd::Nil =>
        (fst, snd) match {
          case (sum: java.lang.Long, arg: java.lang.Long) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Float) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Double) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Integer) => (sum > arg).asInstanceOf[Object]

          case (sum: java.lang.Double, arg: java.lang.Long) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Float) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Double) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Integer) => (sum > arg).asInstanceOf[Object]

          case (sum: java.lang.Integer, arg: java.lang.Long) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Float) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Double) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Integer) => (sum > arg).asInstanceOf[Object]

          case (sum: java.lang.Float, arg: java.lang.Long) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Float) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Double) => (sum > arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Integer) => (sum > arg).asInstanceOf[Object]

          case x => throw new InvalidTypeNumericalOperation(x)
        }

      case x => throw new InvalidNumberOfArguments(">", args.length)
    })),

    ">=" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case fst::snd::Nil =>
        (fst, snd) match {
          case (sum: java.lang.Long, arg: java.lang.Long) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Float) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Double) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Integer) => (sum >= arg).asInstanceOf[Object]

          case (sum: java.lang.Double, arg: java.lang.Long) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Float) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Double) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Integer) => (sum >= arg).asInstanceOf[Object]

          case (sum: java.lang.Integer, arg: java.lang.Long) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Float) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Double) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Integer) => (sum >= arg).asInstanceOf[Object]

          case (sum: java.lang.Float, arg: java.lang.Long) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Float) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Double) => (sum >= arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Integer) => (sum >= arg).asInstanceOf[Object]

          case x => throw new InvalidTypeNumericalOperation(x)
        }
      case x => throw new InvalidNumberOfArguments(">=", args.length)
    })),

    "<" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case fst::snd::Nil =>
        (fst, snd) match {
          case (sum: java.lang.Long, arg: java.lang.Long) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Float) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Double) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Integer) => (sum < arg).asInstanceOf[Object]

          case (sum: java.lang.Double, arg: java.lang.Long) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Float) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Double) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Integer) => (sum < arg).asInstanceOf[Object]

          case (sum: java.lang.Integer, arg: java.lang.Long) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Float) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Double) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Integer) => (sum < arg).asInstanceOf[Object]

          case (sum: java.lang.Float, arg: java.lang.Long) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Float) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Double) => (sum < arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Integer) => (sum < arg).asInstanceOf[Object]

          case x => throw new InvalidTypeNumericalOperation(x)
        }
      case x => throw new InvalidNumberOfArguments("<", args.length)
    })),

    "<=" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case fst::snd::Nil =>
        (fst, snd) match {
          case (sum: java.lang.Long, arg: java.lang.Long) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Float) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Double) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Long, arg: java.lang.Integer) => (sum <= arg).asInstanceOf[Object]

          case (sum: java.lang.Double, arg: java.lang.Long) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Float) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Double) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Double, arg: java.lang.Integer) => (sum <= arg).asInstanceOf[Object]

          case (sum: java.lang.Integer, arg: java.lang.Long) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Float) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Double) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Integer, arg: java.lang.Integer) => (sum <= arg).asInstanceOf[Object]

          case (sum: java.lang.Float, arg: java.lang.Long) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Float) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Double) => (sum <= arg).asInstanceOf[Object]
          case (sum: java.lang.Float, arg: java.lang.Integer) => (sum <= arg).asInstanceOf[Object]

          case x => throw new InvalidTypeNumericalOperation(x)
        }
      case x => throw new InvalidNumberOfArguments("<=", args.length)
    })),

    "apply" -> Ref[Object](BuiltinProcedure((args, ctx) => args match {
      case List(p: Procedure, S.List(args)) => p.execute(args, ctx)
      case List(head, _) => throw new ApplyNonProcedure(head)
      case _ => throw new InvalidNumberOfArguments("apply", args.length)
    })),

    "display" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case arg::Nil =>
        System.out.println(if (arg == null) { "null" } else { arg.toString() })
        Void.value
      case _ => throw new InvalidNumberOfArguments("display", args.length)
    })),

    "null?" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case arg::Nil => S.Bool(arg == null)
      case _ => throw new InvalidNumberOfArguments("display", args.length)
    })),

    "raise" -> Ref[Object](BuiltinProcedure((args, _) => args match {
      case arg :: Nil => throw new Exception(arg.toString())
      case _ => throw new InvalidNumberOfArguments("raise", args.length)
    })),

    "debug" -> Ref[Object](BuiltinProcedure((args, ctx) => args match {
      case Nil =>
        ctx.debug.value = !ctx.debug.value
        Void.value
      case _ => throw new InvalidNumberOfArguments("debug", args.length)
    })),

    "try" -> Ref[Object](BuiltinProcedure((args, ctx) => args match {
      case List(fn: Procedure, catcher: Procedure) =>
        try {
          fn.execute(Nil, ctx)
        } catch {
          case exn: Throwable =>
            catcher.execute(List(exn), ctx)
        }
      case _ => throw new InvalidNumberOfArguments("try", args.length)
    }))
  )

}
