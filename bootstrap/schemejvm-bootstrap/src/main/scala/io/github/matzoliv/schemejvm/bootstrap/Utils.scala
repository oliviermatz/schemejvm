package io.github.matzoliv.schemejvm.bootstrap

import io.github.matzoliv.schemejvm.runtime.{Pair, Symbol}

import scala.annotation.tailrec

object S {
  object Symbol {
    def apply(s: String) = new Symbol(s)

    def unapply(o: Object) = o match {
      case s: Symbol => Some(s.getName())
      case _ => None
    }
  }

  object Cons {
    def apply(car: Object, cdr: Object) = new Pair(car, cdr)

    def unapply(o: Object) = o match {
      case p: Pair => Some((p.getCar(), p.getCdr()))
      case _ => None
    }
  }

  object List {
    def createWithTail(xs: List[Object], tail: Object) = {
      @tailrec
      def loop(xs: List[AnyRef], acc: Object): Object = xs match {
        case Nil => acc
        case x::xs => loop(xs, Cons(x, acc))
      }
      loop(xs.reverse, tail)
    }

    def apply(xs: Object*) = createWithTail(xs.toList, null)

    def unapply(o: Object) = {
      @tailrec
      def loop(o: Object, acc: List[Object]): Option[List[Object]] = o match {
        case Cons((car, cdr)) => loop(cdr, car::acc)
        case null => Some(acc.reverse)
        case _ => None
      }
      loop(o, Nil)
    }
  }

  object Int {
    def apply(n: Int) = new java.lang.Integer(n)
  }

  object Double {
    def apply(n: Double) = new java.lang.Double(n)
  }

  object Bool {
    def apply(n: Boolean) = new java.lang.Boolean(n)
  }
}

