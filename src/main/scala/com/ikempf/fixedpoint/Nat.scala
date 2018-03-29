package com.ikempf.fixedpoint

import scalaz.Functor

trait Nat[+A]

object Nat {
  case class Succ[A](n: A) extends Nat[A]
  case object Zero         extends Nat[Nothing]

  implicit val functor: Functor[Nat] = new Functor[Nat] {
    override def map[A, B](fa: Nat[A])(f: A ⇒ B): Nat[B] =
      fa match {
        case Succ(a) ⇒ Succ(f(a))
        case Zero    ⇒ Zero
      }
  }

}
