package com.ikempf.fixedpoint

import scalaz.Functor

trait Expr[+A]

object Expr {

  case class Mul[A](l: A, r: A) extends Expr[A]
  case class Add[A](l: A, r: A) extends Expr[A]
  case class Num[A](n: Int)     extends Expr[Nothing]

  implicit val functor: Functor[Expr] = new Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: A ⇒ B): Expr[B] =
      fa match {
        case Mul(l, r) ⇒ Mul(f(l), f(r))
        case Add(l, r) ⇒ Add(f(l), f(r))
        case Num(n)    ⇒ Num(n)
      }
  }

}
