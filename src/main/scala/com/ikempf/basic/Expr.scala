package com.ikempf.basic

trait Expr

object Expr {

  case class Mul(l: Expr, r: Expr) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Num(n: Int)           extends Expr

  def eval(e: Expr): Int =
    e match {
      case Mul(l, r) ⇒ eval(l) * eval(r)
      case Add(l, r) ⇒ eval(l) + eval(r)
      case Num(n)    ⇒ n
    }

}
