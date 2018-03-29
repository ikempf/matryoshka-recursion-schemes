package com.ikempf.basic

trait Nat

object Nat {
  case class Succ(n: Nat) extends Nat
  case object Zero        extends Nat

  def eval(n: Nat): Int =
    n match {
      case Succ(n) ⇒ 1 + eval(n)
      case Zero    ⇒ 0
    }
}
