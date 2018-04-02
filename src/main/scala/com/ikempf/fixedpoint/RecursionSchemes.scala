package com.ikempf.fixedpoint

import com.ikempf.fixedpoint.Expr.Add
import com.ikempf.fixedpoint.Expr.Mul
import com.ikempf.fixedpoint.Expr.Num
import com.ikempf.fixedpoint.Nat.Succ
import com.ikempf.fixedpoint.Nat.Zero
import com.ikempf.fixedpoint.RecursionSchemes.Fix
import com.ikempf.fixedpoint.Tree.Leaf
import com.ikempf.fixedpoint.Tree.Node
import scalaz.Functor
import scalaz.syntax.functor._

import scala.language.higherKinds

object RecursionSchemes {

  case class Fix[F[_]](unfix: F[Fix[F]])

  //  def cata[F[_]: Functor, T, A](structure: F[T])(algebra: F[A] ⇒ A): A =
  //    algebra(structure.map(cata(_)(algebra)))

  def cata[F[_]: Functor, B](struct: Fix[F])(algebra: F[B] ⇒ B): B =
    algebra(struct.unfix.map(cata(_)(algebra)))

  def natToInt(nat: Fix[Nat]): Int = cata[Nat, Int](nat) {
    case Succ(x) ⇒ 1 + x
    case Zero    ⇒ 0
  }

  def natToString(nat: Fix[Nat]): String = cata[Nat, String](nat) {
    case Succ(x) ⇒ "Succ" + x
    case Zero    ⇒ "Zero"
  }

  def exprToInt(expr: Fix[Expr]): Int = cata[Expr, Int](expr) {
    case Mul(l, r) ⇒ l * r
    case Add(l, r) ⇒ l * r
    case Num(n)    ⇒ n
  }

  def treeToIn(n: Fix[Tree]): Int = cata[Tree, Int](n) {
    case Node(tree, l, r) ⇒ tree + l + r
    case Leaf(n)          ⇒ n
  }

}

object Main extends App {

  val nat: Fix[Nat] =
    Fix(Succ(Fix(Succ(Fix(Succ(Fix[Nat](Zero)))))))

  println(RecursionSchemes.natToInt(nat))
  println(RecursionSchemes.natToString(nat))

}
