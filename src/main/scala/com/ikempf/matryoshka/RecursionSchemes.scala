package com.ikempf.matryoshka

import com.ikempf.fixedpoint.Nat
import com.ikempf.fixedpoint.Nat.Succ
import com.ikempf.fixedpoint.Nat.Zero
import com.ikempf.matryoshka.RecursionSchemes.natToInt
import matryoshka.{Corecursive, Recursive}
import matryoshka.data.Fix
import matryoshka.implicits._

object RecursionSchemes {

  def natToInt[T](n: T)(implicit T: Recursive.Aux[T, Nat]): Int = n.cata[Int] {
    case Succ(x) ⇒ 1 + x
    case Zero    ⇒ 0
  }

}

object Main extends App {

  def nat[T](implicit T: Corecursive.Aux[T, Nat]): T =
    Succ(
      Succ(
        Succ(
          Zero.embed
        ).embed
      ).embed
    ).embed

  val natRes = natToInt(nat[Fix[Nat]])
  println(natRes)

}
