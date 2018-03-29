package com.ikempf.fixedpoint

import scalaz.Functor

trait Tree[+A]

object Tree {

  case class Node[A](v: Int, l: A, r: A) extends Tree[A]
  case class Leaf[A](v: Int)             extends Tree[A]

  implicit val functor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A ⇒ B): Tree[B] =
      fa match {
        case Node(n, l, r) ⇒ Node(n, f(l), f(r))
        case Leaf(n)       ⇒ Leaf(n)
      }
  }

}
