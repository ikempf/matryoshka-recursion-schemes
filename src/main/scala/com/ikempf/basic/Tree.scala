package com.ikempf.basic

trait Tree

object Tree {

  case class Node(v: Int, l: Tree, r: Tree) extends Tree
  case class Leaf(v: Int)                   extends Tree

  def eval(e: Tree): Int =
    e match {
      case Node(n, l, r) ⇒ n + eval(l) + eval(r)
      case Leaf(n)       ⇒ n
    }

}
