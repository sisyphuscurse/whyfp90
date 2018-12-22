package com.yiguan.raw

import Lists._

object Trees extends App {


  case class Node[T](label: T)(val subtrees: List[Node[T]])

  def foldtree[T, S](f: S => S => S)(g: S => T => S)(a: S)(node: Node[T]): S =
    g(foldtrees(f)(g)(a)(node.subtrees))(node.label)

  def foldtrees[T, S](f: S => S => S)(g: S => T => S)(a: S): List[Node[T]] => S = {
    case Nil => a
    case Cons(subtree, rest) => f(foldtree(f)(g)(a)(subtree))(foldtrees(f)(g)(a)(rest))
  }

  /*
                  1
                /    \
               /      \
              2        3
                        \
                         4
   */

  val intTree: Node[Int] = Node(1)(
    Cons(
      Node(2)(Nil),
      Cons(Node(3)(
        Cons(Node(4)(Nil), Nil)),
        Nil
      )))

  val add: (Int, Int) => Int = _ + _

  println(
    foldtree(add.curried)(add.curried)(0)(intTree)
  )
}
