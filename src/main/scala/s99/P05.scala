package s99

import scala.annotation.tailrec

/**
 * User: zhaoyao
 * Date: 4/15/14
 * Time: 20:44
 */
// P05 (*) Reverse a list.
//     Example:
//     scala> reverse(List(1, 1, 2, 3, 5, 8))
//     res0: List[Int] = List(8, 5, 3, 2, 1, 1)

object P05 extends App {

  def reverseBuiltin[A](xs: List[A]) = {
    xs.reverse
  }

  def reverseRecursive[A](xs: List[A]) = {
    @tailrec
    def r(cur: List[A], result: List[A]): List[A] = {
      cur match {
        case Nil => result
        case x :: tail => r(tail, x :: result)
      }
    }
    r(xs, Nil)
  }

  def reverseFunctional[A](xs: List[A]) = {
    xs.foldLeft(List[A]()) { ( l, x ) => x :: l }
  }

  println(reverseFunctional(List(1, 2, 3, 4, 5, 6)))



}
