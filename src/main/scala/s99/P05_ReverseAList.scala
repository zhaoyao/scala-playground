package s99

import scala.annotation.tailrec

/**
 * User: zhaoyao
 * Date: 4/15/14
 * Time: 20:44
 */
object P05_ReverseAList extends App {

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
