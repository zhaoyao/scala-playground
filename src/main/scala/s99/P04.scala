package s99

import scala.annotation.tailrec

/**
 * User: zhaoyao
 * Date: 4/15/14
 * Time: 1:22
 */
// P04 (*) Find the number of elements of a list.
//     Example:
//     scala> length(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 6

object P04 extends App {

  def sizeBuiltin(xs: List[Any]): Int = {
    xs.size
  }

  def sizeRecursive(xs: List[Any]): Int = {
    @tailrec
    def s(c: Int, ls: List[Any]): Int = ls match {
      case Nil => c
      case _ :: rest => s(c+1, rest)
    }
    s(0, xs)
  }

  def sizeF(xs: List[Any]) = xs.foldRight(0) { (_, c) => c + 1 }

  println(sizeRecursive(List(1, 2, 3, 4)))
  println(sizeF(List(1, 2, 3, 4)))
  println(sizeRecursive(Nil))

}
