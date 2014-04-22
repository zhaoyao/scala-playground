package s99

import scala.annotation.tailrec

/**
 * User: zhaoyao
 * Date: 4/15/14
 * Time: 0:46
 */
// P02 (*) Find the last but one element of a list.
//     Example:
//     scala> penultimate(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 5

object P02 extends App {

  def penultimateRecursive[A](xs: List[A]): A = xs match {
    case x :: _ :: Nil => x
    case _ :: rest => penultimateRecursive(rest)
    case _ => throw new NoSuchElementException
  }

  def penultimateBuiltin[A](xs: List[A]) = {
    if (xs.isEmpty) throw new NoSuchElementException
    xs.init.last
  }

  def lastNthBuiltin[A](n: Int, ls: List[A]): A = {
    if (n <= 0) throw new IllegalArgumentException
    if (ls.length < n) throw new NoSuchElementException
    ls.takeRight(n).head
  }

  // len = 7  n = 2    xs(-3) = xs(4) = xs(7 - 3)
  //[A, B, C, D, E, F, G]
  def lastNthRecursive[A](n: Int, xs: List[A]): A = {
    @tailrec
    def lastNth(c: Int, resultList: List[A], curList: List[A]): A = {
      curList match {
        case Nil if c > 0 => throw new NoSuchElementException
        case Nil => resultList.head
        case _ :: rest =>
          lastNth(c - 1, if (c > 0) {println(c); resultList} else resultList.tail, rest)
      }
    }
    if (n <= 0) throw new NoSuchElementException
    else lastNth(n, xs, xs)
  }

  println(lastNthRecursive(2, List(1, 2, 3, 4, 5, 6, 7)))

}
