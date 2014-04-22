package s99

import scala.annotation.tailrec

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 0:13
 */
// P08 (**) Eliminate consecutive duplicates of list elements.
//     If a list contains repeated elements they should be replaced with a
//     single copy of the element.  The order of the elements should not be
//     changed.
//
//     Example:
//     scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

object P08 extends App {

  def compressMy[A](xs: List[A]) = {
    def f(prevElt: A, list: List[A], result: List[A]): List[A] =
      if (list.isEmpty)
        result.reverse
      else if (prevElt == list.head)
        f(list.head, list.tail, result)
      else
        f(list.head, list.tail, list.head :: result)
    f(xs.head, xs.tail, xs.head :: Nil)
  }

  def compressRecursive[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }

  def compressTailRecursive[A](xs: List[A]): List[A] = {
    def compressR(result: List[A], curr: List[A]): List[A] = curr match {
      case h :: tail => compressR(h :: result, curr.dropWhile(_ == h))
      case Nil => result.reverse
    }
    compressR(Nil, xs)
  }

  def compressFunctional[A](ls: List[A]): List[A] =
      ls.foldRight(List[A]()) { (h, r) =>
        if (r.isEmpty || r.head != h) h :: r
        else r
    }

  println(compressTailRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

}
