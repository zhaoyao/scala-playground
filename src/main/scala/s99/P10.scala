package s99

import scala.annotation.tailrec

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 0:38
 */
// P10 (*) Run-length encoding of a list.
//     Use the result of problem P09 to implement the so-called run-length
//     encoding data compression method.  Consecutive duplicates of elements are
//     encoded as tuples (N, E) where N is the number of duplicates of the
//     element E.
//
//     Example:
//     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

object P10 extends App {

  import P09.pack

  def encode[A](xs: List[A]): List[(A, Int)] = {
    @tailrec
    def enc(e: A, c: Int, cur: List[A], result: List[(A, Int)]): List[(A, Int)] = {
      cur match {
        case Nil => (e, c) :: result
        case h :: tail =>
          if (e == h) enc(e, c + 1, tail, result)
          else enc(h, 1, tail, (e, c) :: result)
      }
    }
    enc(xs.head, 1, xs.tail, Nil).reverse
  }

  def encode2[A](xs: List[A]): List[(A, Int)] = {
    pack(xs).map(x => (x(0), x.length))
  }

  println(encode2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

}
