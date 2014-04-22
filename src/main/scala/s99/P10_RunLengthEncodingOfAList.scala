package s99

import scala.annotation.tailrec

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 0:38
 */
object P10_RunLengthEncodingOfAList extends App {

  import P09_PackConsecutiveDuplicatesOfListElementsIntoSublists.pack

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
