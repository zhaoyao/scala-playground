package s99

/**
 * User: zhaoyao
 * Date: 4/23/14
 * Time: 0:45
 */
// P19 (**) Rotate a list N places to the left.
//     Examples:
//     scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
//
//     scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
object P19 extends App {

  def rotate[A](n: Int, xs: List[A]): List[A] = {
    val i = if (n >= 0) n else xs.length + n
    (xs drop i) ::: (xs take i)
  }

  println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}
