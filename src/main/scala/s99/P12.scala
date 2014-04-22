package s99

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 18:50
 */
// P12 (**) Decode a run-length encoded list.
//     Given a run-length code list generated as specified in problem P10,
//     construct its uncompressed version.
//
//     Example:
//     scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
//     res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

object P12 extends App {

  def decode[A](xs: List[(Int, A)]) = {
    def decodeR(t: (Int, A), result: List[A], list: List[(Int, A)]): List[A] = {
      (t, list) match {
        case ((0, _), Nil) => result
        case ((0, _), _) => decodeR(list.head, result, list.tail)
        case ((n, x), _) => decodeR((n-1, x), x :: result, list)
      }
    }
    decodeR(xs.head, Nil, xs.tail).reverse
  }

  def decodeBuiltin[A](xs: List[(Int, A)]) = xs flatMap { e => List.fill(e._1)(e._2) }

  println(decodeBuiltin(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
}
