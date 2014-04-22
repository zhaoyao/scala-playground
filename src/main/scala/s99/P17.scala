package s99

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 19:13
 */
// P17 (*) Split a list into two parts.
//     The length of the first part is given.  Use a Tuple for your result.
//
//     Example:
//     scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

object P17 extends App {

  def split[A](n: Int, xs: List[A]) = (xs.take(n), xs.drop(n))

  def splitRecursive[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    (n, xs) match {
      case (_, Nil) => (Nil, Nil)
      case (0, list) => (Nil, list)
      case (n, h :: tail) => {
        val (pre, post) = splitRecursive(n - 1, tail)
        (h :: pre, post)
      }
    }
  }


  println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

}
