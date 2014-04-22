package s99

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 19:13
 */
object P17_SplitAListIntoTwoParts extends App {

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
