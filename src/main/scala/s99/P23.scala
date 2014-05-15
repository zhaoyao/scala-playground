package s99

/**
 * User: zhaoyao
 * Date: 4/23/14
 * Time: 23:43
 */
// P23 (**) Extract a given number of randomly selected elements from a list.
//     Example:
//     scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
//     res0: List[Symbol] = List('e, 'd, 'a)
//
//     Hint: Use the answer to P20.

object P23 extends App {

  import P20.removeAt

  def randomSelect[A](n: Int, xs: List[A]): List[A] = {
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt(new util.Random().nextInt(xs.length), xs)
      e :: randomSelect(n-1, rest)
    }
  }

  println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
}
