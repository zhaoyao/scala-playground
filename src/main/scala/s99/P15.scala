package s99

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 19:01
 */
// P15 (**) Duplicate the elements of a list a given number of times.
//     Example:
//     scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
//     res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
object P15 extends App {

  def duplicate[A](xs: List[A]) = xs flatMap { e => List(e, e) }

  def duplicateN[A](n: Int, xs: List[A]) = xs flatMap { e => List.fill(n)(e) }


  println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))

}
