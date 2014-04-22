package s99

/**
 * User: zhaoyao
 * Date: 4/23/14
 * Time: 1:03
 */
// P21 (*) Insert an element at a given position into a list.
//     Example:
//     scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
//     res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

object P21 extends App {

  def insertAt[A](e: A, i: Int, xs: List[A]): List[A] = {
    val (pre, post) = xs.splitAt(i)
    pre ::: (e :: post)
  }

  println(insertAt('new, 1, List('a, 'b, 'c, 'd)))

}
