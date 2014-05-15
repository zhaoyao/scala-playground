package s99

import s99.P23.randomSelect

/**
 * User: zhaoyao
 * Date: 4/23/14
 * Time: 23:51
 */
// P25 (*) Generate a random permutation of the elements of a list.
//     Hint: Use the solution of problem P23.
//
//     Example:
//     scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
//     res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)

object P25 extends App {

  def randomPermute[A](xs: List[A]): List[A] = {
    randomSelect(xs.length, xs)
  }

  println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))

}
