package s99

/**
 * User: zhaoyao
 * Date: 4/23/14
 * Time: 0:59
 */
// P20 (*) Remove the Kth element from a list.
//     Return the list and the removed element in a Tuple.  Elements are
//     numbered from 0.
//
//     Example:
//     scala> removeAt(1, List('a, 'b, 'c, 'd))
//     res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

object P20 extends App {

  def removeAt[A](i: Int, xs: List[A]): (List[A], A) = {
    ((xs take i) ::: (xs drop i + 1), xs(i))
  }

  println(removeAt(1, List('a, 'b, 'c, 'd)))

}
