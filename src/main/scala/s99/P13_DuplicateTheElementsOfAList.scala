package s99

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 19:01
 */
object P13_DuplicateTheElementsOfAList extends App {

  def duplicate[A](xs: List[A]) = xs flatMap { e => List(e, e) }

  def duplicateN[A](n: Int, xs: List[A]) = xs flatMap { e => List.fill(n)(e) }


  println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))

}
