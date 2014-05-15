package s99

/**
 * User: zhaoyao
 * Date: 4/23/14
 * Time: 23:48
 */
// P24 (*) Lotto: Draw N different random numbers from the set 1..M.
//     Example:
//     scala> lotto(6, 49)
//     res0: List[Int] = List(23, 1, 17, 33, 21, 37)

object P24 extends App {

  import P23.randomSelect

  def lotto(n: Int, limit: Int): List[Int] = {
    randomSelect(n, List.range(1, limit+1))
  }

  println(lotto(6, 49))

}
