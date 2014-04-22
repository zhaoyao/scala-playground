package s99

/**
 * User: zhaoyao
 * Date: 4/23/14
 * Time: 1:06
 */
// P22 (*) Create a list containing all integers within a given range.
//     Example:
//     scala> range(4, 9)
//     res0: List[Int] = List(4, 5, 6, 7, 8, 9)

object P22 extends App {

  def range(from: Int, to: Int): List[Int] = {
    def rangeR(i: Int, to: Int, result: List[Int]): List[Int] = {
      if (i == to) {
        (i :: result).reverse
      } else {
        rangeR(i+1, to, i :: result)
      }
    }
    rangeR(from, to, Nil)
  }

  def range2(from: Int, to: Int): List[Int] = {
    from.to(to).toList
  }

  println(range2(4, 9))

}
