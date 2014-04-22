package s99

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 19:06
 */
object P16_DropEveryNthElementFromAList extends App {

  def drop[A](n: Int, xs: List[A]) = {
    xs.foldRight((List[A](), 1)) {
      (x, n) => if (n._2 % 3 != 0) (x :: n._1, n._2 + 1) else (n._1, n._2 + 1)
    }._1
  }

  def drop2[A](n: Int, xs: List[A]) = {
    xs.zipWithIndex.filter{ v => (v._2 +1) % n != 0 }.map(_._1)
  }

  println(drop2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}
