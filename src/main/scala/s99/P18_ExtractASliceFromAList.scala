package s99

/**
 * User: zhaoyao
 * Date: 4/23/14
 * Time: 0:34
 */
object P18_ExtractASliceFromAList extends App {

  def slice[A](i: Int, k: Int, xs: List[A]) = xs.slice(i, k)

  def sliceFunctional[A](i: Int, k: Int, xs: List[A]) = {
    xs drop i take (k - (i max 0))
  }

  def sliceRecursive[A](i: Int, k: Int, xs: List[A]): List[A] = {
    def sliceR(pos: Int, i: Int, k: Int, curr: List[A], result: List[A]): List[A] = {
      if (pos < i) {
        sliceR(pos + 1, i, k, curr.tail, result)
      } else if (pos < k) {
        sliceR(pos + 1, i, k, curr.tail, curr.head :: result)
      } else {
        result
      }
    }
    sliceR(i, i, k, xs.drop(i), Nil).reverse
  }

  println(sliceRecursive(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}
