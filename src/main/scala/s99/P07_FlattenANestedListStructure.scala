package s99

/**
 * User: zhaoyao
 * Date: 4/17/14
 * Time: 19:55
 */
object P07_FlattenANestedListStructure extends App {

  def flatten(xs: List[Any]): List[Any] = xs flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  def flatten2(xs: List[Any]): List[Any] = xs map {
    case ms: List[_] => flatten2(ms)
    case e => List(e)
  }

  println(flatten2(List(List(1, 1), 2, List(3, List(5, 8)))))
}
