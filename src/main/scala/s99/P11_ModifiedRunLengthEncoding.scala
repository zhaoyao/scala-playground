package s99


/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 1:07
 */
object P11_ModifiedRunLengthEncoding extends App {
  import s99.P09_PackConsecutiveDuplicatesOfListElementsIntoSublists.pack

  def encode2(xs: List[Any]): List[Any] = {
    pack(xs).map(x => if (x.length==1) x(0) else (x(0), x.length))
  }

  def encodeModified2[A](ls: List[A]): List[Either[A, (A, Int)]] =
    pack(ls) map { t => if (t.length == 1) Left(t(0)) else Right((t(0), t.length)) }

  println(encode2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

}
