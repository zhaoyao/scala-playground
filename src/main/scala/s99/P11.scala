package s99


/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 1:07
 */
// P11 (*) Modified run-length encoding.
//     Modify the result of problem P10 in such a way that if an element has no
//     duplicates it is simply copied into the result list.  Only elements with
//     duplicates are transferred as (N, E) terms.
//
//     Example:
//     scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

object P11 extends App {
  import s99.P09.pack

  def encode2(xs: List[Any]): List[Any] = {
    pack(xs).map(x => if (x.length==1) x(0) else (x(0), x.length))
  }

  def encodeModified2[A](ls: List[A]): List[Either[A, (A, Int)]] =
    pack(ls) map { t => if (t.length == 1) Left(t(0)) else Right((t(0), t.length)) }

  println(encode2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

}
