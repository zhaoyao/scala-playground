package s99

/**
 * User: zhaoyao
 * Date: 4/18/14
 * Time: 0:53
 */
// P09 (**) Pack consecutive duplicates of list elements into sublists.
//     If a list contains repeated elements they should be placed in separate
//     sublists.
//
//     Example:
//     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

object P09 extends App {

  def pack[A](xs: List[A]) = {
    def p(e: A, sub: List[A], cur: List[A], result: List[List[A]]): List[List[A]] = cur match {
      case Nil => sub :: result
      case h :: tail =>
        if(e == h) p(e, h :: sub, tail, result)
        else p(h, h::Nil, tail, sub :: result)
    }
    p(xs.head, xs.head::Nil, xs.tail, Nil).reverse
  }

  def pack2[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      println(packed)
      println(next)
      if (next == Nil) List(packed)
      else packed :: pack2(next)
    }
  }

  println(pack2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

}
