package s99

import scala.annotation.tailrec

/**
 * User: zhaoyao
 * Date: 4/15/14
 * Time: 1:12
 */
object P03_FindTheKthElementOfAList extends App {

  def nthBuiltin[A](n: Int, xs: List[A]) = {
    if (n >= 0) xs(n)
    else throw new NoSuchElementException
  }

  @tailrec
  def nthRecursive[A](n: Int, xs: List[A]): A = (n, xs) match {
      case (0, h :: _) => h
      case (_, Nil) => throw new NoSuchElementException
      case (c, _ :: rest) => nthRecursive(c-1, rest)
  }

  println(nthRecursive(2, List(1, 2, 3, 4)))

}
