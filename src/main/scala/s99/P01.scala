package s99



// P01 (*) Find the last element of a list.
//     Example:
//     scala> last(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 8

// The start of the definition of last should be
//     def last[A](l: List[A]): A = ...
// The `[A]` allows us to handle lists of any type.

object P01 {

  object :+ {
    def unapply[A](xs: List[A]): Option[(List[A], A)] = {
      if (xs.isEmpty)
        None
      else
        Some(xs.init, xs.last)
    }
  }

  def lastUnapply[A](xs: List[A]): A = {
    xs match {
      case _ :+ last => last
      case Nil => throw new NoSuchElementException
    }
  }

  def lastRecursive[A](xs: List[A]): A = {
    xs match {
      case last :: Nil => last
      case _ :: rest => lastRecursive(rest)
      case Nil => throw new NoSuchElementException
    }
  }


}
