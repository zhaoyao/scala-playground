package s99


object :+ {
  def unapply[A](xs: List[A]): Option[(List[A], A)] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.init, xs.last)
  }
}

object FindTheLastElementOfAList {

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
