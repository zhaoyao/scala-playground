package s99

/**
 * User: zhaoyao
 * Date: 4/17/14
 * Time: 19:54
 */
object P06_FindOutWhetherAListIsAPalindrome {

  def palindrome[A](xs: List[A]) = xs == xs.reverse

}
