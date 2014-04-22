package s99

/**
 * User: zhaoyao
 * Date: 4/17/14
 * Time: 19:54
 */
// P06 (*) Find out whether a list is a palindrome.
//     Example:
//     scala> isPalindrome(List(1, 2, 3, 2, 1))
//     res0: Boolean = true

object P06 {

  def palindrome[A](xs: List[A]) = xs == xs.reverse

}
