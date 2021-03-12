case object BubbleSort extends App {

  /**
   * Sealed trait for implementing the comparasion across different types
   * @tparam T
   */
  sealed trait Comparable[T]{
    def compareTo(a:T,b:T):Int
  }

  /**
   * Implicit definition for comparing String
   */
  implicit val comparableString: Comparable[String] = new Comparable[String]{
    override def compareTo(a: String, b: String): Int = a.compareTo(b)
  }
  /**
   * Implicit definition for comparing int
   */
  implicit val comparableInt: Comparable[Int] = new Comparable[Int]{
    override def compareTo(a: Int, b: Int): Int = a.compareTo(b)
  }
  /*
  One can add more implicit definitions for comparing other type of values, such as Double.
   */

  /**
   * Recursively check if the list is sorted.
   * @param list
   * @param comparable
   * @tparam A
   * @return true, if the list is sorted, otherwise false
   */
  def isSorted[A](list:List[A])(implicit comparable:Comparable[A]): Boolean = list match{
    case Nil => false
    case x::Nil => true
    case x::xs => comparable.compareTo(x,xs.head) != 1 && isSorted(xs)
  }

  /**
   * Recursively swap the elements in the list.
   * If the element is bigger than the predecessor than swap it.
   * Otherwise, do not swap it
   * @param list
   * @param comparable
   * @tparam A
   * @return List of A
   */
  def swap[A] (list:List[A])(implicit comparable: Comparable[A]): List[A] = list match{
    case Nil => Nil
    case x::Nil => x :: Nil
    case x::y::xs => if (comparable.compareTo(x,y) != - 1) y :: swap(x :: xs) else x :: swap(y :: xs)
  }

  /**
   * Recursively sort the list.
   * The worst-time and best-time complexity is O(n^2)
   * @param list
   * @param comparable
   * @tparam A
   * @return sorted list of A's
   */
  def bubbleSort[A](list:List[A])(implicit comparable: Comparable[A]): List[A] = list match {
    case Nil => Nil
    case x :: Nil => if (!isSorted(list)) bubbleSort(swap(list)) else list
    case x :: xs => if (!isSorted(list)) bubbleSort(swap(list)) else list
  }

}
