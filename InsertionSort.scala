import scala.annotation.tailrec

case object InsertionSort extends App {
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
   * Given a Sorted List list and element a, this function inserts the element a in appropriate position
   * while keeping the list sorted.
   * @param list
   * @param a
   * @param inserted
   * @param comparable
   * @tparam A
   * @return list with element a inserted
   */
  def insert[A] (list:List[A])(a:A)(inserted:Boolean)(implicit comparable: Comparable[A]):List[A] = (list,a,inserted) match{
    case (Nil,Nil,false) => Nil
    case (_,Nil,false) => list
    case (x::Nil,_,bool) => if(bool) list
                            else if(comparable.compareTo(a,x) == 1) x::a::Nil
                            else if(comparable.compareTo(a,x) == -1) a :: x :: Nil
                            else x :: Nil
    case (x::xs,_,bool) => if(comparable.compareTo(a,x) == -1 && !bool)  a :: x :: insert(xs)(a)(inserted = true)
                           else if (bool) x :: insert(xs)(a)(inserted = true)
                           else x :: insert(xs)(a)(inserted = false)
  }

  /**
   * Performs the insertion sort given list.
   * @param list
   * @param comparable
   * @tparam A
   * @return sorted list
   */
  def insertion[A](list:List[A])(implicit comparable: Comparable[A]) = {
    /**
     * Accumulate the elements in sorted list, and use insert function to insert the element to sortedList without
     * changing the order.
     * @param list
     * @param sortedList
     * @param comparable
     * @tparam A
     * @return
     */
    def insertionSort[A](list: List[A])(sortedList: List[A])(implicit comparable: Comparable[A]): List[A] = (list, sortedList) match {
      case(Nil,_) => Nil
      case(x::Nil,b) => insert(b)(x)(inserted = false)
      case(x::xs,b) =>  insertionSort(xs)(insert(b)(x)(inserted = false))

    }
    insertionSort(list)(list.head :: Nil)
  }



}
