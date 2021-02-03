import java.util.Comparator

object sorting extends App {
  def mergeSort[A](list: List[A])(comparator: Comparator[A]): List[A] = {
    def insertIntoList(element: A, list: List[A]): List[A] =
      if (list.isEmpty) List(element)
      else if (comparator.compare(list.head, element) < 0) element :: list
      else list.head :: insertIntoList(element, list.tail)

    def mergeOrderedArrays(list1: List[A], list2: List[A]): List[A] =
      if (list1.isEmpty) list2
      else mergeOrderedArrays(list1.tail, insertIntoList(list1.head, list2))

    def mergeSortAux(list1: List[A], list2: List[A]): List[A] =
      if (list2.isEmpty) List()
      else if (list1.isEmpty) list2
      else mergeOrderedArrays(
          mergeSortAux(list1.slice(0, list1.length / 2), list1.slice(list1.length / 2, list1.length)),
          mergeSortAux(list2.slice(0, list2.length / 2), list2.slice(list2.length / 2, list2.length))
        )
    mergeSortAux(list.slice(0, list.length / 2), list.slice(list.length / 2, list.length))
  }

  def binarySearch[A <: Int](element: A, list: List[A]): Boolean = {
    if (list.isEmpty) false
    else if (list(list.length / 2) == element) true
    else if (list(list.length / 2) < element) binarySearch(element, list.slice(list.length / 2 + 1, list.length))
    else if (list(list.length / 2) > element) binarySearch(element, list.slice(0 ,list.length / 2))
    else false
  }
  val testList1: List[Int] = List(8,3,5,1,0,2,4,9)
  println(mergeSort(testList1)((x: Int,y: Int) => if (x < y) 1 else -1))
  val testList2: List[Int] = List(8,3,5)
  println(mergeSort(testList2)((x: Int,y: Int) => if (x < y) 1 else -1))
  val testList3: List[Int] = List(8,5)
  println(mergeSort(testList3)((x: Int,y: Int) => if (x < y) 1 else -1))
  val testList4: List[Int] = List(8)
  println(mergeSort(testList4)((x: Int,y: Int) => if (x < y) 1 else -1))
  val testList5: List[Int] = List()
  println(mergeSort(testList5)((x: Int,y: Int) => if (x < y) 1 else -1))

  println(binarySearch(2, mergeSort(testList1)((x: Int,y: Int) => if (x < y) 1 else -1)))
  println(binarySearch(6, mergeSort(testList1)((x: Int,y: Int) => if (x < y) 1 else -1)))


}
