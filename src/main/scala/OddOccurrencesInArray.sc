//Solution #0 Complexity O(N)
object Solution0 {

  def solution(A: Array[Int]): Int = {
    A.foldLeft(Map[Int, Int]()) {
      case (result: Map[Int, Int], it: Int) => {
        result updated(it, result.getOrElse(it, 0) + 1)
      }
    }.collectFirst { case i if (i._2 % 2 == 1) => i._1 } match {
      case Some(i) => i
      case None => -1
    }
  }
}
//Solution #1 Complexity O(N**)
object Solution1 {

  def solution(A: Array[Int]): Int = {
    A.groupBy(identity).
      mapValues(_.size)
      .collectFirst { case i if (i._2 % 2 == 1) => i._1 } match {
      case Some(i) => i
      case None => -1
    }
  }
}