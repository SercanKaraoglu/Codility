import scala.collection.mutable

object Solution {
  def solution(X: Int, A: Array[Int]): Int = {
    val path = mutable.HashSet() ++ (1 to X toSet)
    val res = A.iterator.takeWhile { _ => path.nonEmpty }.foldLeft(0) {
      case (res, it) =>
        path -= it
        res + 1
    }
    if (path.nonEmpty && res >= 1) -1 else res - 1
  }
}