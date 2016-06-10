import scala.collection.mutable

object Solution {
  def solution(A: Array[Int]): Int = {

    val perm = 1 to A.size + 1 to (mutable.TreeSet.newCanBuildFrom[Int])

    val value = A.foldLeft(perm) {
      case (res, it) =>
        res -= it
    }

    if (value.size > 0) value.firstKey else -1
  }
}