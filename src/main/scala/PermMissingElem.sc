object Solution {
  def solution(A: Array[Int]): Int = {
    val N: Long = A.length+1
    (((N*(N+1))/2)-A.sum) toInt
  }
}