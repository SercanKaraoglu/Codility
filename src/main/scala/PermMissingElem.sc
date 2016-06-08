object Solution {
  def solution(A: Array[Int]): Int = {
    val N: Long = A.length
    (((N*N+1)/2)-A.sum) toInt
  }
}