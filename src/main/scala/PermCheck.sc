object Solution {
  def solution(A: Array[Int]): Int = {
    val Perm=1 to A.size toArray;
    if(A.diff(Perm).size==0) 1 else 0
  }
}