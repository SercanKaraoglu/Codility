object Solution {
  def solution(A: Array[Int], K: Int): Array[Int] = {
    if (null == A || A.size == 0) A else (A drop A.size - (K % A.size)) ++ (A take A.size - (K % A.size))
  }
}