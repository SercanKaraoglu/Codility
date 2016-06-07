import Math._

object Solution {
  def solution(A: Array[Int]): Int = {
    (1 to A.size - 1) map (index => abs((A take (index) sum) - (A drop (index) sum))) min
  }
}