object Solution {
  def solution(X: Int, Y: Int, D: Int): Int = {
    val diff = Y-X
    diff/D + (if (diff % D == 0) 0 else 1)
  }
}