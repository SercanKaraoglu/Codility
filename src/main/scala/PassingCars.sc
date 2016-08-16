object Solution {
  def solution(a: Array[Int]): Int = {
    a.foldLeft(0, 0) {
      case ((inc, res), it) if res > 1000000000 || res == -1 => (inc, -1)
      case ((inc, res), it) => if (0 == it) (inc + 1, res) else (inc, res + inc)
    }._2
  }
}