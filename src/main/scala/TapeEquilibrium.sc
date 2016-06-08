import Math._
//Solution #0 complexity O(N**)
object Solution0 {
  def solution(A: Array[Int]): Int = {
    (1 to A.size - 1) map (index => abs((A take (index) sum) - (A drop (index) sum))) min
  }
}

//Solution #1 complexity O(N)
object Solution1 {
  def solution(A: Array[Int]): Int = {
    lazy val x=A.scanLeft(0)(_ + _).slice(1, A.size)

    lazy val y=A.scanRight(0)(_ + _).slice(1, A.size)

    x zip(y) map(p=>Math.abs(p._1-p._2)) min

  }
}