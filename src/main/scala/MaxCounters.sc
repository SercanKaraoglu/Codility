

object Solution0 {
  def solution(n: Int, a: Array[Int]): Array[Int] = {
    val res = (for {k <- a.indices} yield {
      k
    }).foldLeft((Vector.fill[Int](n)(0), 0, 0)) {
      case ((arr, max, lastValue), k) if 0 < a(k) && a(k) < n + 1 => {
        val index = a(k) - 1
        val ret = if (arr(index) < lastValue) arr.updated(index, lastValue + 1) else arr.updated(index, arr(index) + 1)
        (ret, Math.max(max, arr(index)), lastValue)
      }
      case ((arr, max, lastValue), k) if a(k) == n + 1 => (arr, lastValue, lastValue)
    }

    res._1.map{el => if (el < res._3) res._3 else el}.toArray
  }
}