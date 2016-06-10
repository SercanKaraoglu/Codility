object Solution0 {
  def solution(N: Int, A: Array[Int]): Array[Int] = {
    (for {k <- 0 to A.size - 1} yield {
      k
    }).foldLeft(Array.ofDim[Int](N)) {
      case (a, k) if 1 <= A(k) && A(k) <= N => a.updated(A(k) - 1, a(A(k) - 1) + 1)
      case (a, k) if A(k) == N + 1 => val max = a.max; a.transform(_ => max); a;
    }
  }
}

