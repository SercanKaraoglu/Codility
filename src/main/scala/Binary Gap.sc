import scala.annotation.tailrec

object Solution {
  def toBinary(n: Int): Stream[Int] = {
    @tailrec def binary(acc: Stream[Int], n: Int): Stream[Int] = {
      n match {
        case 0 | 1 => n #:: acc
        case _ => binary((n % 2) #:: acc, (n / 2))
      }
    }
    binary(Stream(), n)
  }
  case class State(var max: Int, var count: Int)
  def solution(N: Int): Int = {
    toBinary(N).foldLeft(State(0, -1)) {
      case (state: State, it: Int) => {
        if (0 == it && state.count >= 0) {
          state.count = state.count + 1
        }
        if (1 == it) {
          state.max = if (state.count > state.max) state.count else state.max;
          state.count = 0
        }
        state
      }
    }.max
  }
}