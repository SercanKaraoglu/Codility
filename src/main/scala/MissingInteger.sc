import scala.collection.mutable

val N = Array(-1)

val ordering = Ordering.fromLessThan[Int](_ < _)
val perm = mutable.TreeSet.empty(ordering) ++ (1 to N.size toSet)

N.foldLeft(perm) {
  case (res, it) =>
    perm -= it
}.toList match {
  case x :: xs => x
  case x :: Nil => x
  case Nil => -1
}