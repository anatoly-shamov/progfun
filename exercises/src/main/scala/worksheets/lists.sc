def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (y > x) x :: y :: ys else y :: insert(x, ys)
}

val oddsList = List(1, 3, 5, 7, 9, 11)
insert(4, oddsList)
insert(40, oddsList)
insert(-1, oddsList)
insert(1, List())

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

val unsortedList = List(11, 5, 1, 7, 3, 9)
isort(unsortedList)