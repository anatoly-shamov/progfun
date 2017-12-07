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

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

oddsList.init

def removeAt[T](n: Int, xs: List[T]): List[T] = {
  def loop(xs: List[T], n: Int, acc: List[T]): List[T] = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) acc ++ xs.tail
    else loop(xs.tail, n - 1, acc ++ List(xs.head))
  }
  loop(xs, n, List.empty)
}

def removeAt2[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

removeAt(3, oddsList)
removeAt(0, oddsList)
//removeAt(42, oddsList)

removeAt2(3, oddsList)
removeAt2(0, oddsList)
removeAt2(42, oddsList)

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case y :: ys => y match {
    case l: List[Any] => flatten(l) ++ flatten(ys)
    case a: Any => List(a) ++ flatten(ys)
  }
}

val deepList = List(List(1, 1), 2, List(3, List(5, 8)))
flatten(deepList)

def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
  case (Nil, y :: ys1) => ys
  case (x :: xs1, Nil) => xs
  case (x :: xs1, y :: ys1) => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
}

merge(List(1,3,5), List(2,6,7))

def squareListPattern(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => (y * y) :: squareListPattern(ys)
}

squareListPattern(oddsList)

def squareListMap(xs: List[Int]): List[Int] = xs map (y => y * y)

squareListMap(oddsList)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (l, r) = xs1 span ((y: T) => y == x)
    (x :: l) :: pack(r)
}

pack(List(1, 1, 1, 2, 2, 3, 4, 4, 4, 4))

def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (l, r) = xs1 span ((y: T) => y == x)
    (x, l.length + 1) :: encode(r)
}

encode(List(1, 1, 1, 2, 2, 3, 4, 4, 4, 4))

def encode2[T](xs: List[T]): List[(T, Int)] = pack(xs) map (ys => (ys.head, ys.length))

encode2(List(1, 1, 1, 2, 2, 3, 4, 4, 4, 4))
