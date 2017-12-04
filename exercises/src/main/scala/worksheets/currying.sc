object currying {

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  def fact(b: Int) =
    product((a: Int) => a)(1, b)

  def monoid(neutrEl: Int, binOp: (Int, Int) => Int)
            (f: Int => Int)
            (a: Int, b: Int): Int =
    if(a > b) neutrEl
    else binOp(f(a), monoid(neutrEl, binOp)(f)(a + 1, b))

  def fact2(a: Int) =
    monoid(1, (a: Int, b: Int) => a * b)((a: Int) => a)(1, a)

  def mapReduce(map: Int => Int, reduce: (Int, Int) => Int, zero: Int)
               (a: Int, b: Int): Int =
    if (a > b) zero
    else reduce(map(a), mapReduce(map, reduce, zero)(a + 1, b))

  def fact3(a: Int) =
    mapReduce((a: Int) => a, (a: Int, b: Int) => a * b, 1)(1, a)

  fact(10)
  fact2(10)
  fact3(10)
}