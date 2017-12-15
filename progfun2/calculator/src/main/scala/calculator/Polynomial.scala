package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val sqrt = Signal(math.sqrt(delta()))
    Signal(
      for {
        sqrt <- Set(sqrt(), -sqrt())
      } yield (-b() + sqrt) / (2 * a())
    )
  }
}
