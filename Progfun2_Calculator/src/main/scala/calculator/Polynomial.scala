package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val delta = computeDelta(a, b, c)()
      val delimiter = 2.0 * a()
      delta match {
        case x if x > 0 =>
          val sqrtDelta = math.sqrt(delta)
          val first = (-b() + sqrtDelta) / delimiter
          val second = (-b() - sqrtDelta) / delimiter
          Set[Double](first, second)
        case 0 => Set[Double](-b() / delimiter)
        case _ => Set[Double]()
      }
    }
  }
}
