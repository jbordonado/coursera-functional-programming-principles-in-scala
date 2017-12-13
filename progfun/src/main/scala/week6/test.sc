object test {
  val xs = Array(1, 2, 3, 44)
  xs map (x => x * 2)

  val s = "Hello World"
  s filter (c => c.isUpper)
  s exists (c => c.isUpper)
  s forall (c => c.isUpper)

  val pairs = List(1, 2, 3) zip s
  pairs unzip

  s flatMap (c => List('.', c))

  val M = 5
  val N = 3
  (1 to M) flatMap (x => (1 to N) map (y => (x,y)))

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]) : Double = {
    (xs zip ys).map(xy => xy._1 * xy._2).sum
  }

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]) : Double = {
    (xs zip ys).map { case (x, y) => x * y}.sum
  }
}