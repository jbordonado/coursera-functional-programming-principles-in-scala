object mergesort {

  // 1st way
  def msort1(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y ) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort1(fst), msort1(snd))
    }
  }

  val nums = List(2, -4, 5, 7, 1)

  msort1(nums)

  // 2nd way : parameterized
  def msort2[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort2(fst)(lt), msort2(snd)(lt))
    }
  }

  val fruits = List("apple", "pineapple", "orange", "banana")

  msort2(nums)((x: Int, y: Int) => x < y)
  msort2(fruits)((x: String, y: String) => x.compareTo(y) < 0)

  // 3rd way : paameterized + ordering argument
  def msort3[T](xs: List[T])(ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort3(fst)(ord), msort3(snd)(ord))
    }
  }

  msort3(nums)(Ordering.Int)
  msort3(fruits)(Ordering.String)

  // 4th way (best one) : parameterized + implicit ordering argument (compiler figures out the ordering type to use)
  def msort4[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort4(fst)(ord), msort4(snd)(ord))
    }
  }

  msort4(nums)
  msort4(fruits)
}