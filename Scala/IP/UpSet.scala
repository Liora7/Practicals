class  UpSet[T <: PartialOrder[T]](s: Set[T]) {

  val minElems: Set[T] = s.filter( x => s.forall( y => y <= x))

  def contains(x: T): Boolean = {
      minElems.exists(_ <= x)
  }

  def intersection(that: UpSet[T]): UpSet[T] = {
      var res = new MySet[T]()
      var s1 = minElems.iterator
      var s2 = that.minElems.iterator
      for (y <- s1) if (that.contains(y)) res = res + y
      for (x <- s2) if (contains(x)) res = res + x
      new UpSet(res)
  }

  def <=(that : UpSet[T]): Boolean = {
    var iter = minElems.iterator
    for (x <- iter) if (! that.contains(x)) return false
    true
  }

  def lub(that: UpSet[T]): UpSet[T] = {
    var res = new MySet[T]()
    var s1 = minElems.iterator
    var s2 = that.minElems.iterator
    for (y <- s1) res = res + y
    for (x <- s2) res = res + x
    new UpSet(res)
  }
}
