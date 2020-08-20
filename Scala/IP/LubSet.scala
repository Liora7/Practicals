class LubSet[T](elements: Set[T]) extends PartialOrder[MySet[T]] {

  def <=(that: MySet[T]): Boolean = {
    for (elem <- elements) if (! that.contains(elem)) return false
    true
  }

  def lub(that: MySet[T]): MySet[T] = {
    if (this <= that) return new MySet(elements)
    else {
      var lub = new MySet(that)
      for (elem <- elements) lub = lub + elem
      lub
    }
  }
}
