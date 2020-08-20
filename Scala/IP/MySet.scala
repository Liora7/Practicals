class MySet[T](elements: Set[T]) extends Set[T]{

  def contains(key: T): Boolean = {
    elements.contains(key)
  }

  def iterator: Iterator[T] = {
    elements.iterator
  }

  def +(elem: T) = {
    new MySet(elements + elem)
  }

  def -(elem: T) = {
    new MySet(elements - elem)
  }

  override def empty = {
    new MySet[T](elements.empty)
  }
}
