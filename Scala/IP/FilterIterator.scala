class FilterIterator[T] (test: T => Boolean, it: Iterator[T]) extends Iterator[T]{

    val iter = it filter test

    override def hasNext: Boolean = iter.nonEmpty

    override def next: T = iter.next


}
