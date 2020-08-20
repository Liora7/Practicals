class ArrayQueue extends Queue[Int]{
  val MAX = 100 // max number of pieces of data
  val data = new Array[Int](MAX)
  var i = 0; var j = 0
  // Abs: data = {x0, x1, ... x(MAX-1)} where x0 was added first and x(MAX-1) was added last.
  // DTI = order of elems is preserved && no more than MAX elems in the queue at once

  /** Add x to the back of the queue */
  def enqueue(x: Int) ={
    if (j < MAX){
      data(j) = x
      j += 1
    }
    else {
      data(0) = x
      j = 1
    }
  }

  /** Remove and return the first element. */
  def dequeue: Int ={
    val x = data(i)
    i += 1
    x
  }

  /** Is the queue empty? */
  def isEmpty: Boolean ={
    i == j
  }

  /** Is the queue full? */
  def isFull : Boolean ={
    (j+1) % MAX == i
  }
}
