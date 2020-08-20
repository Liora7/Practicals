class IntQueue extends Queue[Int]{
  // Abs: head: {xi.elem | xi in L(head)}
  // DTI: L(head) is finite and for all 0<=i xi was added before x(i+1)

  private var head = new IntQueue.Node(0, null)
  private var end = head

  /** Add x to the back of the queue */
  def enqueue(x: Int) ={
    val node = new IntQueue.Node(x, null)
    end.next = node
    end = node
  }

  /** Remove and return the first element. */
  def dequeue: Int ={
    assert(head.next != null)
    val x = head.next.elem
    head.next = head.next.next
    x
  }

  /** Is the queue empty? */
  def isEmpty: Boolean ={
    head.next == null
  }
}


object IntQueue{
  private class Node(var elem: Int, var next:Node)
}
