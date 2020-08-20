class DoubleEndedQueue{
  //state: s : seq Int
  //Init: s = {}
  //Abs: head: {xi.datum | xi in L(head)} && tail = xN
  //DTI: L(head) is finite and tail points to the last node
  
  private var head = new DoubleEndedQueue.Node(0, null, null)
  private var tail = head

  /** Is the queue empty? */
  // Post: s = s0 and returns s == {}
  def isEmpty : Boolean = {
    head.next == null
  }

  /** add x to the start of the queue. */
  // Post: s = {x} ++ s0
  def addLeft(x: Int) = {
    val node = new DoubleEndedQueue.Node(x, head, head.next)
    if (head.next != null)
    head.next.prev = node
    head.next = node
    if (node.next == null) tail = node
  }

  /** get and remove element from the start of the queue. */
  // Pre: s = {y} ++ s0
  // Post: s = s0 and return y
  def getLeft : Int = {
    val y = head.next.datum
    if (tail == head.next) tail = head
    head.next = head.next.next
    y
  }

  /** add element to the end of the queue. */
  // Post: s = s0 ++ {x}
  def addRight(x: Int) = {
    val node = new DoubleEndedQueue.Node(x, tail, null)
    tail.next = node
    tail = node

  }

  /** get and remove element from the end of the queue. */
  // Pre: s = s0 ++ {y}
  // Post: s = s0 and return y
  def getRight : Int = {
    val y = tail.datum
    tail = tail.prev
    if (head == tail) head.next = null
    tail.next = null
    y
  }
}

object DoubleEndedQueue{
  private class Node(var datum: Int, var prev: Node, var next: Node)
}
