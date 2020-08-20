/** A queue of data of type A.
* state: q : seq A
* init: q = [] */
trait Queue[A]{

  /** Add x to the back of the queue
  * post: q = q0 ++ [x] */
  def enqueue(x: A)

  /** Remove and return the first element.
  * pre: q 6= []
  * post: q = tail q0 ∧ returns head q0
  * or post: returns x s.t. q0 = [x] ++ q */
  def dequeue: A

  /** Is the queue empty?
  * post: q = q0 ∧ returns q = [] */
  def isEmpty: Boolean
}
