// Representing the phone book using a linked list with a dummy header

class LinkedListHeaderBook extends Book{
  private var list = new LinkedListHeaderBook.Node("?", "?", null)
  // list represents the mapping composed of (n.name -> n.number)
  // maplets, when n is a node reached by following 1 or more
  // next references.

  /** Return the node before the one containing name.
    * Post: book = book_0 && returns n s.t. n in L(list) &&
    * (n.next.name=name or n.next=null if no such Node exists)*/
  private def find(name:String) : LinkedListHeaderBook.Node = {
    var n = list
    // Invariant: name does not appear in the nodes up to and
    // including n; i.e.,
    // for all n1 in L(list.next, n.next), n1.name != name
    while(n.next != null && n.next.name != name) n = n.next
    n
  }

  /** Is name in the book? */
  def isInBook(name: String): Boolean = find(name).next != null

  /** Return the number stored against name */
  def recall(name: String) : String = {
    val n = find(name); assert(n.next != null); n.next.number
  }

  /** Add the maplet name -> number to the mapping */
  def store(name: String, number: String) = {
    val n = find(name)
    if(n.next == null){ // store new info at end of list
      list = revlst(list)
      list = new LinkedListHeaderBook.Node(name, number, list)
      list = revlst(list)
    }
    else n.next.number = number
  }

  private def revlst(list: LinkedListHeaderBook.Node)={
    var (prev: LinkedListHeaderBook.Node) = null; var (cont: LinkedListHeaderBook.Node) = null
    var current = list
    while(current != null){
      cont = current.next
      current.next = prev
      prev = current
      current = cont
    }
    prev
  }

  /** Delete the number stored against name (if it exists);
    * return true if the name existed. */
  def delete(name: String) : Boolean = {
    val n = find(name)
    if(n.next != null){ n.next = n.next.next; true }
    else false
  }
}

// Companion object
object LinkedListHeaderBook{
  private class Node(var name:String, var number:String, var next:Node){
    override def toString : String = {
      var liststr = ""
      if(next != null) {
        liststr += (name + "'s number: " + number + "/n" + next.toString)
      }
      else liststr += name + "'s number: " + number
      liststr
    }
  }
}
