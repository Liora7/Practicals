// Representing the phone book using a linked list with a dummy header

class LinkedSortListBook extends Book{
  // Abs: book = {n.name -> n.number | n in L(list)}
  // DTI: L(list) is finite and the names in L(list) are distinct and for each node n in L(list), n.name < n.next.name

  private var list = new LinkedSortListBook.Node("?", "?", null)
  // list represents the mapping composed of (n.name -> n.number)
  // maplets, when n is a node reached by following 1 or more
  // next references.

  /** Return the node with node.name <= name.
    * Post: book = book_0 && returns n s.t. n in L(list) &&
    * (node.name <= name*/
  private def find(name:String) : LinkedSortListBook.Node = {
    var n = list
    // Invariant: name does not appear in the nodes up to and
    // including n; i.e.,
    // for all n1 in L(list.next, n.next), n1.name != name
    while(n.next != null && n.next.name <= name) n = n.next
    n
  }

  /** Return the node with node.name < name.
    * Post: book = book_0 && returns n s.t. n in L(list) &&
    * (node.name < name*/
  private def finddel(name:String) : LinkedSortListBook.Node = {
    var n = list
    // Invariant: name does not appear in the nodes up to and
    // including n; i.e.,
    // for all n1 in L(list.next, n.next), n1.name != name
    while(n.next != null && n.next.name < name) n = n.next
    n
  }

  /** Is name in the book? */
  def isInBook(name: String): Boolean = find(name).name == name

  /** Return the number stored against name */
  def recall(name: String) : String = {
    val n = find(name); n.number
  }

  /** Add the maplet name -> number to the mapping such that the list is sorted*/
  def store(name: String, number: String) = {
    val n = find(name)
    if (n.name == name) n.number = number
    else n.next = new LinkedSortListBook.Node(name, number, n.next)
    println(list)
  }


  /** Delete the number stored against name (if it exists);
    * return true if the name existed. */
  def delete(name: String) : Boolean = {
    val n = finddel(name)
    if(n.next.name == name){ n.next = n.next.next; true }
    else false
  }
}

// Companion object
object LinkedSortListBook{
  private class Node(var name:String, var number:String, var next:Node){
    override def toString : String = {
      if(number == "?") next.toString
      else{
      var liststr = ""
      if(next != null) {
        liststr += (name + "'s number: " + number + "    " + next.toString)
      }
      else liststr += name + "'s number: " + number
      liststr
    }
    }
  }
}
