object LinkedList{
  class Node(val datum: Int, var next: Node){
    override def toString : String = {
      var liststr = ""
      if(next != null) {
        liststr += (datum.toString + " -> " + next.toString)
      }
      else liststr += datum.toString
      liststr
    }
  }

  var myList = new Node(1, null)
  for (i <- 2 to 12){
    myList = new Node(i, myList)
  }

  def revlst(list: Node)={
    var (prev: Node) = null; var (cont: Node) = list.next
    var current = list
    // I: prev.next = current && current != null
    while(current != null){
      cont = current.next
      // store val of current.next
      current.next = prev
      // "reverse" pointer
      prev = current
      // move prev forward
      current = cont
      // move current forward
      // prev.next = current\
      // I
    }
    // current = null
    // for all nodes 'current' in the list, I
    prev
  }



}
