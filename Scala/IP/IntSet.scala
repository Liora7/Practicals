// A class of objects to represent a set

class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {}
  private var theSet : Node = null // or however empty set is represented
  private var count : Int = 0

  /** Convert the set to a string.
    * (The given implementation is not sufficient.) */
  override def toString : String = {
    // O(count), as it goes through each node
    var liststr = "{"
    var n = theSet
    if (n==null) return "{}"
    while(n.next != null) {
      liststr += (n.datum.toString + ", ")
      n = n.next
    }
    liststr += n.datum.toString + "}"
    liststr
  }

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) = {
    // O(count), as find is O(count) and the rest is O(1)
    val n = find(e)
    if (n == null){
      theSet = Node(e, null)
      count += 1
    }
    else if (n.datum == e) {}
    else if (e < theSet.datum) {
      theSet = Node(e, theSet)
      count += 1
    }
    else {
      n.next = Node(e, n.next)
      count += 1
    }
  }

  private def find(x: Int) : Node = {
    // O(count) in worst case, as find goes through each node until x's position is found
    var n = theSet
    if (n == null) return n
    // Invariant: name does not appear in the nodes up to and
    // including n; i.e.,
    // for all n1 in L(list.next, n.next), n1.name != name
    while(n.next != null && n.next.datum <= x) n = n.next
    n
  }

  private def finddel(x: Int) : Node = {
    // O(count), just as for find
    var n = theSet
    if (n == null) return n
    // Invariant: name does not appear in the nodes up to and
    // including n; i.e.,
    // for all n1 in L(list.next, n.next), n1.name != name
    while(n.next != null && n.next.datum < x) n = n.next
    n
  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int = count
  // O(1)

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  def contains(e: Int) : Boolean = {
    // O(count) because of find
    if (theSet != null) {find(e).datum == e} else false
}

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int = {assert (theSet != null); theSet.datum}
  // O(1)

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    // O(count) as move through both linked lists simultaneously to prevent O(countˆ2)
    case s: IntSet => {
      var n = theSet
      var t = s.theSet
      if (count != s.count) return false
      else if (count == 0) return true
      else {
        while(n.datum == t.datum && n.next != null) {
          n = n.next
          t = t.next
        }
      }
      n.datum == t.datum

    }
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    // O(count) because of finddel
    val n = finddel(e)
    if (n == null) false
    else if(theSet.datum == e){ theSet = null; count = 0; true}
    else if(n.next.datum == e){ n.next = n.next.next; count -= 1; true }
    else false
  }


  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: IntSet) : Boolean = {
    // O(that.count) as in the worst case, all elements of this are at the back of that, so we end up checking all of that's elements.
    var n = theSet
    var s = that.theSet
    if (count > that.count) false
    else{
      while(n.next != null) {
        while(s.next != null && n.datum < s.datum) {
          s = s.next
        }
        if (n.datum == s.datum) {n = n.next; s = s.next} else return false
      }
      while(s.next != null && n.datum < s.datum) {
        s = s.next
      }
      if (n.datum == s.datum) return true
    }
    false
  }

  // ----- optional parts below here -----

  /** return union of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = {
    // O((this.count + this.count)*res.count) as each element must be added in O(res.count)
    val res = IntSet()
    var s = theSet
    while (s != null) { res.add(s.datum); s = s.next }
    var n = that.theSet
    while (n != null) { res.add(n.datum); n = n.next }
    res
  }

  /** return intersection of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = {
    // O(that.count*this.count) because each element of n must be checked in O(this.count)
    val res = IntSet()
    var n = that.theSet
    while (n != null) {
      if (this.contains(n.datum)) {
        res.add(n.datum)
        n = n.next
      }
      else n = n.next
  }
  res
}

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet = {
    // O(count^2) as each add is O(count)
    val res = IntSet()
    var n = theSet
    while (n != null){
      res.add(f(n.datum))
      n = n.next
    }
    res
  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} *///
  def filter(p : Int => Boolean) : IntSet = {
    // O(res.count) as each add is O(res.count)
    val res = IntSet()
    var n = theSet
    while (n != null){
      if (p(n.datum)) res.add(n.datum)
      n = n.next
    }
    res
  }
}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined
    * the main constructor and the add operation.
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}
