// Representing the phone book using a pair of arrays

object ArraysBook extends Book{
  private val MAX = 1000 // max number of names we can store
  private val names = new Array[String](MAX)
  private val numbers = new Array[String](MAX)
  private var count = 0
  // These variables together represent the mapping
  // Abs: book =
  // { names(i) -> numbers(i) | i <- [0..count) }
  // DTI: 0 <= count <= MAX &&
  // entries in names[0..count) are distinct

  // Return the index i<count s.t. names(i) = name; or
  //              return count if no such index exists
  private def place(name: String) : Int = {
    // Invariant: name not in names[0..i) && i <= count
  // invariant I: a[0..i) < x <= a[j..N) && 0 <= i <= j <= N
    var i = 0
    var j = MAX; var k = 0
    while(k<j){
      val m = k + (j-k+1)/2
      if (names(m) == null) j = m - 1 else k = m
    }
    if (names(k) == null) j=0 else j = k
    while(i < j){
      val m = (i+j)/2 // i <= m < j
      if(names(m) < name) i = m+1 else j = m
  }
  // I && i = j, so a[0..i) < x <= a[i..N)
  i
  }

  private def find(name: String) : Int = {
    // Invariant: name not in names[0..i) && i <= count
  // invariant I: a[0..i) < x <= a[j..N) && 0 <= i <= j <= N
  val i = place(name)
  if (names(i)==name) i else MAX
  }

  /** Return the number stored against name */
  def recall(name: String) : String = {
    val i = find(name)
    assert(i < count)
    numbers(i)
  }

  /** Is name in the book? */
  def isInBook(name: String) : Boolean = find(name) < count

  /** Add the maplet name -> number to the mapping */
  def store(name: String, number: String) = {
    val i = place(name)
    if(i == count){
      assert(count < MAX); names(i) = name; count += 1
    }
    numbers(i) = number
  }
  /** Delete the number stored against name (if it exists) */
  def delete(name: String) : Boolean = {
    val i = find(name)
    if (i<count){
      names(i) = null
      numbers(i) = null
      for (j <- i+1 until MAX){
        while (names(j) != null){
          names(j-1)=names(j); numbers(j-1)=numbers(j)
        }
      }
      return true
  }
  false
}

}
