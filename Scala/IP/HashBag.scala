// A hash table, representing a bag of words; i.e., for each word we record
// how many times the word is stored.

// Companion object
object HashBag{
  private class Record(val word: String, var count: Int)
}

class HashBag{
  // DTI: 0<=size_<=MAX

  // The hash function we will use
  private def hash(word: String) : Int = {
    def f(e: Int, c: Char) = (e*41 + c.toInt) % MAX
    word.foldLeft(1)(f)
    // word.foldLeft(1)(((_:Int)*41+(_:Char).toInt) % N)
  }

  private val MAX = 100   // # buckets in the hash table
  private var size_ = 0 // # distinct words stored


  private val table = new Array[HashBag.Record](MAX) // the hash table

  /** Find node containing word in linked list starting at head, or
    * return null if word does not appear */

  private def find(word: String) : Int = {
    var h = hash(word)
    var i = h
    while(table(i) != null && table(i).word != word) i = (i+1)%MAX
    i
  }


  /** Add an occurrence of word to the table */
  def add(word: String) = {
    assert(size_ < MAX)
    val h = hash(word)
    var i = find(word)
    while(table(i) != null && table(i).word != word){
      i += 1
    }
    if (table(i)==null){
      table(h) = new HashBag.Record(word, 1)
      size_ += 1
    }
    else table(i).count += 1
    }

  /** The count stored for a particular word */
  def count(word: String) : Int = {
    var i = find(word)
    if(table(i) != null) table(i).count else 0
  }

  // return the size
  def size = size_

  def delete(word: String) = {
    var i = find(word)
    if (table(i) != null && table(i).count > 1) table(i).count = table(i).count - 1
    else if (table(i) != null) {table(i) = null; size_ -= 1}
  }

  // print the hash table
  def printBag = {
    for(i <- 0 until MAX){
      var n = table(i)
      if(n != null){ println(n.word+"\t"+n.count) }
    }
  }
}
