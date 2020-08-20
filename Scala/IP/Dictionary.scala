/** Each object of this class represents a dictionary, in which 
  * words can be looked up.
  * @param fname the name of a file containing a suitable list 
  * of words, one per line. */

class Dictionary(fname: String){
  /** A Set object holding the words */
  private val words = new scala.collection.mutable.HashSet[String]

  /** Initialise dictionary from fname */
  private def initDict(fname: String) = {
    val allWords = scala.io.Source.fromFile(fname).getLines
    // Should word w be included?
    def include(w:String) = w.forall(_.isLower)
    for(w <- allWords; if include(w)) words += w
    // println("Found "+words.size+" words")
  }

  // Initialise the dictionary
  initDict(fname)

  /** test if w is in the dictionary */
  def isWord(w: String) : Boolean = words.contains(w)
}

  
