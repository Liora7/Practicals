class AnaDict(fname: String){

  /** An Array object holding the words */
  private var words: Array[(String, String)] = null
  private var size = 0

  /** Initialise dictionary from fname */
  private def initDict(fname: String) = {
    val allWords = scala.io.Source.fromFile(fname).getLines.toArray
    // Should word w be included?
    def include(w:String) = w.forall(_.isLower)
    for(w <- allWords; if include(w)) size += 1
    // find size of dict
    words = new Array[(String, String)](size)
    // declare words as array
    var i = 0
    for(w <- allWords; if include(w)){
      words(i) = (w.sorted, w)
      // include every word and its sorted anagram in words
      i += 1
    }
    words = words.sorted
    // sort dict
  }

  // Initialise the dictionary
  initDict(fname)

  // find all anagrams of s
  def findanas(s: String): Unit ={
    val sorted = s.sorted
    // sort word in order to find it by binary search
    var i = 0
    var j = size
    while (i < j){
      val m = (i + j)/2
      if (words(m)._1 < sorted) i = m+1 else j = m
    }
    // found first entry in words that is the same as s when sorted
    while (i < size && words(i)._1 == sorted){
      // print all words in word which are anagrams of s
      println(words(i)._2)
      i += 1
    }
  }
}
