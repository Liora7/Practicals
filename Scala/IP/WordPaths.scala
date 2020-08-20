object WordPaths{
  /** The dictionary */
  var dict : Dictionary = null

  /** A type representing paths through the graph of words */
  type Path = List[String]

  /** Print the Path path, separating entries with commas.
    * Pre: path is non-empty. */
  def printPath(path: Path) = {
    print(path.head)
    for(w <- path.tail) print(", "+w)
    println
  }

  /** Find all neighbours of w */
  def neighbours(w: String) : Path = {
    var result = List[String]() // build up the result
    for(i <- 0 until w.length; c <- 'a' to 'z')
      if(c != w(i)){
        val w1 = w.patch(i,List(c),1) // replace ith character
                                      // of w with c
        if(dict.isWord(w1)) result = w1 :: result
      }
    result
  }

  /** Find a minimum length path from start to target.
    * @return Some(p) for some shortest Path p if one exists;
    * otherwise None. */
  def findPath(start: String, target: String) : Option[Path] = {
    // We'll perform a breadth-first search.  Each node of the search graph
    // will be a list of words, consecutive words differing in one letter, and
    // ending with start, thereby representing a path (in reverse order)
    require(target.length == start.length)
    val queue = scala.collection.mutable.Queue(List(start))
    // Keep track of the words we've already considered
    val seen = new scala.collection.mutable.HashSet[String]
    seen += start

    while(!queue.isEmpty){
      val path = queue.dequeue; val w = path.head
      for(w1 <- neighbours(w)){
        if(w1==target) return Some((target::path).reverse)
        else if(!seen.contains(w1)){seen += w1; queue += w1::path}
      } // end of for
    } // end of while
    None // no solutions found
  } // end of findPath

  def init() {
    var dictFile = "knuth_words.txt"
    dict = new Dictionary(dictFile)
  }

  // The main function
  def main(args: Array[String]) = {
    val t0 = System.currentTimeMillis()
    // parse arguments
    val errMsg =
      "Usage: scala WordPaths [-d dict-file] start target"
    var i=0; var start = ""; var target = ""
    var dictFile = "knuth_words.txt"
    while(i<args.length){
      if(args(i)=="-d"){ dictFile = args(i+1); i += 1 }
      else if(start=="") start = args(i)
      else{ require(target=="", errMsg); target = args(i) }
      i += 1
    }
    require(target!="", errMsg)

    // check lengths agree; initialise dictionary
    require(target.length == start.length,
	   "start and target words should be the same length")
    dict = new Dictionary(dictFile)

    // The main stuff
    val optPath = findPath(start, target)
    optPath match{
      case Some(path) => printPath(path)
      case None => println("No path found")
    }

    println("Time taken: "+(System.currentTimeMillis()-t0))
  }
}
