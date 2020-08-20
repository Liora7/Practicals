object Anagrams{

  def lexi(s: Array[Char]): (Array[Char], Boolean)={
    val n = s.size
    var a = s
    var k = n - 1
    while (k > 0 && a(k-1) >= a(k)) k -= 1
    if (k<=0) return (a, false)
    var l = n - 1
    while (a(l) <= a(k-1)) l -= 1
    val temp = a(k-1)
    a(k-1) = a(l)
    a(l) = temp
    var j = n-1; var i = k
    while (k < j){
      val temp = a(k)
      a(k) = a(j)
      a(j) = temp
      k += 1
      j -= 1
    }
    (a, true)
  }

  def anagrams(s: String): Unit={
    var a = s.toLowerCase.toArray.sorted
    val dict = new Dictionary("knuth_words.txt")
    var flag = true
    while (flag){
      val (n, f) = lexi(a)
      a = n; flag = f
      val word = a.mkString("")
      if (dict.isWord(word)) println(word)
    }
  }
  val dict = new scala.collection.mutable.HashSet[String]
  val allWords = scala.io.Source.fromFile("knuth_words.txt").getLines
  for(w <- allWords) dict += w
  val anas = new Array[(String, String)](10 * dict.size)

  def isWord(s: scala.collection.mutable.HashSet[String], w: String) : Boolean = s.contains(w)

  def anadict: Unit = {
    var size = 0
    for (check <- dict){
      var sorted = check.toLowerCase.sorted
      var a = sorted.toArray
      var flag = true
      while (flag){
        val (n, f) = lexi(a)
        a = n; flag = f
        val word = a.mkString("")
        if (isWord(dict, word)) {
          var i = 0; var j = size
          while(i < j){
            val m = (i+j)/2
            if(anas(m)._1 < sorted) i = m+1 else j = m
          }
        anas(i) = (sorted, word); size += 1
      }
    }
  }
}

  def findanas(s: String): Unit = {
    val x = s.toLowerCase.sorted
    val N = anas.size
    var i = 0; var j = N
    while(i < j){
      val m = (i+j)/2
      if(anas(m)._1 < x) i = m+1 else j = m
    }
    while(anas(i)._1 == x){
      println(anas(i)._2)
      i += 1
    }
  }


}
