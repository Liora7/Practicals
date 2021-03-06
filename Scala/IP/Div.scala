object Div{

  def div(x: Int, y: Int): Int = {
    require(y>0)
    var a = x; var q = 0
    while(a>=y){
      a = a - y
      q = q + 1
    }
    q
  }

  def main(args: Array[String]) = {
    //println(div(args(0).toInt, args(1).toInt))
    println(search(args(0).toArray, args(1).toArray))
  }

 /** Does pat appear as a substring of line?
 * i.e. pat[0..K) = line[i..i+K) for some i in [0..j).*/
 def search(pat: Array[Char], line: Array[Char]) : Boolean = {
 val K = pat.size; val N = line.size
 // Invariant: I: found = (line[i..i+K) = pat[0..K) for
 // some i in [0..j)) and 0 <= j <= N-K
 var j = 0; var found = false
 while(j <= N-K && !found){
 // set found if line[j..j+K) = pat[0..K)
 // Invariant: line[j..j+k) = pat[0..k)
 var k = 0
 while(k<K && line(j+k)==pat(k)) k = k+1
 found = (k==K)
 j = j+1
 }
// I && (j=N-K+1 || found)
 // found = ( line[i..i+K) = pat[0..K) for some i in [0..N-K+1) )
found
}

}
