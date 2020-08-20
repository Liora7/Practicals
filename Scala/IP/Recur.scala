object Recur{

  def recur(s: Array[Char]): Int = {
    var n = 1; var len = s.size
    while (n<len){
      // 1<=n<=len
      var i=0
      // 1<=i<=n
      while (i<n && s(i)==s(n+i)){
        // Inv I: s[0..i)==s[i..N) && 1<=i<n
        i += 1
        // 1<=i<=n
      }
      if (i==n) return n
      // I && i==n -> s[0..n)==s[n..N)
      n += 1
      // I && i,n -> s[0..n)/=s[n..N)
    }
    n
    // n = len
  }

  def main(args: Array[String]) = {
    println(recur(args(0).toArray))
  }

}
