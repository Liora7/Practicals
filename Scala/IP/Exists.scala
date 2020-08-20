object Exists {

  def exists(p: Int => Boolean, N: Int): Boolean = {
    var i = 0
    // 0<=i<=N
    while (i<N){
      // Inv I: p(i-1)=false && 0<=i<N
      if (p(i)) return true   // I && p(i)=true && 0<=i<N -> true
      // I && p(i)=false && 0<=i<N
      else i +=1
      // I && p(i-1)=false && 0<=i<=N
    }
    // I && i=N && p(i-1)=false -> false
    false
  }

  def main(args: Array[String]) = {
    val p: (Int => Boolean) = x => {x>100}
    println(exists(p, args(0).toInt))
  }
}
