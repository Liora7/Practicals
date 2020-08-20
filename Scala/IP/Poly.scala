object Poly {

  def eval(a: Array[Double], x: Double): Double = {
    var xpow = 1.0; val n = a.size
    var i = 0; var result=0.0
    // Inv I: result = sum(a(k)*xpow), 0<=k<i && 0<=i<=n
    while(i<n){
      result += a(i)*xpow
      // result = sum(a(k)*xpow), 0<=k<i+1 && 0<=i<n
      xpow = xpow * x
      i += 1
      // I && 0<=i<=n
    }
    // I && i=n -> result = sum(a(k)*xpow), 0<=k<n
    result
  }

  def main(args: Array[String]) = {
    val a = Array[Double](1, 1, 1)
    println(eval(a, args(0).toDouble))
  }

}
