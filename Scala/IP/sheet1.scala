object sheet1{
  def square(x: Int): Int = x*x

  def rem3(x: Int): Int = x - 3*(x/3)

  def lsquare(x: Int): Int = {
    var n = 0
    while (square(n)<=x) {n=n+1}
    n-1
}

  def divmod(x: Int, y: Int) = {
    require (y>0)
    // x >= 0, y > 0
    var q = 0; var r = x
    // Invariant I: x = q*y + r
    while (r >= y) {
        q +=1
        // x < q*y + r
        r -= y
        // I: x = q*y + r
      }
      (q, r)
    }

    def gcd(m: Int, n: Int): Int = {
      var a = m; var b = n
      while(a>0 && b>0) {
        var r = divmod(a, b)._2
        a = b; b=r
      }
      if (a==0) b else a
    }

    def euclid(m: Int, n: Int) = {
      var a = m; var b = n
      var x = 0; var x1 = 1; var y = 1; var y1 = 0;
      // Invariant I: a*x1 + b*y1 = a && a>=0 && b>=0
      while(a>0 && b>0) {
        var (q,r) = divmod(a, b)
        var tempx = x; var tempy = y
        x = x1 - q*x; x1 = tempx
        y = y1 - q*y; y1 = tempy
        // a*x1 + b*y1 = b && a>0 && b>0
        a = b; b=r
        // I: a*x1 + b*y1 = a && a>=0 && b>=0
      }
      if (a==0) (b, x1, y1) else (a, x1, y1)
      // I && a==0 || b==0: m*x1 + n*x1 = b = gcd || m*x1 + n*x1 = a = gcd
    }


  def main(args: Array[String]){
    /*println(square(args(0).toInt))
    println(rem3(args(1).toInt))
    println(lsquare(args(2).toInt))*/
    println(gcd(args(0).toInt, args(1).toInt))
    println(euclid(args(0).toInt, args(1).toInt))
  }

}
