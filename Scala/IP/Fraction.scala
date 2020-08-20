object Fraction {

  def findM(p: Int, q: Int): Int = {
    var m = 1
    var pm = p
    // Inv I: pm = p*m && 1<=m<=q
    while (q>pm){
      // I && 1<=m<q && q>pm
      pm += p
      // 1<=m<q && pm=p*(m+1)
      m += 1
      // I && 1<=m<=q
    }
    // I && 1<=m<=q && q<=pm
    m
  }

  def findArray(x: Int, y: Int)= {
    var p = x; var q = y
    var d = new Array[Int](p)
    var i=0; var m=1
    // Inv I: x/y = p/q + sum(1/d(n)), 0<=n<i
    while(p>0){
      m = findM(p, q)
      d(i) = m
      i += 1
      p = m*p - q
      q = q*m
      // I
    }
    // I && p=0 -> x/y = sum(1/d(n)), 0<=n<i
    d
  }

  def main(args: Array[String]) = {
    val p = args(0).toInt; val q = args(1).toInt
    val d = findArray(p, q)
    var i = 1
    print(p+"/"+q + " = " + "1/"+d(0))
    while(i<d.size && d(i)>0){
      print(" + " + "1/"+d(i))
      i += 1
    }
    println()
  }
}
