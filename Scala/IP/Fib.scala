object Fib{

  def fib(n : Int, depth: Int): Int = {
    println("| " * depth + "fib("+n+")")
    val res = n match {
     case 0 => 0
     case 1 => 1
     case _ => {
      fib((n-1), depth+1)+fib((n-2), depth+1)
     }
    }
    println("| " * depth + "= " + res)
    res
  }

  def iterfib(n: Int): Int = {
    var f0 = 0
    var f1 = 1
    var i = 0
    // Invariant I: f0 = fib(i), f1 = fib(i+1) && 0<=i<=n
    while (i<n) {
      // 0<=i<n
      val temp = f0
      // temp = fib(i)
      f0 = f1
      // f0 = fib(i+1)
      f1 = f1 + temp
      // f1 = fib(i+2)
      i += 1
      // I: f0 = fib(i), f1 = fib(i+1) && 0<=i<=n
    }
    f0
    // I && i=n: f0 = fib(n)
  }

  def main(args: Array[String]){
    val n = args(0).toInt
    fib(n, 0)
    println(iterfib(n))
  }

}
