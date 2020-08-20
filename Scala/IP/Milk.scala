// Add up a milk bill
object Milk{
  /** Calculate sum of a
    * Post: returns sum(a) */
  def findSum(a : Array[Int]) : Int = {
    val n = a.size
    var total = 0; var i = n
    // Invariant I: total = sum(a[i..n)) && 0<=i<=n
    // Variant i
    while(i > 0){
      // I && i>0
      i -= 1
      //total = sum(a[i+1..n))
      total += a(i)
      // I: total = sum(a[i..n)) && i=>0
    }
    // I && i=0
    // total = sum(a[0..n))
    total
  }

  def findMax(a : Array[Int]) : Int = {
    val n = a.size
    var max = 0; var i = n
    // Invariant I: max=maximum(a[i..n)) && 0<=i<=n
    // Variant i
    while(i>0){
      i -= 1
      // max=maximum(a[i+1..n))
      if (a(i)>max) max=a(i)
      // max=maximum(a[i..n)) && i=>0
    }
    // I && I=0
    // max=maximum(a[0..n))
    max
  }

  // Main function.  Various equivalent forms commented out
  def main(args : Array[String]) = {
    // val n = args.size
    // val a = new Array[Int](n)
    // for(i <- 0 until n) a(i) = args(i).toInt
    val a = args.map(x => x.toInt)
    println(findSum(a))
    println(findMax(a))
  }

  // def main(args : Array[String]) = println(findSum(args.map(s => s.toInt)));
}
