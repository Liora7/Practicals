object insertion{

  def sort(a: Array[Int]): Array[Int]={
    var array = a; val n = a.size
    var i = 1
    // Inv I: array[0..i) is sorted && 1<=i<=n
    while(i<n){
      val temp = array(i)
      var j = i-1
      while(j>=0 && array(j)>temp){
        array(j+1) = array(j)
        j -= 1
      }
      array(j+1) = temp
      // array[0..i+1) is sorted && 1<=i<n
      i += 1
      // I
    }
    // I && i=n -> array[0..n) is sorted
    array
  }

  def main(args: Array[String])={
    val n = args.size
    var a = new Array[Int](n)
    for (i <- 0 until n) a(i) = args(i).toInt
    val sorted = sort(a)
    println()
    for (i <- 0 until n) print(sorted(i) + " ")
    println()
  }

}
