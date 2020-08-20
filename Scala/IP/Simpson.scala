object Simpson{

  def approx(n: Int): Double = {
    val f = (x: Double) => scala.math.pow(x, 1.5)
    var (i: Int) = 0; var (x: Double) = 0
    var (sum: Double) = 0
    while(x<2){
      if (i%2 == 0){
        sum = sum + 2*(f(x))
      }
      else{
        sum = sum + 4*(f(x))
      }
      x = x + 2.0/n
      i += 1
    }
    sum = sum + (f(x))
    sum = 2.0/(3*n) * sum
    sum
  }

  def main(args: Array[String])={
    val (result: Double) = scala.math.pow(2, 3.5)/5
    var (i: Int) = 1; var (j: Int) = 2
    while(i<25){
      val (simpson: Double) = approx(j)
      println("n= 2^" + i + ": " + simpson)
      println("error= " + (result - simpson))
      println()
      i += 1
      j *= 2
    }
  }

}
