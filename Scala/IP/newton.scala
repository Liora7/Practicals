object newton{

  def approx(f: Double => Double, d: Double => Double): Double={
    var x: Double = 0
    var n = 1
    while (n<10000){
      x = x - (f(x) / d(x))
      n += 1
    }
    x
  }

  def main(args: Array[String])={
    val f = (k: Double, x: Double) => scala.math.pow(scala.math.E, k*(x-1)) - x
    val d = (k: Double, x: Double) => k* scala.math.pow(scala.math.E, k*(x-1)) - 1
    var k: Double = 1
    while (k<=10){
      val f = (x: Double) => scala.math.pow(scala.math.E, k*(x-1)) - x
      val d = (x: Double) => k* scala.math.pow(scala.math.E, k*(x-1)) - 1
      val res = approx(f, d)
      println("k = " + (scala.math.rint(k*10))/10 + "     x = " + res)
      k = (scala.math.rint((k+0.1)*10))/10
    }
  }

}
