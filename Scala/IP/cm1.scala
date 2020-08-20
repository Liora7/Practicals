import scala.math.{E, abs, pow}
object cm1 {

  def findk(error: Double): Int = {
    var k = 0
    var lbound = -E + (1/E); var ubound = E + (1/E)
    while (abs(lbound)>error && ubound>error) {
      k += 1
      lbound = lbound/(k+1); ubound = ubound/(k+1)
    }
    k
  }

  def main(args: Array[String]) {
    println(findk(pow(10, -15)))
  }
}
