object Newtons{

  type Vecf = ((Double, Double) => (Double, Double))

  def newton(f: Vecf, x0: (Double, Double), N: Int, tol: Double, J: (Array[(Double, Double) => Double])) = {
    var n = 0; var (dx: (Double, Double)) = (1,1)
    var x = new Array[(Double, Double)](N+1)
    x(0) = x0
    while (n < N && abs(dx) > tol*(1 + abs(x(n)))){
      val fxs = f(x(n)._1, x(n)._2)
      val Jinv = invert(calcm(J,x(n)))
      dx = matmul((fxs._1, fxs._2), Jinv)
      val (p, r) = (x(n)._1 - dx._1, x(n)._2 - dx._2)
      x(n+1) = (p, r)
      println(x(n+1))
      n += 1
    }
    if (n==N) println("hit max iterations")
    x(n)
  }

  def abs(x: (Double, Double))={
    val prod = scala.math.sqrt(x._1 * x._1 + x._2 * x._2)
    prod
  }

  def calcm(J: Array[(Double, Double) => Double], x: (Double, Double)): Array[Double]={
    val M = new Array[Double](4)
    M(0) = J(0)(x._1, x._2)
    M(1) = J(1)(x._1, x._2)
    M(2) = J(2)(x._1, x._2)
    M(3) = J(3)(x._1, x._2)
    M
  }

  def invert(J: Array[Double]): Array[Double] ={
    val det = 1/(J(0)*J(3) - J(1)*J(2))
    val inv = new Array[Double](4)
    inv(0) = det * J(3)
    inv(1) = -1 * det * J(1)
    inv(2) = -1 * det * J(2)
    inv(3) = det * J(0)
    inv
  }

  def matmul(v: (Double, Double), m: Array[Double]): (Double, Double)={
    val x = m(0) * v._1 + m(1) * v._2
    val y = m(2) * v._1 + m(3) * v._2
    (x, y)
  }

  def main(args: Array[String])={
    val (f: Vecf) = (x: Double, y: Double) => (1/((2-x)*(2-y)) - x, 2/((3-2*x)*(3-y)) - y)
    val J = new Array[(Double, Double) => Double](4)
    J(0) = (x: Double, y: Double) => 1/((2-x)*(2-x)*(2-y)) - 1
    J(1) = (x: Double, y: Double) => 1/((2-x)*(2-y)*(2-y))
    J(2) = (x: Double, y: Double) => 4/((3-2*x)*(3-2*x)*(2-y))
    J(3) = (x: Double, y: Double) => 2/((3-2*x)*(3-y)*(3-y)) - 1
    val (x0: (Double, Double))= (0.5, 0.5)
    val N = 20
    val tol = 0.000001
    println(newton(f, x0, N, tol, J))
  }

}
