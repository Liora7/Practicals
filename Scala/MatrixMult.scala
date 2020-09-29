import io.threadcso._
class MatrixMult(a : Array[Array[Int]], b : Array[Array[Int]], n : Int, nWorkers: Int){

  def main(args : Array[String]) = {
  }

  // Task consists of the index of the row of matrix a and column of b to multiply
  private type Task = (Int, Int)
  private type Res = (Int, Int, Int)

  /** Channel from the controller to the workers, to distribute tasks.  We
  * create the channel freshly for each run of the system. */
  private var toWorkers: Chan[Task] = null // = OneMany[Task]

  /** Channel from the workers to the collector, to return sub-results. */
  private val toCollector = ManyOne[Res]

  /** A worker, which repeatedly receives arguments from the distributor,
  * calculates the dot product, and sends the result to the collector. */
  private def worker = proc("worker"){
    repeat{
      val (i, j) = toWorkers?()
      val result = (i, j, dot(i, j))
      toCollector!result
    }
  }

  private def dot(i: Int, j: Int) = {
    var sum = 0
    for (k <- 0 until n){
      sum += a(i)(k) * b(k)(j)
    }
    sum
  }

  private def distributor = proc("distributor"){
    for(i <- 0 until n){
      for (j <- 0 until n){
        toWorkers!(i, j)
      }
    }
    toWorkers.close
  }

  /** This variable ends up holding the result. */
  private var c = Array.ofDim[Int](n, n)

  /** A collector, that accumulates the sub-results into result. */
  private def collector = proc("collector"){
    for(k <- 0 until n*n){
      val (i, j, r) = toCollector?()
      c(i)(j) = r
    }
    toCollector.close
  }

  /** The main system. */
  private def system = {
    toWorkers = OneMany[Task]
    val workers = || (for (i <- 0 until nWorkers) yield worker)
    workers || distributor || collector
  }

  def apply: Array[Array[Int]] = { system(); c }
}


import scala.util.Random
object MatrixMultTest{
  // size of matric; range of input values.
  val n = 100; val Max = 100
  val nWorkers = 1 + Random.nextInt(Max)
  /** Run a single test.  Generate a random number of workers and two arrays filled with random numbers.
    * Receive output.  Check result is as expected. */
  def doTest = {
    val a = new Array[Array[Int]](n)
    for (i <- 0 until n){
      val xs = Array.fill(n)(Random.nextInt(Max))
      a(i) = xs
    }
    val b = new Array[Array[Int]](n)
    for (i <- 0 until n){
      val xs = Array.fill(n)(Random.nextInt(Max))
      b(i) = xs
    }
    val mult = new MatrixMult(a, b, n, nWorkers)
    val c = mult.apply
    sameMatrixMult(c, a, b)
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }

  def sameMatrixMult(c: Array[Array[Int]], a: Array[Array[Int]], b: Array[Array[Int]]){
    for (i <- 0 until n){
      for (j <- 0 until n){
        var z = 0
        for (k <- 0 until n){
          z += a(i)(k) * b(k)(j)
        }
        assert(z == c(i)(j))
      }
    }
  }
}
