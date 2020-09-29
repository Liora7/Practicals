import io.threadcso._
import scala.util.Random

object Q1{

  def multiplex[T](in0: ?[T], in1: ?[T], out0: ![T], out1: ![T]): PROC = proc{
    val mid = OneOne[(Int, T)]
    val tag = tagger(in0, in1, mid)
    val detag = detagger(out0, out1, mid)
    run(tag || detag)
  }

  def tagger[T](in0: ?[T], in1: ?[T], mid: ![(Int,T)]): PROC = proc{
    serve(
       in0 =?=> { x => mid!(0, x)}
      |in1 =?=> { x => mid!(1, x)}
    )
    in0.closeIn; in1.closeIn; mid.closeOut
  }

  def detagger[T](out0: ![T], out1: ![T], mid: ?[(Int,T)]): PROC = proc{
    repeat{
      val (tag, v) = mid?()
      tag match{
        case 0 => out0!v
        case 1 => out1!v
        case _ => throw new Exception()
      }
    }
    out0.closeOut; out1.closeOut; mid.closeIn
  }
}

/* This system terminates if there is a finite stream of inputs coming through in0 and in1, and if all of these inputs are synchronously received on out0 and ou1. The system ensures fairness through the serve statement, which ensures that both in0 and in1 will have infinite chances to send inputs as time goes to infinity.
*/

object Q2{

  def ChangeMachine(inPound: ?[Unit], out5p: ![Unit], out10p: ![Unit], out20p: ![Unit]): PROC = proc{
    var bal = 0
    serve (
        (bal==0 && inPound) =?=> { _ => bal += 100}
      | (bal>=5 && out5p) =!=> {} ==> {bal -= 5}
      | (bal>=10 && out5p) =!=> {} ==> {bal -= 10}
      | (bal>=20 && out5p) =!=> {} ==> {bal -= 20}
    )
  }

}

object Q3{

  def buff[T](in: ?[T], out: ![T]) = proc{
    val q = new scala.collection.mutable.Queue[T]()
    serve(
      in =?=> {x => q.enqueue(x)}
      | (q.nonEmpty && out) =!=> {q.dequeue}
    )
    in.closeIn; out.closeOut
  }

}


class Adaptive(f: Double => Double, a: Double, b: Double, Epsilon: Double, nWorkers: Int){
  require(a <= b)
  type Task = (Double, Double, Double, Double)
  val getTask = OneMany[Task]
  val putTask = ManyOne[Task]
  val result = ManyOne[Double]
  var res: Double = 0

  def worker: PROC = proc{
    repeat{
      val (l, r, fl, fr) = getTask?()
      val mid = (l+r)/2.0
      val fmid = f(mid)
      val lArea = (fl+fmid)*(mid-l)/2; val rArea = (fmid+fr)*(r-mid)/2
      val area = (fl+fr)*(r-l)/2
      if (Math.abs(lArea+rArea-area) < Epsilon) {
        result!area
      }
      else{
        putTask!(l, mid, fl, fmid); putTask!(mid, r, fmid, fr)
      }
    }
    getTask.closeIn; putTask.closeOut; result.closeOut
  }

  def bag: PROC = proc{
    val tasks = new scala.collection.mutable.Queue[Task]()
    getTask!(a, b, f(a), f(b))
    var outstanding = 1.0
    serve(
        (tasks.nonEmpty && getTask) =!=> {outstanding += 1; tasks.dequeue}
      | (outstanding>0 && putTask) =?=> {t => tasks.enqueue(t); outstanding -= 0.5}
      | (outstanding>0 && result) =?=> {r => res += r; outstanding -= 1}
    )
    getTask.closeOut; putTask.closeIn; result.closeIn
  }

  def apply(): Double = {
    val workers = || (for (i <- 0 until nWorkers) yield worker)
    run(bag || workers)
    return res
  }

}

/** Object to test the concurrent Trapezium rule code. */
object TrapeziumTest{
  /** We will test the Trapezium class by selecting random polynomials.  Each
    * polynomial will be represented by an array of its coefficients.  The
    * polynomial p represents the sum of p(i)*x^i, where i ranges over p's
    * indices. */
  type Polynomial = Array[Double]

  /* We'll create random polynomials, with degree uniform in [0..MaxDegree), and
    * coefficients uniform in [-MaxCoeff..MaxCoeff). */
  val random = scala.util.Random
  val MaxDegree = 5
  val MaxCoeff = 100

  /** Random Double in [-max, max). */
  def uniform(max: Double): Double = max*(2*random.nextDouble-1)

  /** Create a random polynomial. */
  def mkPoly: Polynomial =
    Array.fill(1+random.nextInt(MaxDegree))(uniform(MaxCoeff))

  /** Convert a poly to a string. */
  def toString(poly: Polynomial): String =
    (0 until poly.length).map(i => poly(i).toString+"x^"+i).mkString(" + ")

  /** Evaluate poly at x. */
  def evalPoly(poly: Polynomial)(x: Double): Double = {
    // Use Horner's rule
    var result = 0.0
    for(i <- poly.size-1 to 0 by -1) result = result*x + poly(i)
    result
  }

  /** Pick parameters for a test.
    * @return a tuple (f, p, a, b, nWorkers, n) indicating that the integral of f
    * from a to b should be estimated using n intervals and nWorkers workers,
    * and that f corresponds to p. */
  def pickParams: (Double => Double, Polynomial, Double, Double, Int, Int) = {
    // function to evaluate
    val p = mkPoly; val f = evalPoly(p)(_)
    // limits
    val a = uniform(10); val b = a+10*random.nextDouble
    // Number of workers
    val nWorkers = 1+random.nextInt(16)
    // Number of intervals
    val n = nWorkers + random.nextInt(1000)
    (f, p, a, b, nWorkers, n)
  }



  def main(args: Array[String]) = {
  }

}


/** A test for adaptive quadrature.
  *
  * Note: this assumes that TrapeziumTest and Adaptive are on the current
  * path. */
object AdaptiveTest{
  val Epsilon = 1E-6

  /** We will test the Adaptive class by selecting random polynomials.  Each
    * polynomial will be represented by an array of its coefficients.  The
    * polynomial p represents the sum of p(i)*x^i, where i ranges over p's
    * indices. */
  type Polynomial = Array[Double]

  /** Estimate the integrap of f from a to b using adaptive quadrature. */
  def estimate(f: Double => Double, a: Double, b: Double) : Double = {
    val mid = (a+b)/2.0
    val fa = f(a); val fb = f(b); val fmid = f(mid)
    val lArea = (fa+fmid)*(mid-a)/2; val rArea = (fmid+fb)*(b-mid)/2
    val area = (fa+fb)*(b-a)/2
    if (Math.abs(lArea+rArea-area) < Epsilon) area
    else estimate(f,a,mid) + estimate(f,mid,b)
  }

  /** Pick parameters for a test.
    * @return a tuple (f, p, a, b, nWorkers) indicating that the integral of f
    * from a to b should be estimated using nWorkers workers, and that f
    * corresponds to p. */
  def pickParams: (Double => Double, Polynomial, Double, Double, Int) = {
    // function to evaluate
    val p = TrapeziumTest.mkPoly; val f = TrapeziumTest.evalPoly(p)(_)
    // limits
    val a = TrapeziumTest.uniform(10); val b = a+10*Random.nextDouble
    // Number of workers
    val nWorkers = 1+Random.nextInt(16)
    (f, p, a, b, nWorkers)
  }

  /** Do a single test. */
  def doTest = {
    val (f, p, a, b, nWorkers) = pickParams
    val seqResult = estimate(f, a, b)
    val concResult = new Adaptive(f, a, b, Epsilon, nWorkers)()
    assert(
      seqResult != 0.0 && Math.abs((seqResult-concResult)/seqResult) < 1E-7 ||
        Math.abs(seqResult-concResult) < 1E-10,
      "failed\nf = "+TrapeziumTest.toString(p)+"\n"+
        "a = "+a+"; b = "+b+"; nWorkers = "+nWorkers+"\n"+
        "seqResult = "+seqResult+"; concResult = "+concResult)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 10000){
      doTest; if(i%10 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}


class Q5{

  val manReq = ManyOne[(String, Chan[String])]
  val womanReq = ManyOne[(String, Chan[String])]

  def manSync(me: String): String = {
    val myChan = OneOne[String]
    var partner = ""
    proc{manReq!(me, myChan)
    partner = myChan?()}()
    partner
  }

  def womanSync(me: String): String = {
    val myChan = OneOne[String]
    var partner = ""
    proc{womanReq!(me, myChan)
    partner = myChan?()}()
    partner
  }

  def matchMaker: PROC = proc{
    repeat{
      val (manName, manChan) = manReq?()
      val (womanName, womanChan) = womanReq?()
      manChan!womanName; womanChan!manName
    }
  }

  def shutDown() = {
    manReq.close; womanReq.close
  }


}

object Q5Test{
  // Number of elements; range of input values.
  val N = 100; val Length = 10
  def doTest = {
    val matcher = new Q5()
    val men = List.fill(N)(Random.alphanumeric.take(Length).mkString(""))
    val women = List.fill(N)(Random.alphanumeric.take(Length).mkString(""))
    var pairs = scala.collection.mutable.HashMap[String, String]()
    def manProc(m: String): PROC = proc{
        val w = matcher.manSync(m)
        if (pairs.contains(m)) assert (pairs(m) == w)
        else pairs += m -> w
    }
    def menProcs = || (for (m <- men) yield manProc(m))
    def womanProc(w: String): PROC = proc{
        val m = matcher.womanSync(w)
        if (pairs.contains(m)) assert (pairs(m) == w)
        else pairs += m -> w
    }
    def womenProcs = || (for (w <- women) yield womanProc(w))
    def runAndShut: PROC = proc{ run(menProcs || womenProcs); matcher.shutDown()}
    run(runAndShut || matcher.matchMaker) // run concurrently
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }
}


class Q6[T](f: (T, T) => T, n: Int, xs: List[T]){
  private var fxs: Option[T] = null

  private def p0(x: T, left: ?[T], right: ![T]): PROC = proc{
    right!x //send x0 to p1
    val res = left?()
    right!(res) // get final result f(f...f(x0, x1)...xn-1)
    fxs = Some(left?())
  }

  private def p(x: T, left: ?[T], right: ![T]): PROC = proc{
    val v = left?() // if this is pi, get f(f...f(x0,x1)...xi-1)
    right!f(v, x) // send f(f...f(x0,x1)...xi)
    val res = left?()
    right!res
  }

  def apply(): Option[T] = {
    val chans = List.fill(n)(OneOne[T])
    val pis = || (for (i <- 1 until n) yield (p(xs(i), chans(i), chans((i+1)%n))))
    run(pis || p0(xs(0), chans(0), chans(1)))
    return fxs
  }

}

object Q6Test{
  /** Pick parameters for a test.
    * @return a tuple (f, p, a, b, nWorkers) indicating that the integral of f
    * from a to b should be estimated using nWorkers workers, and that f
    * corresponds to p. */


  /** Do a single test. */
  def doTest = {
    val n = 100; val Max = 100
    val a, b, c = Random.nextInt(5) // function params
    val f: (Int, Int)=>Int = ((x: Int, y: Int) => a * x + b * y + c)
    val xs = List.fill(n)(Random.nextInt(Max))
    val seqResult = xs.reduce(f)
    val rr = new Q6[Int](f, n, xs)()
    assert (rr != None)
    val concResult = rr match {
      case Some(x) => x
      case _ => -1
    }
    assert(seqResult == concResult)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 100){
      doTest; if(i%10 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}

/* If f is commutative, each process can send round its value of x and pass on any values it gets, and each process can then incrementally compute f with the values of x it keeps receiving.
*/

/* Q7:
a) Suppose P0 is passive and sends a token with true to P1. Then P0 receives a message from PK and becomes active; suppose that then PK becomes passive, and the token only passes through passive processes. However, P0 is active and/or may have awoken other processes, and hence the result that all procs are passive is false. Hence this scheme does not achieve the desired result.

b) Let P0 be the counter process. Then let all processes send their id and state to P0; if all process id's are associated with passive, and P0 is passive, return passive. If in the process of checking, P0 gets an update from some process Pi saying that it is now active, P0 may return that not all processes are passive. If processes are fully connected or all have connections to P0, this is straightforward to implement. Otherwise, these messages may be forwarded along a chain of processes as the messages include process identities.

c) if all messages follow the ring topology, use the original scheme, with the difference that P0 can still return false even if it receives a true back from Pn, if P0 had received a message in the meantime - this is because in a ring topology, any message from unchecked processes that turns checked passive processes to active must go through P0.
