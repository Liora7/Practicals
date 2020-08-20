import io.threadcso._
import scala.language.postfixOps
import scala.util.Random

object Q1{

  def multiplex[T](in0: ?[T], in1: ?[T], out0: ![T], out1: ![T]): PROC = {
    val mid = OneOne[(Int, T)] // create middle channel, which sends tuples of tag and value
    val tag = tagger(in0, in1, mid) // create tagger
    val detag = detagger(mid, out0, out1) // create detagger
    tag || detag // do concurrently
  }

  def tagger[T](in0: ?[T], in1: ?[T], mid: ![(Int, T)]): PROC = proc{
    serve( // keep accepting values from both inputs
        in0 =?=> { x => mid!(0, x) } // send tagged value on mid
      | in1 =?=> { x => mid!(1, x) }
      )
    in0.closeIn; in1.closeIn; mid.closeOut
  }

  def detagger[T](mid: ?[(Int, T)], out0: ![T], out1: ![T]): PROC = proc{
    repeat{
        val (tag, v) = mid?() // keep accepting values from mid until mid is closed
        tag match { // match tag and send to corresponding out channel
            case 0 => out0!v
            case 1 => out1!v
            case _ => println("error")
        }
    }
    out0.closeOut; out1.closeOut; mid.closeIn
  }

}


object Q1Test{
  // Number of elements; range of input values.
  val N = 100; val Max = 100
  def doTest = {
    val x0s, x1s = List.fill(N)(Random.nextInt(Max)) // create two lists of random inputs
    var y0s, y1s = List[Int]() // two empty lists for outputs
    val in0, in1, out0, out1 = OneOne[Int] // in and out channels
    def sender0 = proc{ for(x <- x0s) in0!x; in0.close } //send elems from first list on first in channel
    def sender1 = proc{ for(x <- x1s) in1!x; in1.close } // send elems from second list on second in channel
    def receiver0 = proc{ for(x <- x0s) y0s = out0?() :: y0s } // receive elems from first out channel
    def receiver1 = proc{ for(x <- x1s) y1s = out1?() :: y1s } // receive elems from second out channel
    run(sender0 || sender1 || receiver0 || receiver1 || Sheet2.multiplex(in0, in1, out0, out1)) // run concurrently
    assert(x0s.sorted.sameElements(y0s.sorted) && x1s.sorted.sameElements(y1s.sorted)) // check that each output list has the same values as the corresponding input list
  }
  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }
}


object Q2{

  def ChangeMachine(inPound: ?[Unit], out5p: ![Unit], out10p: ![Unit], out20p: ![Unit]){
    var balance = 0 // keep track of how many p are left
    serve(
        (balance == 0 && inPound) =?=> { _ => balance += 100 } // 1 pound received
      | (balance >= 5 && out5p) =!=> { balance -= 5; () } // give out 5p
      | (balance >= 10 && out10p) =!=> { balance -= 10; () } // give out 10p
      | (balance >= 20 && out20p) =!=> { balance -= 20; () } // give out 20p
    )
  }

  def doTest = {
    val inPound, out5p, out10p, out20p = OneOne[Unit] //create chans
    def change = ChangeMachine(inPound, out5p, out10p, out20p)
    def customer = proc{
      inPound!() // put in one pound
      var left = 100 // keep track of how often we need to ask for change
      while (left > 0){
        val rand = Random.nextInt(2) // make random choice
        if (rand == 0){ out5p?(); left -= 5}
        else if (rand == 1){ out10p?(); left -= 10}
        else if (rand == 2){ out20p?(); left -= 20}
      }
    }
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }

}


object Q3{

  def buff[T](in: ?[T], out: ![T]) = proc{
      val buf = scala.collection.mutable.Queue[T]() // create buffer
      serve(
          in =?=> { x => buf.enqueue(x) } // put new item in buffer
        | (!buf.isEmpty && out) =!=> { val x = buf.dequeue; x } // return item that's been in the buffer longest if there is one
      )
      in.closeIn; out.closeOut
  }

  def doTest = {
    val N = 100; val Max = 100
    val xs = Array.fill(N)(Random.nextInt(Max)) // create array of random inputs
    var ys = new Array[Int](N) // empty array for outputs
    val in, out = OneOne[Int] // in and out channels
    val b = buff[Int](in, out)
    def sender = proc{ for(x <- xs) in!x; in.close } //send elems from array on in
    def receiver = proc{ for(i <- 0 until N) ys(i) = out?() } // receive elems from out channel
    run(sender || receiver || b) // run concurrently
    assert(xs.sameElements(ys)) // check that output list is same as input list
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }

}


class Adaptive(f: Double => Double, a: Double, b: Double, Epsilon: Double, nWorkers: Int){
  require(a <= b)

  type Task = (Double, Double)
  /** Channel from the controller to the workers, to distribute tasks. */
  private val toWorkers = OneMany[Task]
  /** Channel from the workers to the controller, to return sub-tasks. */
  private val toController = ManyOne[Task]
  /** Channel from the workers to the controller, to return intermediate results */
  private val sendRes = ManyOne[Double]
  /** This variable ends up holding the result. */
  private var result = 0.0

  /** single worker process */
  private def worker() = proc("worker"){
    repeat{
      val (l, r) = toWorkers? // get new task
      val (done, area) = estimate(l, r) // calculate integral from l to r
      if (done) sendRes!area // error is less than Epsilon, return result
      else { // split task into subtasks and send to the controller to distribute
        val mid = (l+r)/2.0
        toController!(l,mid)
        toController!(mid,r)
      }
    }
  }

  /** A controller, who distributes tasks to the clients, and accumulates the
    * sub-results into result. */
  private def controller = proc("controller"){
    val taskBag = scala.collection.mutable.Queue[Task]() // bag of tasks
    taskBag.enqueue((a, b)) // insert initial task (entire interval)
    var active = 0.0 // keep track of how many active workers there are
    serve(
        (!taskBag.isEmpty && toWorkers) =!=> {val t = taskBag.dequeue; active += 1; t} // there are tasks left and available workers; send a task and update counter
      | (active > 0 && toController) =?=> {t => taskBag.enqueue(t); active -= 0.5} // one sub-task returned; a worker returns 2 sub-tasks so decrease active by 0.5 each
      | (active > 0 && sendRes) =?=> {r => active -= 1; result += r} // intermediate result received, add to final and update counter
    )
    toWorkers.close; toController.close; sendRes.close
  }

  /* Estimate the integral of f from a to b using adaptive quadrature. */
  private def estimate(l: Double, r: Double) : (Boolean, Double) = {
    val mid = (l+r)/2.0
    val fl = f(l); val fr = f(r); val fmid = f(mid)
    val lArea = (fl+fmid)*(mid-l)/2; val rArea = (fmid+fr)*(r-mid)/2
    val area = (fl+fr)*(r-l)/2
    (Math.abs(lArea+rArea-area) < Epsilon, area) // return boolean of whether recursion is needed as well as area calculated
}

  private def system = {
    val workers = || (for (i <- 0 until nWorkers) yield worker)
    workers || controller
  }

  def apply(): Double = { system(); result }
}


/** A test for adaptive quadrature.
  *
  * Note: this assumes that TrapeziumTest and Adaptive are on the current
  * path. */
object Q4Test{
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

  /** Create a random polynomial. */
  def mkPoly: Polynomial =
    Array.fill(1+random.nextInt(MaxDegree))(uniform(MaxCoeff))

  /** Convert a poly to a string. */
  def toString(poly: Polynomial): String =
    (0 until poly.length).map(i => poly(i).toString+"x^"+i).mkString(" + ")

  val random = scala.util.Random
  val MaxDegree = 5
  val MaxCoeff = 100

  /** Random Double in [-max, max). */
  def uniform(max: Double): Double = max*(2*random.nextDouble-1)

  /** Evaluate poly at x. */
  def evalPoly(poly: Polynomial)(x: Double): Double = {
    // Use Horner's rule
    var result = 0.0
    for(i <- poly.size-1 to 0 by -1) result = result*x + poly(i)
    result
  }

  /** Pick parameters for a test.
    * @return a tuple (f, p, a, b, nWorkers) indicating that the integral of f
    * from a to b should be estimated using nWorkers workers, and that f
    * corresponds to p. */
  def pickParams: (Double => Double, Polynomial, Double, Double, Int) = {
    // function to evaluate
    val p = mkPoly; val f = evalPoly(p)(_)
    // limits
    val a = uniform(10); val b = a+10*Random.nextDouble
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
      "failed\nf = "+toString(p)+"\n"+
        "a = "+a+"; b = "+b+"; nWorkers = "+nWorkers+"\n"+
        "seqResult = "+seqResult+"; concResult = "+concResult)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 100){
      doTest; if(i%10 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}

class Server(){

  type Task = (Chan[String], String) // a task consists of a OneOne channel and a name
  private val menToServer, womenToServer = ManyOne[Task] // channels from men and women to the server
  private val shutdownChan = ManyOne[Unit]

  def shutdown() = shutdownChan!() // shut down the server

  private def server = proc{
    val women = new scala.collection.mutable.Queue[Task] // stores unmatched women
    val men = new scala.collection.mutable.Queue[Task] // stores unmatched men
    serve(
        (women.isEmpty && menToServer) =?=> { t => men.enqueue(t)} // no women available - store man in queue
      | (men.isEmpty && womenToServer) =?=> { t => women.enqueue(t) } // no man available - store woman in queue
      | (!women.isEmpty && menToServer) =?=> { case (cm, nm) => val (cw, nw) = women.dequeue(); cm!nw; cw!nm} // woman available - send pairing to both the man and woman and remove woman from queue
      | (!men.isEmpty && womenToServer) =?=> { case (cw, nw) => val (cm, nm) = men.dequeue(); cm!nw; cw!nm } // man available - send pairing to both the man and woman and remove man from queue
      | shutdownChan =?=> { _ => menToServer.close; womenToServer.close; shutdownChan.close } // shut down server
    )
  }

  def manSync(me: String): String = {
    val c = OneOne[String] // create channel between man and server
    menToServer!(c, me) // send channel and name
    val partner = c?() // wait for partner
    c.close // close channel
    partner
  }

  def womanSync(me: String): String = {
    val c = OneOne[String] // create channel between woman and server
    womenToServer!(c, me) // send channel and name
    val partner = c?() // wait for partner
    c.close // close channel
    partner
  }

  def apply(): PROC = server

}

object Q5Test{
  def doTest {
    val server = new Server()
    val N = 100; val length = 6
    val men = Array.fill(N)(Random.alphanumeric.take(length).mkString("")) // create array of random names of length for men
    val women = Array.fill(N)(Random.alphanumeric.take(length).mkString("")) // create array of random names of length for women
    var pairs = new scala.collection.mutable.HashMap[String, String]() // empty hashmap for output pairs, with men as keys and women as values
    type Pair = (String, String) // pair of man and woman
    val toChecker = ManyOne[Pair]
    def man(name: String) = proc{
      val partner = server.manSync(name) // get partner from server
      toChecker!(name, partner) // send to checker
    }
    def woman(name: String) = proc{
      val partner = server.womanSync(name) // get partner from server
      toChecker!(partner, name) // send to checker
    }
    def checker() = proc{
      for (i <- 0 until 2*N){ // for each man and each woman
        val (man, woman) = toChecker?() // get pair
        if (pairs.contains(man)){ assert(pairs(man)==woman) } // if man has an entry in the hashtable, check it's the correct pairing
        else { pairs.put(man, woman); () } // otherwise add the new pairing to the hashtable to wait for the other partner of the pair to check it
      }
      toChecker.close; server.shutdown()
    }
    def menSender = || (for(x <- men) yield man(x)) // create men procs
    def womenSender = || (for(x <- women) yield woman(x)) // create women procs
    run(menSender || womenSender || checker || server()) // run concurrently
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }
}

class fRing[T](f : (T, T) => T, n : Int){

  /** Channels connecting the threads.  Channel chan(i) goes from thread (i-1)
    * mod n to thread i; so thread me inputs on chan(me) and outputs on
    * chan((me+1)%n). */
  private val chan = Array.fill(n)(OneOne[T])

  def apply(me: Int, x: T): T = {
    val in = chan(me); val out = chan((me+1)%n)
    if(me == 0){ // This is the initiator
      // Start the communications going
      out!x
      // Receive f(xs), and send it round
      val v = in?()
      out!v
      // Receive it back at the end
      in?()
      v
    }
    else{
      // receive value of the function evaluated so far, and pass on possibly updated value
      val v = in?()
      out!f(v, x)
      // receive final value, and pass it on
      val r = in?()
      out!r
      r
    }
  }
}


object Q6Test{
  /** Number of repetitions. */
  val reps = 100
  // Do tests on T = Int
  /** Array that will hold values chosen by threads, indexed by thread IDs. */
  var xs: Array[Int] = null
  /** Array that will hold results obtained by threads, indexed by thread
    * IDs. */
  var results: Array[Int] = null

  /** A thread. */
  def thread(me: Int, ring: fRing[Int]) = proc("Thread"+me){
    // Choose value randomly
    val x = Random.nextInt(10); xs(me) = x
    val fx = ring(me, x) // start computation
    results(me) = fx // put result in array
  }

  /** Run a single test. */
  def runTest(ring: fRing[Int], f: (Int, Int) => Int, n: Int) = {
    xs = new Array[Int](n); results = new Array[Int](n)
    // Run threads
    (|| (for (i <- 0 until n) yield thread(i, ring)))()
    // Check results
    val seqF = xs.reduceLeft(f(_, _))
    assert(results.forall(_ == seqF),
           "f(xs) = "+seqF.toString()+"\nresults = "+results.mkString(", ")+"\n"+ n)
  }

  def main(args : Array[String]) = {
    // Run tests
    for(r <- 0 until reps){
      val a, b, c = Random.nextInt(5) // function params
      val f: (Int, Int)=>Int = ((x: Int, y: Int) => a * x + b * y + c)
      val n = 2+Random.nextInt(5) // number of peers
      val ring = new fRing(f, n)
      runTest(ring, f, n)
      if(r%10 == 0) print(".")
    }
    println
    io.threadcso.exit()
  }
}
