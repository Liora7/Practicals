import io.threadcso._
import scala.language.postfixOps
import scala.util.Random
import scala.collection.mutable.Stack
import ox.cads.testing._
import io.threadcso.debug.Log

class ServerTotalStack[T]{
  private val pushChan = ManyOne[T]

  private val popChan = OneMany[Option[T]]

  private def server = proc{
    val stack = new Stack[T]
    serve(
      pushChan =?=> { x => stack.push(x) } // push request
      | popChan =!=> { if(stack.nonEmpty) Some(stack.pop) else None } //pop request
    )
  }

  server.fork

  /** Push x onto the stack. */
  def push(x: T) = pushChan!x

  /** Optionally pop a value from the stack.
    * @return Some(x) where x is the value popped, or None if the stack is empty. */
  def pop: Option[T] = popChan?()

  def shutdown = { pushChan.close; popChan.close }
}


/** A linearizability tester for total queues (i.e. queues that do not
  * block, and such that dequeues on the empty queue return None). */
object StackTest{
  var iters = 200  // Number of iterations by each worker
  val MaxVal = 20 // Maximum value placed in the queue
  var pushProb = 0.3 // probability of doing an enqueue

  type SeqStack = scala.collection.immutable.List[Int]
  type ConcStack = ServerTotalStack[Int]

  def seqPush(x: Int)(s: SeqStack) : (Unit, SeqStack) =
    ((), x :: s)
  def seqPop(s: SeqStack) : (Option[Int], SeqStack) =
    if(s.isEmpty) (None, s)
    else{ (Some(s.head), s.tail) }

  /** A worker for the LinTesters */
  def worker(me: Int, log: GenericThreadLog[SeqStack, ConcStack]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt+me*45207)
    for(i <- 0 until iters)
      if(random.nextFloat <= pushProb){
        val x = random.nextInt(MaxVal)
        log.log(_.push(x), "push("+x+")", seqPush(x))
      }
      else log.log(_.pop, "pop", seqPop)
  }

  def main(args: Array[String]) = {
    // parse arguments
    var i = 0; var queueType = "server"
    val p = 4      // Number of workers
    var reps = 100  // Number of repetitions
    while(i < args.length) args(i) match{
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--pushProb" => pushProb = args(i+1).toDouble; i += 2
      case arg => println("Unrecognised argument: "+arg); sys.exit
    }

    for(r <- 0 until reps){
      // The shared concurrent queue
      val concStack = queueType match{
        case "server" => new ServerTotalStack[Int]
      }
      val seqStack = List[Int]()
      val tester = LinearizabilityTester.JITGraph[SeqStack,ConcStack](
        seqStack, concStack, p, worker _, iters)
      assert(tester() > 0)
      concStack.shutdown

      if(r%10 == 0) print(".")
    } // end of for loop
    println; io.threadcso.exit
  }
}

class TreeSync(n: Int){
  /** Channels leading up and down the tree.  Each array is indexed by the
    * child's identity. */
  private val up, down = Array.fill(n)(OneOne[Unit])

  def sync(me: Int) = {
    // Identities of the two children
    val child1 = 2*me+1; val child2 = 2*me+2
    if(child1 < n) up(child1)?() // wait for sync request from both children
    if(child2 < n) up(child2)?()
    // Send sync to parent, and wait for overall sync to return
    if(me != 0){
      up(me)!()
      down(me)?
    }
    // return sync to children
    if(child1 < n) down(child1)!()
    if(child2 < n) down(child2)!()
  }
  def shutdown = { up.foreach(x => x.close); down.foreach(x => x.close) }
}

object BarrierTest{
  val iters = 100 // # iterations per test
  val reps = 500

  // Events to place in Log
  abstract class LogEvent
  // Thread id calls sync or returns from sync
  case class Arrive(id: Int) extends LogEvent
  case class Leave(id: Int) extends LogEvent

  /** Run a single test. */
  def doTest = {
    val p = 1+Random.nextInt(20) // # threads
    val barrier = new TreeSync(p)
    val log = new debug.Log[LogEvent](p)
    def worker(me: Int) = proc{
      for(_ <- 0 until iters){
        log.add(me, Arrive(me)); barrier.sync(me); log.add(me, Leave(me))
      }
    }
    run(|| (for (i <- 0 until p) yield worker(i)))
    barrier.shutdown
    checkLog(log.get, p)
  }

  /** Check that es represents a valid history for p threads. */
  def checkLog(es: Array[LogEvent], p: Int) = {
    // We traverse the log, keeping track of which threads are currently
    // within a sync, respectively waiting to synchronise or leaving; we use a
    // bitmap for each of these sets.
    var waiting, leaving = new Array[Boolean](p)
    var numWaiting, numLeaving = 0 // # currently waiting, leaving
    for(i <- 0 until es.length){
      es(i) match{
        case Arrive(id) =>
          assert(!waiting(id)); waiting(id) = true; numWaiting += 1
          if(numWaiting == p){ // all now can leave
            assert(numLeaving == 0); leaving = waiting; numLeaving = p
            waiting = new Array[Boolean](p); numWaiting = 0
          }
        case Leave(id) =>
          assert(leaving(id)); leaving(id) = false; numLeaving -= 1
      }
    }
  }

  def main(args: Array[String]) = {
    for(r <- 0 until reps){ doTest; if(r%10 == 0) print(".") }
    println; exit
  }
}

class PrefixSums(n: Int, a: Array[Int]){
  require(n == a.size)

  /** Shared array, in which sums are calculated. */
  private val sum = new Array[Int](n)

  /** Barrier synchronisation object. */
  private val barrier = new Barrier(n)

  /** shared vars on which values are communicated, indexed by receiver's identity. */
  private val toSummers = new Array[Int](n)

  /** An individual thread.  summer(me) sets sum[me] equal to sum(a[0..me]). */
  private def summer(me: Int) = proc("summer"+me){
    // Invariant: gap = 2^r and s = sum a(me-gap .. me]
    // (with fictious values a(i) = 0 for i < 0).  r is the round number.
    var r = 0; var gap = 1; var s = a(me)

    while(gap<n){
      if(me+gap < n) toSummers(me+gap) = s // pass my value up the line
      barrier.sync()                // make sure all summers updated their shared variable before any summers start reading values
      if(gap <= me){                // receive from me-gap,
	       val inc = toSummers(me)   // inc = sum a(me-2*gap .. me-gap]
	       s = s + inc              // s = sum a(me-2*gap .. me]
      }
      r += 1; gap += gap           // s = sum a(me-gap .. me]
      barrier.sync()
    }
    sum(me) = s
  }

  /** Calculate the prefix sums. */
  def apply(): Array[Int] = {
    (|| (for (i <- 0 until n) yield summer(i)))()
    sum
  }
}

object PrefixSumsTest{
  val reps = 1000

  /** Do a single test. */
  def doTest = {
    // Pick random n and array
    val n = 1+Random.nextInt(20)
    val a = Array.fill(n)(Random.nextInt(100))
    // Calculate prefix sums sequentially
    val mySum = new Array[Int](n)
    var s = 0
    for(i <- 0 until n){ s += a(i); mySum(i) = s }
    // Calculate them concurrently
    val sum = new PrefixSums(n, a)()
    // Compare
    assert(sum.sameElements(mySum),
           "a = "+a.mkString(", ")+"\nsum = "+sum.mkString(", ")+
             "\nmySum = "+mySum.mkString(", "))
  }

  def main(args : Array[String]){
    for(r <- 0 until reps){ doTest; if(r%100 == 0) print(".") }
    println; exit
  }
}


class GridMax(n: Int, xss: Array[Array[Int]]){
  require(n >= 1 && xss.length == n && xss.forall(_.length == n))

  private val res = Array.fill[Int](n, n)(0) // array to store results - no conflicts as it is indexed by each worker

  private def worker(i: Int, j: Int, x: Int, readUp: ?[Int], writeUp: ![Int], readRight: ?[Int], writeRight: ![Int]) = proc{
    var max = x // keep track of max seen so far - init to x
    if (i==n-1) writeUp!x // if in the bottom row, send their value up to start comparisons
    for (k <- 0 until n){ // receive from all n workers in that column to get column max
      val t = readUp?() // get val from bottom worker
      if (t > max) max = t // calc new max
      writeUp!max // send updated max to top worker
    } // each worker now has its column max
    if (j==0) writeRight!max // if in the leftmost column, send their value right to start comparisons
    for (k <- 0 until n){ // receive from all n workers in that row to get total max
      val t = readRight?() // get val from left worker
      if (t > max) max = t // update max
      writeRight!max // send updated max to right worker
    } // each worker has the overall max
    res(i)(j) = max // put result in shared array
  }

  private val upChans = Array.fill[Chan[Int]](n, n)(OneOneBuf[Int](1))
  private val rightChans = Array.fill[Chan[Int]](n, n)(OneOneBuf[Int](1))

  private def system = || (for (i <- 1 until n+1; j <- 0 until n) yield worker(i-1, j, xss(i-1)(j), upChans(i-1)(j), upChans(i%n)(j), rightChans(i-1)(j), rightChans(i-1)((j+1)%n))) // toroidal grid; worker i receives from channels (i-1)(j) and sends to (i%n)(j) and (i-1)((j+1)%n)

  /** Run the system, and return array storing results obtained. */
  def apply(): Array[Array[Int]] = {system(); res}

}

class LogGridMax(n: Int, xss: Array[Array[Int]]){
  require(n >= 1 && xss.length == n && xss.forall(_.length == n))

  private val res = Array.fill[Int](n, n)(0) // array to store results - no conflicts as it is indexed by each worker
  private val barrier = Barrier(n*n) // barrier for syncing


  private def worker(i: Int, j: Int, x: Int, read: ?[Int], write: List[List [![ Int ]]]) = proc{
    var max = x // keep track of max seen so far - init to x
    val vparent = (i-1)/2 // column parent
    val vchild1 = 2*i+1; val vchild2 = 2*i+2 // children within column
    if(vchild1 < n) max = Math.max(max, read?) // if children are in valid position, receive their values and update max
    if(vchild2 < n) max = Math.max(max, read?)

    if (i!=0) { // if worker has a valid parent
      write(vparent)(j)!max  // send max to parent
      max = read?() // get column max back from parent
    }

    if(vchild1 < n) write(vchild1)(j)!max // pass column max to children
    if(vchild2 < n) write(vchild2)(j)!max

    barrier.sync // sync before starting row max

    val hparent = (j-1)/2 // row parent
    val hchild1 = 2*j+1; val hchild2 = 2*j+2 // children within row
    if(hchild1 < n) max = Math.max(max, read?) // if children are in valid position, receive their values and update max
    if(hchild2 < n) max = Math.max(max, read?)

    if (j!=0) { // if worker has a valid parent
      write(i)(hparent)!max  // send max to parent
      max = read?() // get total max back from parent
    }

    if(hchild1 < n) write(i)(hchild1)!max // pass total max on to children
    if(hchild2 < n) write(i)(hchild2)!max

    res(i)(j) = max // put result in shared array

  }

  private val chans = List.fill[Chan[Int]](n, n)(N2NBuf[Int](size = 1, readers = 1, writers = n-1))

  private def system = || (for (i <- 0 until n; j <- 0 until n) yield worker(i, j, xss(i)(j), chans(i)(j), chans)) // toroidal grid; worker i receives on channel (i)(j)

  /** Run the system, and return array storing results obtained. */
  def apply(): Array[Array[Int]] = {system(); res}

}

/** Test for GridMax and LogGridMax. */
object GridMaxTest{
  /** Run a single test.
    * @param useLog should the logarithmic version be used? */
  def doTest(useLog: Boolean) = {
    val n = 1+Random.nextInt(10)
    val xss = Array.fill[Int](n, n)(Random.nextInt(1000))
    val results = new LogGridMax(n, xss)()//if(useLog) new LogGridMax(n, xss)() else new GridMax(n, xss)()
    val expected = xss.map(_.max).max
    assert(results.forall(_.forall(_ == expected)))
  }

  /** Main method. */
  def main(args: Array[String]) = {
    val useLog = args.nonEmpty && args(0) == "--useLog"
    for(i <- 0 until 1000){
      doTest(useLog)
      if(i%100 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}

/** Smooth image a, using p workers, with at most maxIters iterations. */
class SmoothShared(a: Array[Array[Boolean]], p: Int, maxIters: Int){

  private val combBarrier = new lock.AndBarrier(p) // barrier for syncing and checking if done
  private val nRows = a.length // number of rows in the image
  private val nCols = a(0).length // number of columns in the image

  private def worker(startR: Int, endR: Int) = proc{
    var k = 0 // current iteration
    var done = false
    while (!done && k < maxIters){ // until done or max iterations reached
      var newRows = Array.ofDim[Boolean](nRows, nCols) // put subsequent result in here to avoid read/write conflicts and be able to compare against image from last iteration
      var myDone = true // to see if this worker is done
      for (i <- startR until endR; j <- 0 until nCols){ // for each pixel in the rows assigned to this worker
        newRows(i)(j) = Smooth.majority(a, i, j) // calculate new status
        myDone &&= (a(i)(j) == newRows(i)(j)) // see if there was a change to the last iteration
      }
      done = combBarrier.sync(myDone) // see if all workers are done
      if (!done){ // if the image isn't smooth yet, transfer updated rows to a
        for (r <- startR until endR){
          a(r) = newRows(r)
        }
      }
      combBarrier.sync(true) // sync so that no worker starts reading before all workers have updated a
      k+=1 // increment iteration counter
    }
  }

  private val workSize = nRows/p

  private val system = || (for (k <- 0 until p) yield worker(k*workSize, (k+1)*workSize))

  def apply() = system()

}

/** Various things shared between the sequential and concurrent smoothing
  * functions. */
object Smooth{
  type Row = Array[Boolean] // of size m
  type Image = Array[Row] // of size N

  /** Test if majority of neightbours of b(i)(j) are set. */
  def majority(b: Image, i: Int, j: Int): Boolean = {
    val n = b.length
    var sum = 0 // # set neighbours so far
    var count = 0 // # neighbours so far
    for(i1 <- i-1 to i+1; if i1 >= 0 && i1 < n;
        j1 <- j-1 to j+1; if j1 >= 0 && j1 < n){
      count += 1; if(b(i1)(j1)) sum += 1
    }
    2*sum >= count
  }

  /** Print the image in a. */
  def printArray(a: Smooth.Image) = {
    val n = a.length
    for(i <- 0 until n){
      for(j <- 0 until n) if(a(i)(j)) print("*") else print(" ");
      println;
    }
    println;
  }
}

/** A sequential smoothing algorithm. */
class SmoothSequential(a: Array[Array[Boolean]], maxIters: Int){
  private val n = a.length
  assert(a.forall(_.length == n))

  def apply() = {
    var done = false; var iters = 0

    while(!done && iters < maxIters){
      done = true
      val newA = Array.ofDim[Boolean](n, n)
      for(i <- 0 until n; j <- 0 until n){
	newA(i)(j) = Smooth.majority(a, i, j)
	done &&= (newA(i)(j) == a(i)(j))
      }
      iters += 1
      for(i <- 0 until n) a(i) = newA(i) // update for next round
    }
  }
}

/** Test of SmoothShared, comparing it with the sequential implementation. */
object SmoothSharedTest{
  val n = 40 // size of image
  val p = 10 // # workers
  val maxIters = 40 // max # iterations

  /** Do a single test. */
  def doTest = {
    val a = Array.fill(n,n)(Random.nextFloat >= 0.55)
    val a1 = a.map(_.clone)
    new SmoothShared(a, p, maxIters)()
    new SmoothSequential(a1, maxIters)()
    assert((0 until n).forall(i => a(i).sameElements(a1(i))),
           { Smooth.printArray(a); Smooth.printArray(a1); n })
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 100){
      doTest; if(true || i%10 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}


/** A bag of tasks for numerical integration from a to b, using n intervals
* and nTasks tasks. */
class BagOfTasks(a: Double, b: Double, n: Int, nTasks: Int){
require(n%nTasks == 0)

  type Task = (Double, Double, Int, Double)

  // size of each interval
  private val delta = (b-a)/n
  private val taskSize = n/nTasks // size of each task
  private val taskRange = (b-a)/nTasks // range of each task
  // Number of intervals not yet allocated.
  private var left = a // left hand boundary of next task

  /** Get a task. */
  def getTask: Task = synchronized{
    if(left < (b - taskRange/2)){ // in case of rounding errors
      val right = Math.min(left+taskRange, b)
      val task = (left, right, taskSize, delta)
      left = right
      return task
    }
    else return null
}
}

/** A collector, to sum up partial results. */
class Collector{

  private var result = 0.0

  /** Add x to the result. */
  def add(x: Double) = synchronized{
    result += x
  }
  /** Get the result. */
  def get: Double = synchronized{
    result
  }
}

class Mod3Monitor{

  private var sMod3 = 0  // sum of ids currently entered, mod 3
  private var users = new scala.collection.mutable.HashSet[Int] // set of users currently using resource

  def enter(id: Int) = synchronized{
    require(!users(id)) // user should not already be using the resource
    while(sMod3 != 0) wait() // wait until the id sum isn't divisible by 3
    sMod3 = (sMod3 + id)%3; users += id // add new reader, update sum
  }

  def exit(id: Int) = synchronized{
    require(users(id)) // user should be using the resource
    sMod3 = (sMod3 - id + 3)%3; users -= id // remove reader and update sum
    notifyAll() // notify waiting users so they can check the new sum
  }
}


object Mod3Test{
  var iters = 20  // Number of iterations by each worker

  type SeqMonitor = Set[Int] // sequential version
  type ConcMonitor = Mod3Monitor

  def seqEnter(id: Int)(m: SeqMonitor) : (Unit, SeqMonitor) =
    {var sum = 0; // calculate sum mod 3
      for (i <- m) sum += i
      require(sum%3==0); require(!m.contains(id))
      ((), m + id)} // return unit and updated monitor
  def seqExit(id: Int)(m: SeqMonitor) : (Unit, SeqMonitor) =
    {require(m.contains(id)); ((), m - id)} // return unit and updated monitor

  /** A worker for the LinTesters */
  def worker(me: Int, log: GenericThreadLog[SeqMonitor, ConcMonitor]) = {
    for(i <- 0 until iters){
      log.log(_.enter(me), "enter("+me+")", seqEnter(me)) // each worker enters and leaves
      log.log(_.exit(me), "exit("+me+")", seqExit(me))
    }
  }

  def main(args: Array[String]) = {
    // parse arguments
    var i = 0
    val p = 1      // Number of workers
    var reps = 1000  // Number of repetitions

    for(r <- 0 until reps){
      // The shared concurrent queue
      val concMonitor = new Mod3Monitor
      val seqMonitor = Set[Int]()
      val tester = LinearizabilityTester.JITGraph[SeqMonitor,ConcMonitor](
        seqMonitor, concMonitor, p, worker _, 2*iters)
      assert(tester() > 0)
      if(r%50 == 0) print(".")
    } // end of for loop
    println; io.threadcso.exit
  }
}


import scala.collection.mutable.Queue

/** A partial queue that terminates if all worker threads are attempting to
  * dequeue, and the queue is empty.
  * @param numWorkers the number of worker threads. */
class TerminatingPartialQueue[A](numWorkers: Int){

  private val queue = new Queue[A]()
  private var waiting = 0
  private var shut = false

  /** Enqueue x.*/
  def enqueue(x: A): Unit = synchronized{
    if (!shut) {queue.enqueue(x); if (waiting>0) notify()}
  }

  /** dequeue a value.*/
  def dequeue: Option[A] = synchronized{
    if (!shut && queue.isEmpty){
      if (waiting == numWorkers - 1) shutdown
      else{
        waiting += 1
        while(!queue.isEmpty && !shut) wait()
        waiting -= 1
      }
    }
    if (shut) None
    else Some(queue.dequeue)
  }

  /** Shut down this queue. */
  def shutdown = {shut = true; notifyAll()}
}

/** A class to search Graph g concurrently. */
class ConcGraphSearch[N](g: Graph[N]) extends GraphSearch[N](g){
  /**The number of workers. */
  val numWorkers = 8

  /** Try to find a path in g from start to a node that satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[List[N]] = {
    // All nodes seen so far.
    val seen = new ConcSet[N]; seen.add(start)
    // Queue storing edges and the path leading to that edge
    val queue = new TerminatingPartialQueue[(N, List[N])](numWorkers)
    queue.enqueue((start, List(start)))


    // Variable that ends up holding the result; written by coordinator.
    var result: Option[List[N]] = None

    // A single worker
    def worker = proc("worker"){
      repeat{
        var done = false
        while (!done) queue.dequeue match{
            case Some((n, path)) =>
              for(n1 <- g.succs(n)){
                if(isTarget(n1)) synchronized{
                  if(result.isEmpty){
                    result = Some(path :+ n1)
                    queue.shutdown
                    done = true
                  }
                }
                else if(seen.add(n1)) queue.enqueue((n1, path :+ n1))
              }
            case None => done = true
        }
      }
    }

    run (|| (for(_ <- 0 until numWorkers) yield worker))
    result
  }
}

/** A graph of words, corresponding to the dictionary file fName.  Two words
  * are neighbours if they differ by a single character. */
class WordGraph(fName: String) extends Graph[String]{
  /** A dictionary. */
  private val dict = scala.collection.mutable.Set[String]()

  // Initialise dict
  for(w <- scala.io.Source.fromFile(fName).getLines) dict += w

  def succs(w: String): List[String] = {
    var result = new scala.collection.mutable.ArrayBuffer[String]
    for(i <- 0 until w.length; c <- 'a' to 'z'; if c != w(i)){
      val w1 = w.patch(i,List(c),1) // replace ith character of w with c
      if(dict.contains(w1)) result += w1
    }
    result.toList
  }
}

/** A trait representing an unlabelled graph with nodes of type N. */
trait Graph[N]{
  /** The successors of node n. */
  def succs(n: N): List[N]
}

// -------------------------------------------------------

/** Trait representing a graph search problem in graph g. */
abstract class GraphSearch[N](g: Graph[N]){
  /** Try to find a path in g from start to a node that satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[List[N]]
}

/** A concurrent set with an add operation. */
class ConcSet[A]{
  /** The underlying set. */
  private val set = scala.collection.mutable.Set[A]()

  /** Add x to this set.  Return true if x was not previously in the set. */
  def add(x: A): Boolean = synchronized{ set.add(x) }
  // This implementation uses a monitor, for simplicity.
}


// -------------------------------------------------------

object WordPath{
  def main(args: Array[String]) = {
    val start = args(0); val target = args(1)
    val g = new WordGraph("knuth_words")
    val searcher: GraphSearch[String] = new ConcGraphSearch(g)
    searcher(start, (w: String) => w == target) match{
      case Some(p) => println(p.mkString(", "))
      case None => println("No solution found")
    }
    io.threadcso.exit
  }
}
