import io.threadcso._
import scala.util.Random
import ox.cads.testing._
import io.threadcso.debug.Log
import scala.language.postfixOps


class BufferMonitor(n: Int){

  private var buffer = new Array[Int](n)
  private var i = 0

  def put(item: Int) = synchronized{
    while (i==n) wait()
    buffer(i) = item; i+=1
    if (i==n) notifyAll()
  }

  def get: Array[Int] = synchronized{
    while(i<n) wait()
    val arr = buffer
    buffer = new Array[Int](n); i=0
    notifyAll()
    return arr
  }

}

object BufferMonitorTest{
  var iters = 200 // # iterations by each worker
  var maxValue = 20 // max value added to the stack
  var reps = 100 // # runs
  var n = 4 // Size of each buffer; also the number of producers (to avoid deadlock).
  // Sequential specification type. Note the sequential specification type
  // needs to be immutable, and comparable using ”==”, so we can’t use an
  // array here.
  type S = List[Int]
  // Sequential put operation
  def seqPut(x: Int)(buffer: S): (Unit, S) = {
    require(buffer.length < n); ((), buffer :+ x)
  }
  // It might be better to build buffer in reverse, to avoid the O(n) ”:+”
  // Sequential get operation.
  def seqGet(buffer: S): (List[Int], S) = {
    require(buffer.length == n); (buffer, List[Int]())
  }
  // A worker thread
  def worker(me: Int, log: GenericThreadLog[S,BufferMonitor]) = {
    val random = new scala.util.Random
    for(i <- 0 until iters){
      if(me == 0) // consumer
      log.log(_.get.toList, "get", seqGet)
      else{
        val x = random.nextInt(maxValue)
        log.log(_.put(x), "put("+x+")", seqPut(x))
      }
    }
  }
  // cast the result to a List, to allow comparison using ”==”.
  // The main method
  def main(args: Array[String]) = {
    for(i <- 0 until reps){
      val concBuffer = new BufferMonitor(n)
      val seqBuffer = List[Int]()
      val tester = LinearizabilityTester.JITGraph[S, BufferMonitor](
        seqBuffer, concBuffer, n+1, worker _, iters)
        assert(tester() > 0)
        if(i%10 == 0) print(".")
      }
      println; exit
    }
}


class MatchMakerMonitor{

  private val monitor = new Monitor
  private val doneC, mWaitingC, fWaitingC, gotNamesC = monitor.newCondition
  private var manName, womanName = ""
  private var done = true; private var mWaiting = false

  def manSync(me: String): String = monitor.withLock{
    doneC.await(done) // wait for the previous match to finish
    done = false // start match; don't let another male proc start matching
    manName = me; mWaiting = true; // put name in shared var and start waiting for womanSync
    mWaitingC.signal() // signal to a woman
    fWaitingC.await(); // wait for woman to signal that she is ready
    val partner = womanName // get partner's name
    gotNamesC.signal() // signal that match can now end
    partner // return partner's name
  }

  def womanSync(me: String): String = monitor.withLock{
    mWaitingC.await(mWaiting); mWaiting = false
    val partner = manName
    womanName = me
    fWaitingC.signal()
    gotNamesC.await()
    doneC.signal(); done = true
    partner
  }

}

object MatchMakerTest{
  // Number of elements; range of input values.
  val N = 100; val Length = 10
  def doTest = {
    val matcher = new MatchMakerMonitor()
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
    run(menProcs || womenProcs) // run concurrently
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }
}

abstract class IntTree
case class Leaf(n: Int) extends IntTree
case class Branch(l: IntTree, r: IntTree) extends IntTree

object TreeSum{

  private def sum(t: IntTree, parent: ![Int]): PROC = proc{
    var s = 0
    t match{
      case Leaf(x) => s = x
      case Branch(l, r) => {
        val myChan = ManyOne[Int]
        def rec = proc{ s = myChan?() + myChan?() }
        run(sum(l, myChan) || sum(r, myChan) || rec)
      }
    }
    parent!s
  }


  /** The sum of the leafs of t. */
  def apply(t: IntTree): Int ={
    val out = OneOne[Int]
    var res = 0
    def rec = proc{res = out?()}
    run(rec || sum(t, out))
    res
  }
}

object TreeSumTest{
  /** Produce a random tree.
    * @param w the reciprocal of the probability of producing a Leaf. */
  def makeTree(w: Int): IntTree = {
    if(Random.nextInt(w) == 0) return new Leaf(Random.nextInt(100))
    else return new Branch(makeTree(w-1), makeTree(w-1))
  }

  /** Sequential tree sum. */
  def treeSum(t: IntTree): Int = t match{
    case Leaf(n) => n
    case Branch(l, r) => treeSum(l) + treeSum(r)
  }

  /** A single test. */
  def doTest = {
    val t = makeTree(4); val seqSum = treeSum(t); val concSum = TreeSum(t)
    assert(seqSum == concSum)
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100000){
      doTest; if(i%1000 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}


class BufferSemaphore(n: Int){

  private var buffer = new Array[Int](n)
  private var i = 0
  private val full = SignallingSemaphore()
  private val notFull = MutexSemaphore()

  def put(item: Int) = {
    notFull.down
    buffer(i) = item; i+=1
    if (i==n) full.up else notFull.up
  }

  def get: Array[Int] = {
    full.down
    val arr = buffer
    buffer = new Array[Int](n); i=0
    notFull.up
    return arr
  }

}

object BufferSemaphoreTest{
  var iters = 200 // # iterations by each worker
  var maxValue = 20 // max value added to the stack
  var reps = 100 // # runs
  var n = 4 // Size of each buffer; also the number of producers (to avoid deadlock).
  // Sequential specification type. Note the sequential specification type
  // needs to be immutable, and comparable using ”==”, so we can’t use an
  // array here.
  type S = List[Int]
  // Sequential put operation
  def seqPut(x: Int)(buffer: S): (Unit, S) = {
    require(buffer.length < n); ((), buffer :+ x)
  }
  // It might be better to build buffer in reverse, to avoid the O(n) ”:+”
  // Sequential get operation.
  def seqGet(buffer: S): (List[Int], S) = {
    require(buffer.length == n); (buffer, List[Int]())
  }
  // A worker thread
  def worker(me: Int, log: GenericThreadLog[S,BufferSemaphore]) = {
    val random = new scala.util.Random
    for(i <- 0 until iters){
      if(me == 0) // consumer
      log.log(_.get.toList, "get", seqGet)
      else{
        val x = random.nextInt(maxValue)
        log.log(_.put(x), "put("+x+")", seqPut(x))
      }
    }
  }
  // cast the result to a List, to allow comparison using ”==”.
  // The main method
  def main(args: Array[String]) = {
    for(i <- 0 until reps){
      val concBuffer = new BufferSemaphore(n)
      val seqBuffer = List[Int]()
      val tester = LinearizabilityTester.JITGraph[S, BufferSemaphore](
        seqBuffer, concBuffer, n+1, worker _, iters)
        assert(tester() > 0)
        if(i%10 == 0) print(".")
      }
      println; exit
    }
}

class Mod3Semaphore{

  private var users = new scala.collection.mutable.HashSet[Int]
  private var sum3 = 0
  private val zero = MutexSemaphore()
  private val leave = SignallingSemaphore()


  def enter(id: Int) = {
    zero.down
    assert(!users.contains(id))
    sum3 += id%3
    users += id
    if (sum3==0) {zero.up; leave.up} else leave.up
  }

  def exit(id: Int) = {
    leave.down
    assert(users.contains(id))
    sum3 = (sum3 - id + 3)%3
    users -= id
    if (sum3==0) {zero.up; leave.up} else leave.up
  }

}

object Mod3Test{
  var iters = 20  // Number of iterations by each worker

  type SeqMonitor = Set[Int] // sequential version
  type ConcMonitor = Mod3Semaphore

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
      val concMonitor = new Mod3Semaphore
      val seqMonitor = Set[Int]()
      val tester = LinearizabilityTester.JITGraph[SeqMonitor,ConcMonitor](
        seqMonitor, concMonitor, p, worker _, 2*iters)
      assert(tester() > 0)
      if(r%50 == 0) print(".")
    } // end of for loop
    println; io.threadcso.exit
  }
}

/** A bounded buffer implemented with semaphores. */
class BoundedBuffer[T](bound: Int){
  /** The buffer itself. */
  private val buf = new scala.collection.mutable.Queue[T]


  /** Condition for signalling that the buffer is not full. */
  private val emptySlots = CountingSemaphore(bound)
  private val fullSlots = CountingSemaphore(0)
  private val mutex = MutexSemaphore()


  /** Enqueue x.  Blocks while the queue is full. */
  def add(x: T) = {
    emptySlots.down
    mutex.down
    buf.enqueue(x)
    fullSlots.up
    mutex.up
  }

  /** Dequeue a value.  Blocks until the queue is non-empty. */
  def remove: T = {
    fullSlots.down
    mutex.down
    val result = buf.dequeue
    emptySlots.up
    mutex.up
    result
  }
}


/** A linearizability tester for bounded buffers. */
object BoundedBufferTest{
  var iters = 200  // Number of iterations by each worker
  val MaxVal = 20 // Maximum value placed in the buffer
  var addProb = 0.3 // probability of doing an add
  var bound = 10 // the bound on the length of the buffer

  type SeqBuffer = scala.collection.immutable.Queue[Int]
  type ConcBuffer = BoundedBuffer[Int]

  def seqAdd(x: Int)(q: SeqBuffer) : (Unit, SeqBuffer) = {
    require(q.length < bound); ((), q.enqueue(x))
  }
  def seqRemove(q: SeqBuffer) : (Int, SeqBuffer) = {
    require(q.nonEmpty); q.dequeue
  }

  /** A worker for the LinTesters */
  def worker(me: Int, log: GenericThreadLog[SeqBuffer, ConcBuffer]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt+me*45207)
    for(i <- 0 until iters){
      if(me%2 == 0){
        val x = random.nextInt(MaxVal)
        log.log(_.add(x), "add("+x+")", seqAdd(x))
      }
      else log.log(_.remove, "remove", seqRemove)
    }
  }

  def main(args: Array[String]) = {
    val p = 4      // Number of workers
    var reps = 10000  // Number of repetitions

    for(r <- 0 until reps){
      // The shared concurrent buffer
      val concBuffer = new BoundedBuffer[Int](bound)
      val seqBuffer = scala.collection.immutable.Queue[Int]()
      val tester = LinearizabilityTester.JITGraph[SeqBuffer,ConcBuffer](
        seqBuffer, concBuffer, p, worker _, iters)
      assert(tester() > 0)

      if(r%50 == 0) print(".")
    } // end of for loop
    println; io.threadcso.exit
  }
}

class MatchMakerSemaphore{

  private var manName, womanName = ""
  private val mutex = MutexSemaphore()
  private val woman, man, done = SignallingSemaphore()

  def manSync(me: String): String = {
    mutex.down
    manName = me
    woman.up
    man.down
    val partner = womanName
    done.up
    partner
  }

  def womanSync(me: String): String = {
    woman.down
    val partner = manName
    womanName = me
    man.up
    done.down
    mutex.up
    partner
  }

}


object MatchMakerSemTest{
  // Number of elements; range of input values.
  val N = 100; val Length = 10
  def doTest = {
    val matcher = new MatchMakerSemaphore()
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
    run(menProcs || womenProcs) // run concurrently
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }
}
