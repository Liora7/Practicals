import io.threadcso._
import ox.cads.testing._
import scala.language.postfixOps
import scala.util.Random
import scala.collection.immutable.Queue


class BufferMonitor(n: Int){
  require (n>=1)
  private var buffer = new Array[Int](n)
  private var i = 0

  private val monitor = new Monitor
  // conditions for signalling the slot is empty or non-empty, respectively.
  private val notFull, full = monitor.newCondition

  def put(item: Int) = monitor.withLock{
    notFull.await(i < n)
    buffer(i) = item; i += 1
    if (i == n) full.signal() // signal to a get
  }

  def get: Array[Int] = monitor.withLock{
    if (i < n) full.await(i == n)
    val res = buffer; buffer = new Array[Int](n)
    i = 0; notFull.signalAll() // signal to a put
    res
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


class PartnerMonitor{

  private var fName = ""; private var mName = "" // shared vars to pass names to partner
  private var mWaiting = false // man or woman proc is currently waiting for a match
  private var done = true // pairing has finished; new match can start
  private val monitor = new Monitor

  private val fWaitingC, mWaitingC, gotNamesC, doneC = monitor.newCondition
    // conditions for signalling that either a male or a female is waiting to be paired off, received their partner's name, and that the next match can begin

  def manSync(me: String): String = monitor.withLock{
    doneC.await(done) // wait for the previous match to finish
    done = false // start match; don't let another male proc start matching
    mName = me; mWaiting = true; // put name in shared var and start waiting for womanSync
    mWaitingC.signal() // signal to a woman
    fWaitingC.await(); // wait for woman to signal that she is ready
    val partner = fName // get partner's name
    gotNamesC.signal() // signal that match can now end
    partner // return partner's name
  }

  def womanSync(me: String): String = monitor.withLock{
    mWaitingC.await(mWaiting) // wait for manSync to start match
    mWaiting = false // signal to other woman procs to continue waiting
    fName = me; // put name in shared var
    fWaitingC.signal(); // signal to manSync to retrieve name
    val partner = mName // get partner's name
    gotNamesC.await() // wait for signal that match is finished
    doneC.signal(); done = true // signal to waiting procs that they can start
    partner // return partner's name
  }

}

object PartnerMonitorTest{
  def doTest {
    val server = new PartnerMonitor()
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
        if (pairs.contains(man)){ assert(pairs(man)==woman)} // if man has an entry in the hashtable, check it's the correct pairing
        else { pairs.put(man, woman);() } // otherwise add the new pairing to the hashtable to wait for the other partner of the pair to check it
      }
      toChecker.close; //server.shutdown()
    }
    def menSender = || (for(x <- men) yield man(x)) // create men procs
    def womenSender = || (for(x <- women) yield woman(x)) // create women procs
    run(menSender || womenSender || checker) // run concurrently
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

  private def sum(t: IntTree, c: ![Int]): PROC = proc{
    t match {
      case Leaf(x) => c!x // if the tree is a leaf, send its value to parent summer
      case Branch(l, r) => { // if the tree is a branch
        val c1, c2 = OneOne[Int] // create channels to pass to children summers
        var s1, s2 = 0 // vars for sums of each child
        (sum(l, c1) || sum(r, c2) || proc{s1 = c1?()} || proc{s2 = c2?()})() // concurrently run children and receive results from them
        c!(s1 + s2) // send sum of children's sums to parent
      }
    }
  }

  /** The sum of the leafs of t. */
  def apply(t: IntTree): Int = {
    var res = 0 // final res
    val r = OneOne[Int]
    (sum(t, r) || proc{res = r?()})() // start the first summer and receive final result from it
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
  require (n>=1)
  private var buffer = new Array[Int](n)
  private var i = 0

  private val notFull = MutexSemaphore()
  // semaphores for signalling the slot is full or not full, respectively.
  private val full = SignallingSemaphore()

  def put(item: Int) = {
    notFull.down; // wait until buffer is not full
    assert(i<n) // check the buffer really isn't full
    buffer(i) = item; i += 1 // put new item in the buffer
    if (i == n) full.up else notFull.up // pass the baton either to a get or another put
  }

  def get: Array[Int] = {
    full.down // wait until buffer is full
    assert (i==n) // check buffer really is full
    val res = buffer; buffer = new Array[Int](n) // get result
    i = 0; notFull.up // reset counter and pass baton to a put
    res
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

  private var sMod3 = 0 // sum of ids mod 3
  private var divBy3 = MutexSemaphore()
  private val leave = SignallingSemaphore() // semaphores to signal to enter and exit when these are allowed to proceed

  def enter(id: Int) = {
    divBy3.down // wait until the sum is divisible by 3
    sMod3 = (sMod3 + id)%3 // update sum
    if (sMod3 == 0) {divBy3.up; leave.up} else leave.up // if the sum is still divisible by 3, both enter and exit are allowed, if not, we need to wait for a thread to exit and hence pass the baton there
  }

  def exit(id: Int) = {
    leave.down // wait until the baton is passed
    sMod3 = (sMod3 - id + 3)%3 // update sum
    if (sMod3 == 0) divBy3.up else leave.up // if the sum is divisible by 3, let a new process enter, else wait for another process to exit
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


class PartnerSemaphore{

  private var fName = ""; private var mName = "" // shared vars to pass names to partner
  private var womanWaiting, manWaiting = MutexSemaphore()
  private val woman, man = SignallingSemaphore()


  def manSync(me: String): String = {
    manWaiting.down // obtain man mutex
    man.down // wait for woman to obtain woman mutex
    mName = me; woman.up // put name in shared var and signal to woman
    man.down // wait for signal from woman
    val partner = fName // get partner's name
    woman.up // signal to partner
    manWaiting.up // release man mutex
    partner // return partner's name
  }

  def womanSync(me: String): String = {
    womanWaiting.down // obtain woman mutex
    man.up // signal to man that mutex has been obtained
    woman.down // wait for signal from man
    val partner = mName // get partner's name
    fName = me // put name in shared var
    man.up // signal to man
    woman.down // wait for signal that match is finished
    womanWaiting.up // release woman mutex
    partner // return partner's name
  }

}

object PartnerSemaphoreTest{
  def doTest {
    val server = new PartnerSemaphore()
    val N = 100; val length = 6
    val men = Array.fill(N)(Random.alphanumeric.take(length).mkString("")) // create array of random names of length for men
    val women = Array.fill(N)(Random.alphanumeric.take(length).mkString("")) // create array of random names of length for women
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
      var pairs = new scala.collection.mutable.HashMap[String, String]() // empty hashmap for output pairs, with men as keys and women as values

      for (i <- 0 until 2*N){ // for each man and each woman
        val (man, woman) = toChecker?() // get pair
        if (pairs.contains(man)){ assert(pairs(man)==woman)} // if man has an entry in the hashtable, check it's the correct pairing
        else { pairs.put(man, woman);() } // otherwise add the new pairing to the hashtable to wait for the other partner of the pair to check it
      }
      toChecker.close; //server.shutdown()
    }
    def menSender = || (for(x <- men) yield man(x)) // create men procs
    def womenSender = || (for(x <- women) yield woman(x)) // create women procs
    run(menSender || womenSender || checker) // run concurrently
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }
}

class BoundedBuffer[T](n: Int){
  /** The buffer itself. */
  private val buffer = new scala.collection.mutable.Queue[T]

  /** A monitor object, to control the synchronisations. */
  private val monitor = new Monitor

  /** Semaphore that keeps track of whether buffer is full - integer state of the semaphore corresponds to n - number of items in buffer. */
  private val full = CountingSemaphore(n)

  /** Semaphore that keeps track of whether buffer is empty - integer state of the semaphore corresponds to number of items in buffer. */
  private val empty = CountingSemaphore(0)
  private val mutex = MutexSemaphore()

  /** Add x.  Blocks while the buffer is full. */
  def add(x: T) = {
    full.down // one less allowed add before buffer is full
    mutex.down // obtain mutex
    buffer.enqueue(x) // add x
    empty.up // one more allowed remove before buffer is empty
    mutex.up // give up mutex
  }

  /** Remove a value.  Blocks until the buffer is non-empty. */
  def remove: T = {
    empty.down // one less allowed remove before buffer is empty
    mutex.down // obtain mutex
    val result = buffer.dequeue // get x
    full.up // one more allowed add before buffer is full
    mutex.up // give up mutex
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
      val seqBuffer = Queue[Int]()
      val tester = LinearizabilityTester.JITGraph[SeqBuffer,ConcBuffer](
        seqBuffer, concBuffer, p, worker _, iters)
      assert(tester() > 0)

      if(r%50 == 0) print(".")
    } // end of for loop
    println; io.threadcso.exit
  }
}
