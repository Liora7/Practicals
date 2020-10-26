import io.threadcso._
import scala.util.Random
import ox.cads.testing._
import io.threadcso.debug.Log
import scala.language.postfixOps

abstract class AtomicBroadcast[A](n: Int){
  /** Put x into the buffer; called by producer. */
  def put(x: A): Unit

  /** Get a value from the buffer; called by consumer me, with 0 ≤ me < n. */
  def get(me: Int): A
}

class ServerBroadcast[A](n: Int) extends AtomicBroadcast[A](n){

  private val toServer = ManyOne[(A, Chan[Unit])]       // channel for producers to send values to server
  private val toConsumer = Array.fill(n)(OneOne[A])     //array of channels for consumers to receive values from server

  /** Put x into the buffer; called by producer. */
  def put(x: A): Unit = {
    val myChan = OneOne[Unit]
    toServer!(x, myChan)      //send x and synch channel to server
    myChan?()                 //wait for server to finish distributing x
  }

  /** Get a value from the buffer; called by consumer me, with 0 ≤ me < n. */
  def get(me: Int): A = {
    toConsumer(me)?()     //receive value from server
  }

  private def server: PROC = proc{
    def sendVal(x: A): PROC = || (for (i <- 0 until n) yield proc{toConsumer(i)!x})   //helper function to concurrently send x to each consumer exactly once

    repeat{
      val (x, syncChan) = toServer?()
      sendVal(x); syncChan!() //receive value from producer, concurrently send to all consumers, then let producer move on
    }
    toServer.close()          //close Channels when no more producers want to send values
    for (i <- 0 until n) toConsumer(i).close
  }

  server()      // init server proc upon class creation
}


class MonitorBroadcast[A](n: Int) extends AtomicBroadcast[A](n){

  private val monitor = new Monitor       //create CSO monitor to use conditions and avoid spurious wakeups
  private val slotFilled, distributedVal = monitor.newCondition   //one condition for the producer to signal his arrival, and one for the consumers to signal they are done retrieving the value
  private var consumers = 0         //count number of distinct consumers who have retrieved value
  private var retrieved = Array.fill(n)(false)      //keep track of which consumers have retrieved value
  private var filled = false      //whether or not slot is filled
  private var value = null.asInstanceOf[A]      //buffer for producer to place value into

  /** Put x into the buffer; called by producer. */
  def put(x: A): Unit = monitor.withLock{
    value = x       //place value into buffer
    filled = true     //update filled flag
    slotFilled.signalAll()      //signal to all consumers waiting for a new value
    distributedVal.await()      //wait for all consumers to finish retrieving the vlaue
    consumers = 0; retrieved = Array.fill(n)(false)   //reset consumers counter and retrieved flags
    filled = false        //update filled flag
  }

  /** Get a value from the buffer; called by consumer me, with 0 ≤ me < n. */
  def get(me: Int): A = monitor.withLock{
    if (!filled || retrieved(me)) slotFilled.await()    //if slot is empty or consumer me has already retrieved the current value, wait for the next producer
    val result = value; retrieved(me)=true     //retrieve value and set retrieved flag
    if (consumers < n) consumers += 1     //update consumers counter
    else distributedVal.signal()          // if last consumer to get value, signal to producer
    return result                         //return value
  }

}


class SemaphoreBroadcast[A](n: Int) extends AtomicBroadcast[A](n){
  private val mutex, slotEmptied = MutexSemaphore()        //mutexes for consumers and producers, respectively
  private val slotFilled, consumersLeft = SignallingSemaphore()    //signal for consumers and producer, respectively
  private var value = null.asInstanceOf[A]      //buffer for producer to place value into
  private var filled = false        //whether or not slot is filled
  private var consumers = 0         //count number of distinct consumers who have retrieved value
  private var retrieved = Array.fill(n)(false)      //keep track of which consumers have retrieved value

  /** Put x into the buffer; called by producer. */
  def put(x: A): Unit = {
    slotEmptied.down        //wait for previous producer to finish distributing his value
    value = x             //put value into buffer
    filled = true         //update filled flag
    slotFilled.up         //let waiting consumers proceed
    consumersLeft.down    //wait for consumers to finish
    consumers = 0; retrieved = Array.fill(n)(false)   //reset consumers counter and retrieved flags
    filled = false        //update filled flag
    slotEmptied.up        //signal to next producer to continue
  }

  /** Get a value from the buffer; called by consumer me, with 0 ≤ me < n. */
  def get(me: Int): A = {
    mutex.down          //obtain consumer mutex
    while (!filled || retrieved(me)) {mutex.up; slotFilled.down; slotFilled.up; mutex.down}   //if slot is empty or this consumer has already received this value, give up the mutex and wait for the next producer; then wake up the next waiting consumer and reacquire mutex
    val result = value; retrieved(me) = true    //retrieve value and update retrieved flag
    if (consumers < n) consumers += 1     //update consumers counter
    else consumersLeft.up                 //if last consumer to get value, signal to producer
    mutex.up                              //give up consumer mutex
    result                                //return result
  }

}


class TerminatingConcurrentStack[T](p: Int){

  private val stack = new scala.collection.mutable.Stack[T]()   //actual stack of values
  private val mutex = MutexSemaphore()
  private var elems = CountingSemaphore(0)          //keep track of number of elems on stack - semaphore is also used to wake up waiting consumers in case stack shuts down
  private var shut = false                          //flag for shutting down stack
  private var waiting = 0                           //counter for waiting consumers

  /** Push x onto the stack. */
  def push(x: T): Unit = {
    mutex.down                        //acquire mutex
    if (!shut){                       //push nothing if stack is shut
      stack.push(x); elems.up        //else push and increment CountingSemaphore
    }
    mutex.up                          //give up mutex
  }

  /** Pop a value from the stack; or return None if all p threads are attempting a
  * pop and the stack is empty. */
  def pop: Option[T] = {
    mutex.down                           //get mutex
    if (!shut && stack.isEmpty){         //wait for new value or for stack to shut
      if (waiting == p - 1) shutdown     //if all other p-1 threads are waiting, shut queue
      else{
        waiting += 1                     //start waiting
        mutex.up                         //give up mutex
        elems.down                       //wait for new elem on stack
        mutex.down                       //reacquire mutex
        if (shut && waiting > 0) elems.up   //check if stack has been shut; if so, wake up next waiting consumer
        waiting -= 1                     //stop waiting
      }
    }
    if (shut) {mutex.up; None}           //give up mutex and return None if stack is shut
    else {
      val res = Some(stack.pop)         //pop from stack
      mutex.up                          //give up mutex
      res
    }
  }

  /** Shut down this stack. */
  def shutdown = { shut = true }   //set shut to true

}


class Node{
  /** The successors of this node, i.e. those nodes n such that there is an edge
  * from this node to n. */
  var succs = List[Node]()

  /** Visit this node. */
  def visit () = {
  }

}

/** A graph whose start node is start. */
class Graph(val nodes: Array[Node], val start: Node)

/** A concurrent set with an add operation. */
class ConcSet[A]{
  /** The underlying set. */
  private val set = scala.collection.mutable.Set[A]()

  /** Add x to this set.  Return true if x was not previously in the set. */
  def add(x: A): Boolean = synchronized{ set.add(x) }
  // This implementation uses a monitor, for simplicity.
}


/** A class to search Graph g concurrently using p threads. */
class ConcGraphSearch(g: Graph, p: Int){

  /** execute search */
  def apply(start: Node) = {
    // Stack storing unexplored nodes reachable from start
    val stack = new TerminatingConcurrentStack[Node](p)
    // set storing nodes seen so far
    val seen = new ConcSet[Node]
    // init stack to start node
    seen.add(g.start)
    stack.push(g.start)

    // A single worker
    def worker = proc("worker"){
      var done = false        //keep track of whether we have finished exploring
      while(!done){
        val res = stack.pop   //pop from concurrent stack to either receive an unexplored node, or None if the stack is shut and we have finished exploring
        res match{
          case Some(node) => {
            node.visit()                  //call visit on the unexplored node
            for (n <- node.succs) {       // for each of the node's neighbors, add it to the seen set and push it onto the stack if it hasn't already been seen
              if (seen.add(n)) stack.push(n)
            }
          }
          case None => { done=true }      //stack is shut and graph has been explored; terminate worker process
        }
      }
    }

    || (for(_ <- 0 until p) yield worker)()   //create p workers and run them
  }
}



class Combiner[A]{
  private val monitor = new Monitor     //CSO monitor
  private val twoVals = monitor.newCondition     //condition for having both variables filled
  private var v1, v2 = null.asInstanceOf[A]     //buffer slots for the two values
  private var filled = 0    //counter of filled slots

  /** Add x as the first value. */
  def put1(x: A): Unit = monitor.withLock{
    v1 = x; filled += 1       //deposit value and increment counter
    if (filled==2) twoVals.signal()       //if both slots filled, notify consumer
  }

  /** Add y as the second value. */
  def put2(y: A): Unit = monitor.withLock{
    v2 = y; filled += 1       //deposit value and increment counter
    if (filled==2) twoVals.signal()       //if both slots filled, notify consumer
  }

  /** Get the two values, waiting if necessary. */
  def get: (A, A) = monitor.withLock{
    while (filled < 2) twoVals.await()         //wait for both slots to be filled
    (v1, v2)                //retrieve values
  }

}



class Distributor[A]{
  private val monitor = new Monitor     //CSO monitor
  private val filled = monitor.newCondition     //condition for having value deposited
  private var v = null.asInstanceOf[A]     //buffer slot for the value
  private var full = false                 //flag for having slot filled

  /** Pass x to the other threads. */
  def put(x: A): Unit = monitor.withLock{
    v = x; full = true      //deposit value and set full flag
    filled.signalAll()      //signal to waiting get1 and get2 threads
  }

  /** Get the value deposited, waiting if necessary. */
  def get1: A = monitor.withLock{
    while (!full) filled.await()      //if no value deposited yet, wait
    v                                 //return value
  }

  /** Get the value deposited, waiting, if necessary. */
  def get2: A = monitor.withLock{
    while (!full) filled.await()      //if no value deposited yet, wait
    v                                 //return value
  }
}


/** A combining barrier to be used by p threads, with initial value e, and
* combining function f. */
class CombiningBarrier[A](p: Int, e: A, f: (A,A) => A){
  private var res = e
  private val combiners = Array.fill(p-1)(Combiner[A])
  private val distributors = Array.fill(p-1)(Distributor[A])

  /** Synchronise with other threads, passing in myVal. */
  def sync(me: Int, myVal: A): A = {
    val i = (p-1 - p/2) + me / 2; val e = me % 2
    val comb = combiners(i)
    if (e) comb.put1(myVal)
    else comb.put2(myVal)

    val dist = distributors(i)
    if (e) dist.get1(myVal)
    else dist.get2(myVal)
  }

  def propagate = {
    val combs = || (for (i <- 0 until (p-1 - p/2)) yield proc{
      val c1 = 2*i+1; val c2 = 2*i+2
      val (lx, ly) = combiners(c1).get(); val (rx, ry) = combiners(c2).get()
      combiners(i).put1(f(lx, ly))
      combiners(i).put2(f(rx, ry))
    })
    val dists = || (for (i <- 0 until (p-1 - p/2)) yield proc{
      if (i==0) distributors(i).put()
      val c1 = 2*i+1; val c2 = 2*i+2
      val r = distributors(i).get1(); val (rx, ry) = distributors(c2).get()
      combiners(i).put1(f(lx, ly))
      combiners(i).put2(f(rx, ry))
    })
  }

}
