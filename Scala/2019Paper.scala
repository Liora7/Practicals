import io.threadcso._
import scala.util.Random
import ox.cads.testing._
import io.threadcso.debug.Log
import scala.language.postfixOps

trait FilterChan[A]{
  /** Synchronously send x. */
  def send(x: A)

  /** Synchronously receive a value that satisfied p. */
  def receive(p: A => Boolean): A

}

class MonitorFilter[A] extends FilterChan[A]{
  private var v = null.asInstanceOf[A]        //buffer slot for value to be sent
  private var filled = false                  //flag to check status of slot

  /** Synchronously send x. */
  def send(x: A) = synchronized{
    v = x; filled = true                      //deposit value and set flag
    notifyAll()                               //wake up all consumers so they can check
    while (filled) wait()                     //wait for consumer to take value
  }

  /** Synchronously receive a value that satisfied p. */
  def receive(p: A => Boolean): A = synchronized{
    while (!filled && !p(v)) wait()           //wait until the slot is full and v satisfies p
    val res = v;  filled = false              //retrieve value and reset flag
    notifyAll()                               //wake up sender to finish sync
    res
  }

}


class ServerFilter[A] extends FilterChan[A]{
  private val prodToServer = OneOne[(A, Chan[Unit])]
  private val consToServer = ManyOne[(A => Boolean, Chan[A])]

  /** Synchronously send x. */
  def send(x: A) = {
    val myChan = OneOne[Unit]           //create sync channel for prod with server
    prodToServer!((x, myChan))          //send value and sync chan
    myChan?()                           //wait for sync
  }

  /** Synchronously receive a value that satisfied p. */
  def receive(p: A => Boolean): A = {
    val myChan = OneOne[A]              //create reply channel for cons with server
    consToServer!((p, myChan))          //send predicate and reply chan
    myChan?()                           //wait for reply
  }

  private def sever: PROC = proc{
    var v = null.asInstanceOf[A]; var prod = null.asInstanceOf[Chan[Unit]]    //store waiting sender
    var waitingProd = false     //whether or not a sender is waiting
    var waitingCons = new scala.collection.mutable.Queue[(A => Boolean, Chan[A])]     //queue of waiting receivers
    def findCons(x: A): Option[Chan[A]] = {     //helper function to find a consumer who can receive x, if one exists
      if (waitingCons.isEmpty) return None
      else{
        val cons = waitingCons.removeFirst(cc => cc._1(x))
        cons match{
          case Some((p, c)) => return Some(c)
          case None => return None
        }
      }
    }
    serve(
      prodToServer =?=> {case (x, prodChan) => findCons(x) match{   //check if a consumer can receive x; if yes, send it and sync with prod, else let proc wait
                                case Some(cons) => {prodChan!(); cons!x}
                                case None => {v=x; prod=prodChan; waitingProd = true}
                              }
                          }
      | consToServer =?=> {case (p, consChan) => if (waitingProd && p(v)) {    waitingProd = false; consChan!v; prod!()}          //check if prod is waiting and cons can receive his value; if yes, send it, otherwise let cons wait
          else {waitingCons.enqueue((p, consChan))}}
    )
  }

}


class SemaphoreFilter[A] extends FilterChan[A]{
  private var mutex = MutexSemaphore()
  private var waitingCons = new scala.collection.mutable.Queue[(A => Boolean, Semaphore)] //waiting consumers
  var v = null.asInstanceOf[A]; var prod = null.asInstanceOf[Semaphore]    //store waiting sender
  var waitingProd = false     //whether or not a sender is waiting

  private def findCons(x: A): Option[Semaphore] = {     //helper function to find a consumer who can receive x, if one exists
    if (waitingCons.isEmpty) return None
    else{
      val cons = waitingCons.removeFirst(cc => cc._1(x))
      cons match{
        case Some((p, c)) => return Some(c)
        case None => return None
      }
    }
  }

  /** Synchronously send x. */
  def send(x: A) = {
    mutex.down        //obtain mutex
    v = x             //store value, signalling semaphore, and set flag
    val mySem = SignallingSemaphore() //syncing semaphore
    waitingProd=true
    prod = mySem
    val c = findCons(x)     //call helper function to see if some consumer can receive the value
    c match{
      case Some(cons) => {cons.up; mutex.up; mySem.down}    //send value to receiver and wait for sync
      case None => {mutex.up; mySem.down}   //no cons found; wait to be notified on syncing semaphore
    }
  }

  /** Synchronously receive a value that satisfied p. */
  def receive(p: A => Boolean): A = {
    mutex.down      //obtain mutex
    if (waitingProd && p(v)){       //if prod waiting and value fits p, receive value and wake up prod
      val res = v
      prod.up
      waitingProd = false
      mutex.up
      return res
    }
    else {
      val mySem = SignallingSemaphore()
      waitingCons.enqueue((p, mySem))     //else start waiting in queue
      mutex.up                            //give up mutex before waiting
      mySem.down                          //woken by prod
      mutex.down                          //reobtain mutex
      val res = v                         //receive value and reset flag
      prod.up
      waitingProd = false
      mutex.up
      return res
    }
  }

}

object Q2{
  type Matrix = Array[Array[Double]]

  /** Add alpha times row r2 to row r1 of m. */
  def addRow(m: Matrix, r1: Int, r2: Int, alpha: Double) = {
    val row1 = m(r1); val row2 = m(r2); assert(row1.length == row2.length)
    for(j <- 0 until row1.length) row1(j) += alpha*row2(j)
  }

  /** The row number for the pivot for column d: i.e. the row number of the entry
  * on or below the diagonal in column d with largest absolute value. */
  def maxEntry(m: Matrix, d: Int) = (d until m.length).maxBy(r => Math.abs(m(r)(d)))

  /** Perform Gaussian elimination on m, sequentially. */
  def seqElim(m: Matrix) = {
    val len = m.length
    for(d <- 0 until len-1){
      val pivotR = maxEntry(m, d) // Pivot row
      val t = m(d); m(d) = m(pivotR); m(pivotR)= t // Swap pivot row with row d
      // Subtract multiple of row d from each lower row
      val pivot = m(d)(d); assert(pivot != 0)
      for(r <- d+1 until len) addRow(m, r, d, -m(r)(d)/pivot)
    }
  }

  /** Perform Gaussian elimination on m, concurrently, using p threads. */
  def concElim(m: Matrix, p: Int) = {
    val combBarrier = new CombiningBarrier[Double](p, 0, ((_:Double)+(_:Double)))

    def worker(id: Int): PROC = proc{
      val len = m.length
      for(d <- 0 until len-1){
        var pivot = 0.0
        if (id==0){
          val pivotR = maxEntry(m, d) // Pivot row
          val t = m(d); m(d) = m(pivotR); m(pivotR)= t // Swap pivot row with row d
          // Subtract multiple of row d from each lower row
          pivot = m(d)(d); assert(pivot != 0)
        }
        pivot = combBarrier.sync(pivot)
        val numRows = len - (d+1)
        val workChunk = numRows/p
        val startR = id*workChunk + d+1; val endR = (id+1)*workChunk + d+1
        for(r <- startR until endR) addRow(m, r, d, -m(r)(d)/pivot)
        combBarrier.sync(0)
      }
    }

    val workers = || (for (i <- 0 until p) yield worker(i))

    workers()

  }

  /** Perform Gaussian elimination on m, concurrently, using p threads. */
  def concElim2(m: Matrix, p: Int) = {
    val pivotBarrier = new CombiningBarrier[Double](p, 0, ((_:Double)+(_:Double)))
    val maxBarrier = new CombiningBarrier[Int](p, 0, ((x:Int, y:Int) => if (x<y) y else x))

    def worker(id: Int): PROC = proc{
      val len = m.length
      for(d <- 0 until len-1){
        val numRows = len-1 - d
        val workChunk = numRows/p
        var startR = id*workChunk + d; var endR = (id+1)*workChunk + d
        var pivot = 0.0
        var pivotR = (startR until endR).maxBy(r => Math.abs(m(r)(d)))
        pivotR = maxBarrier.sync(pivotR)
        if (id==0){
          val t = m(d); m(d) = m(pivotR); m(pivotR)= t // Swap pivot row with row d
          // Subtract multiple of row d from each lower row
          pivot = m(d)(d); assert(pivot != 0)
        }
        pivot = pivotBarrier.sync(pivot)
        startR += 1; endR += 1
        for(r <- startR until endR) addRow(m, r, d, -m(r)(d)/pivot)
        pivotBarrier.sync(0)
      }
    }

    val workers = || (for (i <- 0 until p) yield worker(i))

    workers()

  }

}
