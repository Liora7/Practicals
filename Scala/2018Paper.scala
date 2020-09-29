import io.threadcso._
import scala.util.Random
import ox.cads.testing._
import io.threadcso.debug.Log
import scala.language.postfixOps


object Q1{
  type Rectangular[T] = Array[Array[T]]
  type Image = Rectangular[Boolean]
  type Label = (Int, Int)
  type Labelling = Rectangular[Label]

  def minLabel(l1: Label, l2: Label): Label = { // return smaller of the two labels
    if (l1._1 < l2._1 || l1._1 == l2._1 && l1._2 < l2._2) return l1
    else return l2
  }

  def labellingSeq(im: Image): Labelling = {
    val numRows = im.size
    val numCols = im(0).size


    val labels = Array.ofDim[Label](numRows, numCols) // create new labelling
    for (r <- 0 until numRows; c <- 0 until numCols){
      labels(r)(c) = (r,c) // init labels to pixel coordinates
    }

    def leastNeighbor(r: Int, c: Int): (Int, Int) = { // find smallest neighbor of black pixel (r,c)
      var least = (r, c)
      for (dr <- -1 to 1 by 2){ //check vertical neighbors
        if (dr < numRows && im(dr)(c)){
          least = minLabel(least, labels(dr)(c))
        }
      }
      for (dc <- -1 to 1 by 2){ // check horizontal neighbors
        if (dc < numCols && im(r)(dc)){
          least = minLabel(least, labels(r)(dc))
        }
      }
      least
    }

    var converged = false // repeat relabelling while the labels have not converged
    while (!converged){
      var changed = false // keep track if labels have been changed on this iteration
      for (r <- 0 until numRows; c <- 0 until numCols if im(r)(c)){ // replace black pixel coordinates by their least neighbor
        val newLabel = leastNeighbor(r,c)
        if (newLabel != labels(r)(c)) changed = true // label has been changed
        labels(r)(c) = leastNeighbor(r,c)
      }
      converged = !changed // update converged flag
    }
    labels
  }


  def labellingPar(im: Image, W: Int): Labelling = {
    val numRows = im.size
    val numCols = im(0).size

    val labels = Array.ofDim[Label](numRows, numCols) // create new labelling
    for (r <- 0 until numRows; c <- 0 until numCols){
      labels(r)(c) = (r,c) // init labels to pixel coordinates
    }

    val combBarrier = new lock.AndBarrier(W)

    def leastNeighbor(r: Int, c: Int): (Int, Int) = { // find smallest neighbor of black pixel (r,c)
      var least = (r, c)
      for (dr <- -1 to 1 by 2){ //check vertical neighbors
        if (dr < numRows && im(dr)(c)){
          least = minLabel(least, labels(dr)(c))
        }
      }
      for (dc <- -1 to 1 by 2){ // check horizontal neighbors
        if (dc < numCols && im(r)(dc)){
          least = minLabel(least, labels(r)(dc))
        }
      }
      least
    }

    def worker(startRow: Int, endRow: Int): PROC = proc{
      var done = false

      while (!done){
        var myDone = true

        var newLabels = Array.ofDim[Label](numRows, numCols) // put subsequent result in here to avoid read/write conflicts and be able to compare against labelling from last iteration

        for (r <- startRow until endRow; c <- 0 until numCols if im(r)(c)){ // for each black pixel in the rows assigned to this worker
          newLabels(r)(c) = leastNeighbor(r, c) // calculate new status
          myDone &&= (labels(r)(c) == newLabels(r)(c)) // see if there was a change to the last iteration
        }
        done = combBarrier.sync(myDone) // see if all workers are done
        if (!done){ // if the labelling hasn't converged yet, transfer updated rows to labels
          for (r <- startRow until endRow){
            labels(r) = newLabels(r)
          }
        }
        combBarrier.sync(true) // sync so that no worker starts reading before all workers have updated labels
      }
    }

    val rowChunk = numRows/W // size of each workers task

    def workers = || (for (i <- 0 until W) yield worker(i*rowChunk, Math.min((i+1)*rowChunk, numRows))) // split rows between workers

    workers() // run workers
    labels
  }

}

abstract class ManyToOne[T](val size: Int){
  def open: Int
  def closeOut(id: Int): Unit
  def write(id: Int, t: T): Unit
  def read: T
}

class ManyToOneMonitor[T](size: Int) extends ManyToOne[T](size){

  private var i = 0
  private var opened = Array.fill[Boolean](size)(false)
  private var closed = Array.fill[Boolean](size)(false)
  private var item: Option[T] = None
  private var filled, retrieved, allShut = false

  def open: Int = synchronized{
    assert(i<size)
    val id = i
    opened(id) = true
    i += 1
    return id
  }

  def closeOut(id: Int): Unit = synchronized{
    assert(opened(id) && !closed(id))
    closed(id) = true
    if (closed.forall(x => x==true)) allShut = true
  }

  def write(id: Int, t: T): Unit = synchronized{
    assert(opened(id) && !closed(id))
    while (filled) wait()
    item = Some(t)
    filled = true
    notifyAll()
    while (!retrieved) wait()
    retrieved = false; filled = false
    notifyAll()
  }

  def read: T = synchronized{
    while (!filled && !allShut) wait()
    if (allShut) io.threadcso.stop // all producers have closed
    val res = item match{
      case Some(x) => x
    }
    retrieved = true
    notifyAll()
    res
  }
}


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

class ManyToOneSemaphore[T](size: Int) extends ManyToOne[T](size){

  private var i = 0
  private var opened = Array.fill[Boolean](size)(false)
  private var closed = Array.fill[Boolean](size)(false)
  private var item: Option[T] = None
  private var allShut = false
  private val full, retrieved, consumer = SignallingSemaphore()
  private val mutex = MutexSemaphore()

  def open: Int = {
    mutex.down
    assert(i<size)
    val id = i
    opened(id) = true
    i += 1
    mutex.up
    return id
  }

  def closeOut(id: Int): Unit = {
    mutex.down
    assert(opened(id) && !closed(id))
    closed(id) = true
    if (closed.forall(x => x==true)) allShut = true
    mutex.up
  }

  def write(id: Int, t: T): Unit = {
    assert(opened(id) && !closed(id))
    consumer.down
    item = Some(t)
    full.up
    retrieved.down
    mutex.up
  }

  def read: T = {
    mutex.down
    if (allShut) io.threadcso.stop // all producers have closed
    consumer.up
    full.down
    val res = item match{
      case Some(x) => x
    }
    retrieved.up
    res
  }
}


trait Graph[Vertex] extends Iterable[(Vertex, Iterable[Vertex])]{
  def vertices: Iterator[Vertex]

  def iterator: Iterator[(Vertex, Iterable[Vertex])]

  def adjacent(vertex: Vertex): Iterable[Vertex]

  def edgeCount: Int

  def vertexCount: Int

  def edges: Iterator[(Vertex, Vertex)]
}

object Q3{

  def reachable[V](graph: Graph[V], start: V, W: Int): Set[V] = {
    val getTask = OneMany[V] // channel for workers to receive task from bag
    val putTask = ManyOne[V] // channel for workers to send task to bag
    val workerDidTask = ManyOne[Unit] // channel for workers to signal they finished exploring their current task
    var seen = scala.collection.immutable.HashSet[V](start) // set of vertices seen so far

    def worker: PROC = proc{
      repeat{
        val v = getTask?() // receive new task
        for (n <- graph.adjacent(v)){
          putTask!n // send each adjacent vertex to bag
        }
        workerDidTask!() // signal end of exploration
      } // terminates when bag shuts getTask channel
    }

    def bag: PROC = proc{
      val tasks = scala.collection.mutable.Queue[V](start)
      var working = 0 // number of workers currently exploring
      serve(
          (tasks.nonEmpty && getTask) =!=> {val t = tasks.dequeue(); working += 1; t} // send vertex to a worker, add it to seen, and increment working
        | (working > 0 && putTask) =?=> {t => if (!seen(t)) {tasks.enqueue(t); seen += t}} // check if vertex has already been seen; if not, add to seen and enqueue in tasks
        | (working > 0 && workerDidTask) =?=> {_ => working -= 1} // decrement working
      ) // terminates when tasks is empty and no workers are currently exploring
      getTask.close; putTask.close; workerDidTask.close //close channels
    }

    def workers = || (for (i <- 0 until W) yield worker) //create W workers

    run(workers || bag)
    seen
  }

}
