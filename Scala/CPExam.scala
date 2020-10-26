import io.threadcso._
import scala.util.Random
import ox.cads.testing._
import io.threadcso.debug.Log
import scala.language.postfixOps

object Q1{

def transform[S,T](function : S => T)(in : ?[S], out : ![T]) : PROC = proc{
  repeat{
    val x = in?()
    out!(function(x))
  }
  in.closeIn; out.closeOut
}

def fanOut[S](in : ?[S], outs : List[![S]]) : PROC = proc{
  repeat{
    val x = in?()
    alt(
      outs(0) =!=> {x}
      | outs(1) =!=> {x}
    )
  }
  in.closeIn; for(out <- outs) out.closeOut
}

def fanIn[T](ins : List[?[T]], out : ![T]) : PROC = proc{
  serve(
    ins(0) =?=> {x => out!x}
    | ins(1) =?=> {x => out!x}
  )
  for (in <- ins) in.closeIn; out.closeOut
}

def fanBalancer[S,T](workers: Int, task: S => T) (in: ?[S], out: ![T]) : PROC = proc{
  val toWorkers = List.fill(workers)(OneOne[S])
  val fromWorkers = List.fill(workers)(OneOne[T])
  val workerProcs = || (for (i <- 0 until workers) yield transform(task)(toWorkers(i), fromWorkers(i)))
  (fanOut(in, toWorkers) || workerProcs || fanIn(fromWorkers, out))()
}

def balancer[S, T](workers: Int, task: S => T)(in: ?[S], out: ![T]) : PROC = proc{
  val toWorkers = OneMany[S]
  val fromWorkers = ManyOne[T]
  var busy = 0

  val workerProcs = || (for (i <- 0 until workers) yield transform(task)(toWorkers, fromWorkers))

  def server: PROC = proc{
    serve(
      (busy < workers && in) =?=> {x => toWorkers!x; busy += 1}
      | fromWorkers =?=> {y => out!y; busy -= 1}
    )
    in.closeIn; out.closeOut; toWorkers.close; fromWorkers.close
  }

  (server || workerProcs)()

}

def insOuts[S](in0: ?[S], in1: ?[S], out0: ![S], out1: ![S], swap: ?[(Int, Int)]): PROC = proc{
  val ins = Array(in0, in1); val outs = Array(out0, out1)
  var (in, out) = (in0, out0)     //init channels to in0 and out0
  serve(
    in =?=> {x => out!x}          //copy value from current in to current out
    | swap =?=> {case (i, o) => in = ins(i); out = outs(i) }  //define new in and out
  )
  in0.closeIn; in1.closeIn; out0.closeOut; out1.closeOut; swap.closeIn
}

sealed trait ColTree[C]
case class BNode[C](colour : C, left : ColTree[C], right : ColTree[C])
extends ColTree[C]
case class LNode[C](colour : C)
extends ColTree[C]

def sequentialCollapse[C,D](bnode : (C, D, D) => D, leaf : C => D)
(colTree: ColTree[C]) : D = {
  def rec = sequentialCollapse(bnode, leaf)(_)
  colTree match {
  case BNode(colour, left, right) => bnode(colour, rec(left), rec(right))
  case LNode(colour) => leaf(colour)
  }
}

def concurrentCollapse[C,D](bnode : (C, D, D) => D, leaf : C => D)
(colTree: ColTree[C]) : D = {
  def rec = sequentialCollapse(bnode, leaf)(_)
  colTree match {
    case BNode(colour, left, right) => {
      var l, r = null.asInstanceOf[D]   //variables to hold value of left and right subtrees, respectively
      ( proc{l=rec(left)} || proc{r=rec(right)} )()   //concurrently recurse on the two subtrees and set the variables
      bnode(colour, l, r)
    }
    case LNode(colour) => leaf(colour)
  }
}

}


// class SequentialSimulation[V,C](update : List[C] => C) extends Simulation[V,C] {
//   def apply(graph: ColGraph[V,C], rounds : Int) : ColGraph[V,C] = {
//     require(rounds >= 0)
//     var currentGraph = graph
//     for (_ <- 1 to rounds) {
//       val newColours = mutable.Map[V, C]()
//       for (v <- currentGraph.vertices) {
//         val neighbourColours =
//           currentGraph.neighbours(v).toList.map(currentGraph.colour)
//           val newColour = update(neighbourColours)
//           newColours(v) = newColour
//         }
//         currentGraph =
//           ColGraph[V, C](
//             currentGraph.vertices, currentGraph.neighbours, newColours(_))
//           }
//           currentGraph
//         }
// }

case class ColGraph[V,C](
vertices : Set[V], neighbours : V => Set[V], colour : V => C)

trait Simulation[V,C] {
def apply(graph: ColGraph[V,C], rounds : Int) : ColGraph[V,C]
}

class ConcurrentSimulation[V,C](update : List[C] => C, p: Int) extends Simulation[V,C] {
  def apply(graph: ColGraph[V,C], rounds : Int) : ColGraph[V,C] = {
    require(rounds >= 0)
    var currentGraph = graph
    val vertList = graph.vertices.toList        //create list from vertices to distribute among workers

    val barrier = Barrier(p)                    //simple barrier for syncing
    val colours = scala.collection.mutable.Map[V, C]()    //new colours for current round

    def worker(startV: Int, endV: Int): PROC = proc{    //start and end index of vertices assigned to this worker
      for (_ <- 1 to rounds) {
        val newColours = scala.collection.mutable.Map[V, C]()
        for (vi <- startV until endV) {     //sequentially collect colors for each vertex assigned in newColours
          val v = vertList(vi)
          val neighbourColours = currentGraph.neighbours(v).toList.map(currentGraph.colour)
          val newColour = update(neighbourColours)
          newColours(v) = newColour
        }
        barrier.sync()    //wait for all workers to finish reading the old graph colors
        for (vi <- startV until endV) {   //write worker's new colours to shared map - no write-write conflicts as vertex sets assigned are disjoint
          val v = vertList(vi)
          colours(v) = newColours(v)
        }
        barrier.sync()    //make sure all workers have finished updating colours map
        if (startV == 0) currentGraph = ColGraph[V, C]( currentGraph.vertices, currentGraph.neighbours, colours(_))     //first worker updates graph
        barrier.sync()    //wait for first worker to finish updating graph before reading on the next round
      }
    }

    val workChunk = vertList.length/(p-1)   //number of vertices assigned to each worker
    val workers = || (for (i <- 0 until p) yield worker(i*workChunk, (math.min(vertList.length, (i+1)*workChunk))))   //create workers

    workers()   //run workers

    currentGraph
  }
}

// serve(
//   for (i <- 0 until ins.length) yield
//     ins(i) =?=> {x => out!x}
// )

// repeat{
//   val x = in?()
//   alt(
//     for (i <- 0 until outs.length) yield
//       outs(i) =!=> {x}
//   )
// }
