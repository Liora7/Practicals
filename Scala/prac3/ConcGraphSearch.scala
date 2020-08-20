import io.threadcso._
import scala.collection.mutable.Stack


class ConcGraphSearch[N](g: Graph[N]) extends GraphSearch[N](g){
  /**The number of workers. */
  val numWorkers = 8

  /** Perform a depth-first search in g, starting from start, for a node that
    * satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[N] = {
    // Concurrent stack storing nodes
    val stack = new TerminatingPartialStack[N](numWorkers) // create stack of nodes to explore
    stack.push(start) // push starting node onto stack

    // result variable
    var res = None.asInstanceOf[Option[N]]

    def worker = proc{
      repeat{ // do until solution is found or complete tree has been explored
        val n = stack.pop // get next node from the stack
        for(n1 <- g.succs(n)){ // explore successors
          if(isTarget(n1)){ // if solution found
            synchronized{ res = Some(n1); stack.shutdown}
            // put result into res variable and shut down the stack
          }
          else stack.push(n1) // push node onto the stack
        }
      }
    }


    // create numWorkers workers
    val workers = || (for (i <- 0 until numWorkers) yield worker)
    workers()
    return res
  }
}
