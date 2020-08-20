import io.threadcso._
import scala.collection.mutable.{Stack, Queue}

/** A partial stack that terminates if all worker threads are attempting to
  * pop, and the stack is empty.
  * @param numWorkers the number of worker threads. */
class TerminatingPartialStack[A](numWorkers: Int){
  /** Channel for pushing. */
  private val pushChan = ManyOne[A]

  private type ReplyChan = Chan[A]

  /** Channel for popping. */
  private val popChan = ManyOne[ReplyChan]

  /** Channel for shutting down the stack. */
  private val shutdownChan = ManyOne[Unit]

  /** Push x. */
  def push(x: A): Unit = pushChan!x

  /** Attempt to pop a value.*/
  def pop: A = {
    val reply = OneOne[A]
    popChan!reply
    reply?()
  }

  /** Shut down this stack. */
  def shutdown = attempt{ shutdownChan!(()) }{}
  // Note: it's possible that the server has already terminated, in which case
  // we catch the StopException.

  /** The server process. */
  private def server = proc("server"){
    // Currently held values
    val stack = new Stack[A]()
    // Queue holding reply channels for current pop attempt.
    val waiters = new Queue[ReplyChan]()
    // Inv: stack.isEmpty or waiters.isEmpty
    // Termination: signal to all waiting workers
    def close = {
      for(c <- waiters) c.close
      pushChan.close; popChan.close; shutdownChan.close
    }

    serve(
      pushChan =?=> { x =>
        if(waiters.nonEmpty){ // pass x directly to a waiting pop
          assert(stack.isEmpty); waiters.dequeue!x
        }
        else stack.push(x)
      }
      |
      popChan =?=> { reply =>
        if(stack.nonEmpty) reply!(stack.pop) // service request immediately
        else{
          waiters.enqueue(reply)
          if(waiters.length == numWorkers) close
        }
      }
      |
      shutdownChan =?=> { _ => close }
    )
  }

  server.fork
}
