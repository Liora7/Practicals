import io.threadcso._
import scala.util.Random

/* Q1:
It might be beneficial to use concurrency in the implementation of the web browser so that different tabs can send requests to the server in parallel, as waiting for responses may take some time. Furthermore, we might want tabs in the background to be updating at various intervals even when the user is not currently viewing them (e.g. email) and we want this to happen without the tab the user is currently on to freeze because it is happening sequentially. However, on a uni-processor computer, this makes less sense as concurrency will essentially be implemented by transferring control from process to process sequentially, and hence there may be performance or responsiveness issues with the current tab because some other tab in the background is getting to use the processor.
*/

/* Q2:
If P performs m atomic actions, and Q performs n, there are (n+m)!/(n!m!) possible interleavings; there are (n+m)! interleavings in total, but we need to divide this number by n!m! as there are constraints on the order of P's atomic actions, and Q's atomic actions.
*/

/* Q3:
If reading and writing a variable are atomic actions, then the assignment x = x + a consists of the sequence of atomic actions READ x, WRITE x+a.
So if we denote p_ACTION as the action process p is taking, and similarly q, we can have the sequences:
p_READ 0, p_WRITE 1, p_READ 1, p_WRITE 3, q_READ 3, q_WRITE 7
p_READ 0, p_WRITE 1, p_READ 1, q_READ 1, p_WRITE 3, q_WRITE 5
p_READ 0, p_WRITE 1, p_READ 1, q_READ 1, q_WRITE 5, p_WRITE 3
p_READ 0, p_WRITE 1, q_READ 1, q_WRITE 5, p_READ 5, p_WRITE 7
p_READ 0, q_READ 0, p_WRITE 1, q_WRITE 4, p_READ 4, p_WRITE 6
p_READ 0, q_READ 0, q_WRITE 4, p_WRITE 1, p_READ 1, p_WRITE 3
p_READ 0, q_READ 0, p_WRITE 1, p_READ 1, p_WRITE 3, q_WRITE 4
So we can end up with x equal to 3, 4, 5, 6, or 7.
*/

/* Q4:
Process system may not terminate, as as when y=x-1, process p and q may read at the same time, and then process p may write x=x+1, so x=y, but then process q may write y=y-1, so y=x+1, and neither process evaluated its while loop condition in the moment where y=x was true and hence system won't terminate.
*/

/* Q5:
We could have a balance of b, on which process A calls canDebit(b), which returns true. Then before process A can call debit(b), process B may call canDebit(b), which again returns true. Now either process A can call debit(b) or process B can call debit(b), after which we have a balance of 0, but the other process will still call debit(b) as it has already tested for canDebit. Then we have a balance of -b. To solve this, we could define a new method testAndDebit(b) which atomically calls first canDebit, and then straightaway calls debit, so that no other process can interfere in between the two function calls.
*/

object Q6{

  def component(left: ?[Int], right: ![Int]): PROC = proc{
    var max = left?()
    repeat{
      val newVal = left?()
      //print(" got " + newVal)
      if (newVal > max){
        right!max
        //print(" sent " + max)
        max = newVal
      }
      else{
        right!newVal
        //print(" sent " + newVal)

      }
    }
    right!max
    //print(" sent " + max)

    left.closeIn; right.closeOut
  }

  def system(n: Int, chans: List[Chan[Int]]): PROC = proc{
    val comps = || (for (i <- 0 until n) yield component(chans(i), chans(i+1)))
    run(comps)
  }

}


object Q6Test{
  // Number of elements; range of input values.
  val N = 100; val Max = 100
  def doTest = {
    val chans = List.fill(N+1)(OneOne[Int])
    val xs = List.fill(N)(Random.nextInt(Max)) // create list of random inputs
    var ys = List[Int]() // empty list for outputs
    val in = chans(0)
    val out = chans(N)
    def sender = proc{ for(x <- xs) in!x; in.close } //send elems from list
    def receiver = proc{ for(x <- xs) ys = out?() :: ys} // receive elems
    run(sender || receiver || Q6.system(N, chans)) // run concurrently
    assert(xs.sorted == ys.reverse) // check that output list equals sorted input list
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }
}

/* Q6:
There are n sequential messages at the beginning to get all the values to the pipeline. Then there are n+1 sequential messages to get all values out of the pipeline again. So there are 2n+1 sequential messages.
*/


class Q7(n: Int, nWorkers: Int, a: Array[Array[Int]], b: Array[Array[Int]]){

  private type Task = (Int, Int)
  private type Result = (Int, Int, Int)
  private val c = Array.fill(n, n)(0)
  private val toCollector = ManyOne[Result]
  private val getTask = OneMany[Task]

  private def worker: PROC = proc{
    repeat{
      val (i, j) = getTask?()
      val result = multiply(i, j)
      toCollector!(i, j, result)
    }
  }

  private def bag: PROC = proc{
    for (i <- 0 until n){
      for (j <- 0 until n){
        getTask!(i, j)
      }
    }
    getTask.close
  }

  private def collector: PROC = proc{
    for (t <- 0 until n*n){
      val (i, j, res) = toCollector?()
      c(i)(j) = res
    }
    toCollector.close
  }

  private def multiply(i: Int, j: Int): Int = {
    var res = 0
    for (k <- 0 until n){
      res += a(i)(k) * b(k)(j)
    }
    return res
  }

  private def system = {
    val workers = || (for (i <- 0 until nWorkers) yield worker)
    collector || bag || workers
  }

  def apply: Array[Array[Int]] = {
    system(); return c
  }

}

object Q7Test{
  // Number of elements; range of input values.
  val N = 20; val Max = 100
  def doTest = {
    val a = Array.fill(N, N)(Random.nextInt(Max))
    val b = Array.fill(N, N)(Random.nextInt(Max))
    val multiplier = new Q7(N, N, a, b)
    val c: Array[Array[Int]] = multiplier.apply
    matrixMultCheck(c, a, b)
    }

  def matrixMultCheck(c: Array[Array[Int]], a: Array[Array[Int]], b: Array[Array[Int]]){
    for (i <- 0 until N){
      for (j <- 0 until N){
        var z = 0
        for (k <- 0 until N){
          z += a(i)(k) * b(k)(j)
        }
        assert(z == c(i)(j))
      }
    }
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }
}


/* Q8:
We could have a process for each int in a, which goes through b looking for an occurence of that number. Alternatively, we could have a counter process which keeps track of the count so far, and a process for some subsection of a, which scans b for those numbers of a.
*/
