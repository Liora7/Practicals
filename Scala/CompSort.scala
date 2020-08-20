import io.threadcso._
object CompSort{

  def main(args : Array[String]) = {
  }

  def makePipeline(n: Int): (PROC, Chan[Int], Chan[Int]) = {
    val left = OneOne[Int] // inPort for pipeline
    var cur = left // keep track of current left channel
    var procs = proc{} // accumulate components
    for (i <- 0 until n){
      val chan = OneOne[Int] // create new right channel
      val proc = comp(cur, chan) // create comp with appropriate left and right
      cur = chan // set current right channel to left
      procs = procs || proc // add new comp to proc list
    }
    val right = cur // set outPort of pipeline
    return (procs, left, right)
  }

  def sort(in: ?[Int], out: ![Int], n: Int) = proc{
    val (procs, left, right) = makePipeline(n) // create pipeline of n components
    def send = proc{ // send stream of unsorted ints to start of pipeline
      repeat{ val x = in?(); left!x }
      left.closeIn
    }
    def rec = proc{ // receive stream of sorted ints from end of pipeline
      repeat{ out!(right?()) }
      right.closeOut
    }
    def system = procs || send || rec
    run(system)
    out.closeOut
  }

  def comp(left: ?[Int], right: ![Int]): PROC = proc("COMP"){
      var max = left?() // set largest int seen to first int received
      repeat { var x = left?() // keep asking for input, storing the larger of x and max in max and sending the other through to the next component
               if (x>max){
                 var temp = max
                 max = x
                 right!temp
               }
               else right!x }
      right!max
      right.closeOut
  }
}

import scala.util.Random
object CompSortTest{
  // Number of elements to sort; range of input values.
  val N = 100; val Max = 100
  /** Run a single test.  Generate N random numbers.  Pass them in to a sorter.
    * Receive outputs.  Check result is as expected. */
  def doTest = {
    val xs = Array.fill(N)(Random.nextInt(Max))
    val ys = new Array[Int](N)
    val in, out = OneOne[Int]
    def sender = proc{ for(x <- xs) in!x; in.close }
    def receiver = proc{ var i = 0; repeat{ ys(i) = out?(); i += 1 } }
    run(sender || CompSort.sort(in, out, N) || receiver)
    assert(xs.sorted.sameElements(ys))
  }
  def main(args : Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println; exit
  }
}
