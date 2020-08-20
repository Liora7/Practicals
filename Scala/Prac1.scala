import io.threadcso._
object Sort{

  def main(args : Array[String]) = {
  }

  /* A single comparator, inputting on in0 and in1, and outputting on out0
    (smaller value) and out1 (larger value). */
  def comparator(in0: ?[Int], in1: ?[Int], out0: ![Int], out1: ![Int ]): PROC = proc{
    repeat{
      var a = 0; var b = 0; // init a and b
      def rec0 = proc{a = in0?()}
      def rec1 = proc{b = in1?()}
      run( rec0 || rec1 ) // receive the inputs in either order
      if (a>b){ // test which value should be sent on which channel
        def snd0 = proc{out0!b}
        def snd1 = proc{out1!a}
        run( snd0 || snd1 ) // send outputs in either order
      }
      else{
        def snd0 = proc{out0!a}
        def snd1 = proc{out1!b}
        run( snd0 || snd1 )
      }
    }
    in0.closeIn; in1.closeIn; out0.closeOut; out1.closeOut //close channels
  }

  /* A sorting network for four values. */
  def sort4(ins: List[?[Int ]], outs: List [![ Int ]]): PROC = {
      require(ins.length == 4 && outs.length == 4)
      val c0, c21, c22, c11, c12, c3 = OneOne[Int] //create intermediate channels
      val comp0 = comparator(ins(0), ins(2), c0, c21) //create 5 comparators
      val comp1 = comparator(ins(1), ins(3), c11, c3)
      val comp2 = comparator(c0, c11, outs(0), c12)
      val comp3 = comparator(c21, c3, c22, outs(3))
      val comp4 = comparator(c12, c22, outs(1), outs(2))

      def system = comp0 || comp1 || comp2 || comp3 || comp4 // run in parallel
      system
  }

  /* Insert a value input on in into a sorted sequence input on ins.
    Pre: ins.length = n && outs.length = n+1, for some n >= 1.
    If the values xs input on ins are sorted, and x is input on in, then a
    sorted permutation of x::xs is output on ys. */
    def insert(ins: List[?[Int ]], in: ?[Int], outs: List [![ Int ]]): PROC = {
      val n = ins.length; require(n >= 1 && outs.length == n+1)
      val c = OneOne[Int] // create intermediate channel
      var system = proc{} // create new system
      if (n>1){ // recursive case
        val comp = comparator(in, ins(0), outs(0), c) // use comparator on in and least value of ins
        system = system || comp || insert(ins.tail, c, outs.tail) // run concurrently with recursive call
      }
      else{ // base case
        val comp = comparator(in, ins(0), outs(0), outs(1)) // do last comparator
        system = system || comp
      }
      system
    }

    /* Insert a value input on in into a sorted sequence input on ins.
      Pre: ins.length = n && outs.length = n+1, for some n >= 1.
      If the values xs input on ins are sorted, and x is input on in, then a
      sorted permutation of x::xs is output on ys. */
      def fastInsert(ins: List[?[Int ]], in: ?[Int], outs: List [![ Int ]]): PROC = {
        val n = ins.length; require(n >= 1 && outs.length == n+1)
        val c,d = OneOne[Int] // create intermediate channel
        var system = proc{} // create new system
        if (n>1){ // recursive case
          val ins1 = ins.take(n/2); val ins2 = ins.drop(n/2)
          val outs1 = outs.take(n/2 + 1); val outs2 = outs.drop(n/2 + 1)
          val comp = comparator(in, ins2(0), c, d) // use comparator on in and least value of ins
          system = system || comp || fastInsert(ins1, c, outs1)
          if (n>2){
           system = system || fastInsert(ins2.tail, d, outs2) // run concurrently with two recursive calls
          }
          else{ // ins2 is empty; just pass on value received from d to outs
            system = system || proc{ val x = d?(); outs2(0)!x}
          }
        }
        else{ // base case
          val comp = comparator(in, ins(0), outs(0), outs(1)) // do last comparator
          system = system || comp
        }
        system // return system
      }

    /*  Insertion sort. */
    def insertionSort(ins: List[?[Int ]], outs: List [![ Int ]]): PROC = {
        val n = ins.length; require(n >= 2 && outs.length == n)
        var system = proc{} // create new system
        val med = List.fill(n-1)(OneOne[Int]) // intermediate channles
        if (n > 2){ // recursive case
          val sorter = insertionSort(ins.tail, med) // sort last n-1 items of ins and output on med
          val inserter = insert(med, ins.head, outs) // take sorted n-1 items from med and insert first item of ins, output on outs
          system = system || sorter || inserter // run concurrently
        }
        else{ // base case
          val inserter = insert(ins.tail, ins.head, outs) // use last comparator
          system = system || inserter
        }
        system // return system
    }

}

import scala.util.Random
object SortTest{
  /** Run a single test.  Generate N random numbers.  Pass them in to a sorter.
    * Receive outputs.  Check result is as expected. */
  def test1 = {
    var xs = List.fill(4)(scala.util.Random.nextInt(100)) //create random list
    val i0, i1, i2, i3 = OneOne[Int] // inpout channels
    var ins = List(i0, i1, i2, i3)
    val o0, o1, o2, o3 = OneOne[Int] // output channels
    var outs = List(o0, o1, o2, o3)
    val sorter = Sort.sort4(ins, outs) // create sorter system
    var sendprocs = proc{}
    for(i <- 0 until 4){ // send values concurrently, then close OutPort of in channels
      def p = proc{ ins(i)!xs(i); ins(i).closeOut }
      sendprocs = sendprocs || p
    }
    var ys = List.fill(4)(0) // init result list
    def recprocs = proc{ // receive outputs sequentially to avoid caching conflicts, then close InPort of out channels
      for (i <- 0 until 4){
      ys = ys.updated(i, outs(i)?())
      outs(i).closeIn
      }
    }
    run(sendprocs || sorter || recprocs)
    assert(xs.sorted.sameElements(ys)) // check the result is sorted correctly
  }
  def test2 = {
    val n = 1 + scala.util.Random.nextInt(100) // random length of list
    var xs = List.fill(n)(scala.util.Random.nextInt(100)).sorted //create random list and sort
    var x = scala.util.Random.nextInt(100) // item to be inserted
    val ins = List.fill(n)(OneOne[Int]) // list of ins channels
    val outs = List.fill(n+1)(OneOne[Int]) // list of outs channels
    val in = OneOne[Int] // in channel
    val sorter = Sort.fastInsert(ins, in, outs) // create sorter system
    var sendprocs = proc{ in!x; in.closeOut } // send in value
    for(i <- 0 until n){ // send values of the sorted list concurrently, then close OutPort of ins channels
      def p = proc{ ins(i)!xs(i); ins(i).closeOut }
      sendprocs = sendprocs || p
    }
    var ys = List.fill(n+1)(0) // init result list
    def recprocs = proc{ // receive outputs sequentially to avoid caching conflicts, then close InPort of out channels
      for (i <- 0 until n+1){
      ys = ys.updated(i, outs(i)?())
      outs(i).closeIn
      }
    }
    run(sendprocs || sorter || recprocs)
    assert((x :: xs).sorted.sameElements(ys)) // check the result is sorted correctly
  }

  def test3 = {
    val n = 2 + scala.util.Random.nextInt(50) // random length of list
    var xs = List.fill(n)(scala.util.Random.nextInt(100)) //create random list
    val ins = List.fill(n)(OneOne[Int]) // list of ins channels
    val outs = List.fill(n)(OneOne[Int]) // list of outs channels
    val sorter = Sort.insertionSort(ins, outs) // create sorter system
    var sendprocs = proc{}
    for(i <- 0 until n){ // send values of the sorted list concurrently, then close OutPort of ins channels
      def p = proc{ ins(i)!xs(i); ins(i).closeOut }
      sendprocs = sendprocs || p
    }
    var ys = List.fill(n)(0) // init result list
    def recprocs = proc{ // receive outputs sequentially to avoid caching conflicts, then close InPort of out channels
      for (i <- 0 until n){
      ys = ys.updated(i, outs(i)?())
      outs(i).closeIn
      }
    }
    run(sendprocs || sorter || recprocs)
    assert(xs.sorted.sameElements(ys)) // check the result is sorted correctly
  }
  def main(args : Array[String]) = {
    for(i <- 0 until 100){ test3; if(i%10 == 0) print(".") }
    println; exit
  }
}
