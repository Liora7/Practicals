import io.threadcso._
import scala.language.postfixOps
import scala.util.Random

object Task1{
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def Eat = Thread.sleep(500)
  def Think = Thread.sleep(Random.nextInt(900))
  def Pause = Thread.sleep(500)

  // Each philosopher will send "pick" and "drop" commands to her forks, which
  // we simulate using the following values.
  type Command = Boolean
  val Pick = true; val Drop = false

  val log = new io.threadcso.debug.Log[String](N)

  /** A single left-handed philosopher. */
  def lPhil(me: Int, left: ![Command], right: ![Command]) = proc("Phil"+me){
    repeat{
      Think
      log.add(me, me+" sits"); Pause
      left!Pick; log.add(me, me+" picks up left fork"); Pause //pick up left fork first
      right!Pick; log.add(me, me+" picks up right fork"); Pause
      log.add(me, me+" eats"); Eat
      left!Drop; Pause; right!Drop; Pause // drop left fork first
      log.add(me, me+" leaves")
      if(me == 0) print(".")
    }
  }

  /** A single right-handed philosopher. */
  def rPhil(me: Int, left: ![Command], right: ![Command]) = proc("Phil"+me){
    repeat{
      Think
      log.add(me, me+" sits"); Pause
      right!Pick; log.add(me, me+" picks up right fork"); Pause //pick up right fork
      left!Pick; log.add(me, me+" picks up left fork"); Pause
      log.add(me, me+" eats"); Eat
      right!Drop; Pause; left!Drop; Pause //drop right fork first
      log.add(me, me+" leaves")
      if(me == 0) print(".")
    }
  }

  /** A single fork. */
  def fork(me: Int, left: ?[Command], right: ?[Command]) = proc("Fork"+me){
    serve(
      left =?=> {
        x => assert(x == Pick); val y = left?; assert(y == Drop)
      }
      |
      right =?=> {
        x => assert(x == Pick); val y = right?; assert(y == Drop)
      }
    )
  }

  /** The complete system. */
  val system = {
    // Channels to pick up and drop the forks:
    val philToLeftFork, philToRightFork = Array.fill(N)(OneOne[Command])
    // philToLeftFork(i) is from Phil(i) to Fork(i);
    // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)
    var allPhils = || (
      for (i <- 0 until N-1)
      yield lPhil(i, philToLeftFork(i), philToRightFork(i))
    )
    allPhils = allPhils || rPhil(N-1, philToLeftFork(N-1), philToRightFork(N-1))
    val allForks = || (
      for (i <- 0 until N) yield
        fork(i, philToRightFork((i+1)%N), philToLeftFork(i))
    )
    allPhils || allForks
  }

  /** Run the system. */
  def main(args : Array[String]) = {
    log.writeToFileOnShutdown("philsLog.txt")
    run(system)
    exit
  }

  /* Test log:
0 sits
1 sits
3 sits
0 picks up left fork
2 sits
4 sits
1 picks up left fork
3 picks up left fork
0 picks up right fork
2 picks up left fork
0 eats
1 picks up right fork
1 eats
0 leaves
2 picks up right fork
0 sits
2 eats
0 picks up left fork
1 leaves
3 picks up right fork
0 picks up right fork
3 eats
0 eats
1 sits
2 leaves
4 picks up right fork       // no deadlock because 4 picks up right fork first
1 picks up left fork
4 picks up left fork
2 sits
1 picks up right fork
3 leaves
0 leaves
4 eats
  */

}


object Task2{
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def Eat = Thread.sleep(500)
  def Think = Thread.sleep(Random.nextInt(900))
  def Pause = Thread.sleep(500)

  // Each philosopher will send "pick" and "drop" commands to her forks, which
  // we simulate using the following values.
  type Command = Boolean
  val Pick = true; val Drop = false

  val log = new io.threadcso.debug.Log[String](N)

  val sit, leave = ManyOne[Unit] // channel used for synchronization between phils and butler - butler only accepts sit message if there are less than 4 phils at the table, otherwise waits for one to leave and then lets the next phil sit

  /** A single philosopher. */
  def phil(me: Int, left: ![Command], right: ![Command]) = proc("Phil"+me){
    repeat{
      Think
      sit!() // ask butler to be seated
      log.add(me, me+" sits"); Pause
      left!Pick; log.add(me, me+" picks up left fork"); Pause //pick up left fork first
      right!Pick; log.add(me, me+" picks up right fork"); Pause
      log.add(me, me+" eats"); Eat
      left!Drop; Pause; right!Drop; Pause // drop left fork first
      leave!() // tells butler a space at the table is freed up
      log.add(me, me+" leaves")
      if(me == 0) print(".")
    }
  }

  /** A butler */
  def butler() = proc("Butler"){
    var seated = 0 //keep track of how many are at the table
    serve( // repeatedly receive messages
        sit =?=> { _ -> (
          if (seated == 3){ leave?() } // table is full - wait for one phil to leave, let the one that sent the request sit; number of phils at the table doesn't change
          else{ seated += 1 } ) } // less than 4 phils at the table, let the one who made the request sit and update counter
      | leave =?=> { _ -> (seated -= 1)} // a phil is done and there currently is none waiting to sit down, so update counter and continue receiving messages
    )
  }

  /** A single fork. */
  def fork(me: Int, left: ?[Command], right: ?[Command]) = proc("Fork"+me){
    serve(
      left =?=> {
        x => assert(x == Pick); val y = left?; assert(y == Drop)
      }
      |
      right =?=> {
        x => assert(x == Pick); val y = right?; assert(y == Drop)
      }
    )
  }

  /** The complete system. */
  val system = {
    // Channels to pick up and drop the forks:
    val philToLeftFork, philToRightFork = Array.fill(N)(OneOne[Command])
    // philToLeftFork(i) is from Phil(i) to Fork(i);
    // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)
    val allPhils = || (
      for (i <- 0 until N)
      yield phil(i, philToLeftFork(i), philToRightFork(i))
    )
    val allForks = || (
      for (i <- 0 until N) yield
        fork(i, philToRightFork((i+1)%N), philToLeftFork(i))
    )
    val btl = butler() //create butler
    allPhils || allForks || btl
  }

  /** Run the system. */
  def main(args : Array[String]) = {
    log.writeToFileOnShutdown("philsLog.txt")
    run(system)
    exit
  }

  /* Test Log
  1 sits
  0 sits
  2 sits
  3 sits        // no more than 4 phils may be seated at a time - so now they can all eat
  1 picks up left fork
  0 picks up left fork
  2 picks up left fork
  3 picks up left fork
  0 picks up right fork
  0 eats
  1 picks up right fork
  1 eats
  0 leaves
  4 sits
  2 picks up right fork
  2 eats
  4 picks up left fork
  1 leaves
  0 sits
  3 picks up right fork
  3 eats
  0 picks up left fork
  2 leaves
  4 picks up right fork
  1 sits
  4 eats
  1 picks up left fork
  3 leaves
  0 picks up right fork
  2 sits
  2 picks up left fork
  0 eats
  4 leaves
  1 picks up right fork
  3 sits
  1 eats
  3 picks up left fork
  2 picks up right fork
  0 leaves

  */
}


object Task3{
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def Eat = Thread.sleep(500)
  def Think = Thread.sleep(Random.nextInt(900))
  def Pause = Thread.sleep(500)
  def ForkWait = 1000000 + Random.nextInt(1000000) // random time in ms to wait for right fork before giving up to make deadlocks very unlikely

  // Each philosopher will send "pick" and "drop" commands to her forks, which
  // we simulate using the following values.
  type Command = Boolean
  val Pick = true; val Drop = false

  val log = new io.threadcso.debug.Log[String](N)

  /** A single left-handed philosopher. */
  def phil(me: Int, pickl: channel.DeadlineManyOne[Command], pickr: channel.DeadlineManyOne[Command], dropl: channel.DeadlineManyOne[Command], dropr: channel.DeadlineManyOne[Command]) = proc("Phil"+me){
    repeat{
      Think
      log.add(me, me+" sits"); Pause
      pickl!Pick // pick up left fork
      log.add(me, me+" picks up left fork"); Pause
      var right = pickr.writeBefore(ForkWait)(Pick) // try picking up right fork for ForkWait ns, else give up
      if (right){ // if pick right succeeded go ahead as before
        log.add(me, me+" picks up right fork"); Pause
        log.add(me, me+" eats"); Eat
        dropl!Drop // drop left fork
        Pause
        dropr!Drop // drop right fork
        Pause
        log.add(me, me+" leaves")
        if(me == 0) print(".")
      }
      else{ // right fork could not be picked up
        dropl!Drop // drop left fork
        log.add(me, me+" drops left fork") // and try again from the start
      }
    }
  }

  /** A single fork. */
  def fork(me: Int, pick: channel.DeadlineManyOne[Command], drop: channel.DeadlineManyOne[Command]) = proc("Fork"+me){
    repeat{
      val p = pick?() // fork is available and waits to be picked up
      assert(p == Pick); val d = drop?; assert(d == Drop) // fork waits to be dropped, then becomes available again
      // the fork needs separate pick and drop channels to distinguish between a pick and a drop and two picks
    }
  }

  /** The complete system. */
  val system = {
    // Channels to pick up and drop the forks:
    val picks, drops = Array.fill(N)(new channel.DeadlineManyOne[Command])
    // picks(i) is the channel for pick commands to fork i, and similarly  drops(i) is for drops to fork i
    val allPhils = || (
      for (i <- 0 until N)
      yield phil(i, picks(i), picks((i+1)%N), drops(i), drops((i+1)%N)) // each phil has a channel to pick up each fork and a channel to drop each fork
    )
    val allForks = || (
      for (i <- 0 until N) yield
        fork(i, picks(i), drops(i))
    )
    allPhils || allForks
  }

  /** Run the system. */
  def main(args : Array[String]) = {
    log.writeToFileOnShutdown("philsLog.txt")
    run(system)
    exit
  }

  /* Test log
0 sits
2 sits
0 picks up left fork
4 sits
1 sits
3 sits
2 picks up left fork
0 picks up right fork
4 picks up left fork
3 picks up left fork
2 drops left fork       // avoids deadlock - phil 2 couldn't get both forks so drops the left one and tries again later
0 eats
4 drops left fork
3 picks up right fork
4 sits
2 sits
3 eats
1 picks up left fork
2 picks up left fork
0 leaves
1 drops left fork
2 picks up right fork
4 picks up left fork
0 sits
2 eats
3 leaves
4 picks up right fork
1 sits
4 eats
1 picks up left fork
3 sits
1 picks up right fork
3 picks up left fork
2 leaves

  */
}
