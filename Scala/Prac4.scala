// Template for the Sleeping Tutor practical

import io.threadcso._
import io.threadcso.debug.Log


/** The trait for a Sleeping Tutor protocol. */
trait SleepingTutor{
  /** A tutor waits for students to arrive. */
  def tutorWait

  /** A student arrives and waits for the tutorial. */
  def arrive

  /** A student receives a tutorial. */
  def receiveTute

  /** A tutor ends the tutorial. */
  def endTeach
}

// =======================================================

class SleepingTutorMonitor extends SleepingTutor{
  private var studentWaiting, studentsAvailable = false // one student is waiting
  private var tuteDone = true
  private val monitor = new Monitor
  private val studentsAvailableC, studentWaitingC, tutorDoneC, studentsLeftC, studentSyncC, tutorWaitC =
    monitor.newCondition

  /** A tutor waits for students to arrive. */
  def tutorWait = monitor.withLock{
    tutorWaitC.await(tuteDone) // get lock
    tuteDone = false
    studentsAvailableC.await(studentsAvailable) // wait for both students to be ready

  }

  /** A student arrives and waits for the tutorial. */
  def arrive = monitor.withLock{
    if (!studentWaiting){ // if tute partner isn't waiting yet
      studentWaiting = true // say this student is waiting
      studentWaitingC.await(!studentWaiting) // wait for tute partner
      studentSyncC.signal() // sync with tute partner
    }
    else{ // both students have arrived
      studentWaiting = false // stop waiting for tute partner
      studentWaitingC.signal() // let partner know both students are ready
      studentSyncC.await() // sync with tute partner
      studentsAvailable = true
      studentsAvailableC.signal() // signal to tutor
    }
  }

  /** A student receives a tutorial. */
  def receiveTute = monitor.withLock{
    tutorDoneC.await(tuteDone)  // wait for tutor to finish tutorial
    if (!studentWaiting) { // first student to wake up
      studentWaiting = true // wait for tute partner to wake up
      tutorDoneC.signal() // let students know tutorial is over
      studentWaitingC.await(!studentWaiting) // wait for signal from tute partner
      studentsAvailable = false
      studentSyncC.signal() // sync  with tute partner
    } // if first to wake up, wait for other student to wake up
    else {
      studentWaiting = false
      studentWaitingC.signal() // let tute partner know they're awake
      studentSyncC.await() // sync with tute partner
      studentsLeftC.signal() // let tutor know they're leaving
    }
  }

  /** A tutor ends the tutorial. */
  def endTeach = monitor.withLock{
    tuteDone = true // finish tute
    tutorDoneC.signal() // let students know tutorial is over
    studentsLeftC.await(!studentsAvailable) // wait for students to leave
    tutorWaitC.signal() // give up lock
  }

}

// =======================================================

class SleepingTutorSemaphore extends SleepingTutor{
  private val tutorStarts, tutorEnds, studentsLeave, studentsArrive, studentsReceive = CountingSemaphore(0)
  private val twait = MutexSemaphore()

  /** A tutor waits for students to arrive. */
  def tutorWait = {
    twait.down // tutor obtains mutex in order to pass the baton
    studentsArrive.down // tutor waits for both students to arrive
    studentsArrive.down
    tutorStarts.up // tutor lets both students know the tute can start
    tutorStarts.up
    studentsReceive.down // tutor waits for both students to be ready for the tutorial
    studentsReceive.down
  }

  /** A student arrives and waits for the tutorial. */
  def arrive = {
    studentsArrive.up // student lets tutor know they've arrived
    tutorStarts.down // student waits for tutor to notify that other student has arrived
  }

  /** A student receives a tutorial. */
  def receiveTute = {
    studentsReceive.up // student tells tutor they're ready
    tutorEnds.down // students waits for tutor to finish
    studentsLeave.up // student notifies tutor they're leaving
  }

  /** A tutor ends the tutorial. */
  def endTeach = {
    tutorEnds.up // tutor lets both students know tute is over
    tutorEnds.up
    studentsLeave.down // tutor waits for both students to leave
    studentsLeave.down
    twait.up // tutor gives up mutex to prepare for next round

  }
}



// =======================================================


import scala.util.Random

object SleepingTutorSimulation{
  private val st: SleepingTutor = new SleepingTutorMonitor

  def student(me: String) = proc("Student"+me){
    while(true){
      Thread.sleep(Random.nextInt(2000))
      println("Student "+me+" arrives")
      st.arrive
      println("Student "+me+" ready for tutorial")
      st.receiveTute
      println("Student "+me+" leaves")
    }
  }

  def tutor = proc("Tutor"){
    while(true){
      println("Tutor waiting for students")
      st.tutorWait
      println("Tutor starts to teach")
      Thread.sleep(1000)
      println("Tutor ends tutorial")
      st.endTeach
      Thread.sleep(1000)
    }
  }

  def system = tutor || student("Alice") || student("Bob")

  def main(args: Array[String]) = system()
}



object LogTest{
  var iters = 10 // # of tutes on each rep
  val reps = 100 // # times to repeat

  // Events put into the log
  abstract class LogEvent
  case class StudentArrives(me: Int) extends LogEvent
  case class StudentReady(me: Int) extends LogEvent
  case class StudentLeaves(me: Int) extends LogEvent
  case class TutorWaiting() extends LogEvent
  case class TutorTeaching() extends LogEvent
  case class TutorDone() extends LogEvent
  case class TutorReset() extends LogEvent


  /** A student */
  def student(me: Int, tute: SleepingTutor, log: Log[LogEvent]) = proc{
    Thread.sleep(Random.nextInt(1000))
    log.add(me, StudentArrives(me))
    tute.arrive
    log.add(me, StudentReady(me))
    tute.receiveTute
    log.add(me, StudentLeaves(me))
  }

  /** The tutor */
  def tutor(tute: SleepingTutor, log: Log[LogEvent]) = proc{
    log.add(0, TutorWaiting())
    tute.tutorWait
    log.add(0, TutorTeaching())
    //Thread.sleep(1000)
    log.add(0, TutorDone())
    tute.endTeach
    log.add(0, TutorReset()) // end of tutorial
    //Thread.sleep(1000)
  }

  /** Check that events represents a valid log: if a GotResource event happens,
    * no thread is currently holding the resource.
    * @return true if the log is valid.  */
  def checkLog(events: Array[LogEvent]): Boolean = {
    var arriveStudent, readyStudent, leaveStudent = Array.fill(2)(false)
    var tutorTeaching, tutorDone = false
    var tutorWaiting = true
    var error = false; var i = 0
    while(i < events.size && !error){
      events(i) match{
        case StudentArrives(id) =>
          arriveStudent(id) = true
        case StudentReady(id) =>
          if (!arriveStudent(id)){
            println("Error found:")
            println(events.take(i+1).mkString("\n"))
            error = true
          }
          else readyStudent(id) = true
        case StudentLeaves(id) =>
          if (!tutorDone){
            println("Error found:")
            println(events.take(i+1).mkString("\n"))
            error = true
          }
          else leaveStudent(id) = true
        case TutorWaiting() =>
          tutorWaiting = true; tutorDone = false
        case TutorTeaching() =>
          if (!(readyStudent(0) && readyStudent(1))){
            println("Error found:")
            println(events.take(i+1).mkString("\n"))
            error = true
          }
          else tutorTeaching = true
        case TutorDone() =>
          tutorDone = true
        case TutorReset() =>
          if (!(leaveStudent(0) && leaveStudent(1))){
            println("Error found:")
            println(events.take(i+1).mkString("\n"))
            error = true
          }
          arriveStudent = Array.fill(2)(false); readyStudent = Array.fill(2)(false); leaveStudent = Array.fill(2)(false)
          tutorTeaching = false; tutorDone = false
        }
        i += 1
      }
      !error
    }

  /** Run a single test. */
  def runTest(tute: SleepingTutor) = {
    //println
    val log = new Log[LogEvent](3)
    val students = || (for (i <- 0 until 2) yield student(i, tute, log))
    try{ (students || tutor(tute, log))() } finally{ log.toFile("logFile") }
    //tute.shutdown
    if(!checkLog(log.get)) sys.exit
  }

  def main(args: Array[String]) = {
    // Parse command line arguments
    var tuteType = 1 // Which resource server to use
    var i = 0
    while(i < args.length) args(i) match{
      case "-1" => tuteType = 1; i += 1
      case "-2" => tuteType = 2; i += 1
      case arg => println("Unrecognised argument: "+arg); sys.exit
    }

    for(r <- 0 until reps){
      val tute: SleepingTutor = new SleepingTutorMonitor()
        //if(tuteType == 1) new SleepingTutorMonitor()
        //else if(tuteType == 2) new SleepingTutorSemaphore()
      runTest(tute)
      if(r%10 == 0) print(".")
    }
    println; io.threadcso.exit()
  }
}

/* Sample Log
TutorWaiting()
StudentArrives(0)
StudentArrives(1)
StudentReady(0)
StudentReady(1)
TutorTeaching()
TutorDone()
StudentLeaves(0)
StudentLeaves(1)
TutorReset()


TutorWaiting()
StudentArrives(1)
StudentReady(1)
StudentArrives(0)
StudentReady(0)
TutorTeaching()
TutorDone()
StudentLeaves(1)
StudentLeaves(0)
TutorReset()
*/
