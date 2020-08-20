class WhileCommand[T](test:T => Boolean, cmd:Command[T]) extends Command[T] {

  override def execute(target: T): Option[Change] = {
    var com: Change = null
    if (test(target)) {
      com = cmd.execute(target).get
    }
    else return None
    while (test(target)){
      val res = cmd.execute(target)
      res match {
        case Some(chng: Change) => com = new AndThenChange(com, chng)
        case None => com.undo; return None
      }
    }
    Some(com)
  }

}
