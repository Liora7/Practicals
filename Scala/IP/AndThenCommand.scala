class AndThenCommand[T](first: Command[T], second: Command[T]) extends Command[T] {

  override def execute(target: T): Option[Change] = {
    val res1 = first.execute(target)
    var res2: Option[Change] = None
    res1 match {
      case Some(c1: Change) => res2 = second.execute(target)
      case _ => return None
    }

    res2 match {
      case Some(c2: Change) => {
        res1 match {
          case Some(c1: Change) => {return Some(new AndThenChange(c1, c2))}
          case _ => return None
        }
      }
      case None => {
        res1 match {
          case Some(c1: Change) => {c1.undo; return None}
          case _ => return None
        }
    }
  }
}
}
