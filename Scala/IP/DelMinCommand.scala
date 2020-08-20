class DelMinCommand() extends Command[PriorityQueue] = {

  override def execute(target: PriorityQueue): Option[Change] = {
    if (!target.isEmpty){
      val min = target.delMin
      return Some(new InsChange(min, target))
    }
    else return None
  }

}
