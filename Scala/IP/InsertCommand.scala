class InsertCommand(x: Int) extends Command[PriorityQueue] = {

  override def execute(target: PriorityQueue): Option[Change] = {
    target.insert(x)
    return Some(new InsChange(x, target))
  }

}
