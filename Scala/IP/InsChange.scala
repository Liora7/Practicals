class InsChange(x: Int, target: PriorityQueue) extends Change {
  override def undo() = target.remove(x)
}
