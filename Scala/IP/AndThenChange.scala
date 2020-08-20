class AndThenChange(c1: Change, c2: Change) extends Change {
  override def undo() = {
    c1.undo
    c2.undo
  }
}
