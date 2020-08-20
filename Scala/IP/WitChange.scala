class WitChange(amount: Int, target: Account) extends Change {
  override def undo() = target.deposit(amount)
}
