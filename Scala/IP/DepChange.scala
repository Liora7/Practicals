class DepChange(amount: Int, target: Account) extends Change {
  override def undo() = if (target.balance >= amount) target.withdraw(amount)
}
