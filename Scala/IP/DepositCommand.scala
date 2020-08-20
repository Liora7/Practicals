class DepositCommand(amount: Int) extends Command[Account] {

  override def execute(target: Account): Option[Change] = {
    if (amount >= 0){
      target.deposit(amount)
      return Some(new DepChange(amount, target))
    }
    else return None
  }

}
