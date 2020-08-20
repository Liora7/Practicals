class WithdrawCommand(amount: Int) extends Command[Account] {

  override def execute(target: Account): Option[Change] = {
    if (amount >= 0 && target.balance >= amount){
      target.withdraw(amount)
      return Some(new WitChange(amount, target))
    }
    else return None
  }

}
