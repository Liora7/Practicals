class BasicAccount(init: Int) extends Account {

  private var cur: Int = init

  def deposit(x: Int) = {
    if (x >= 0) cur += x
  }

  def withdraw(x: Int) = {
    if (x >= 0 && cur >= x) cur -= x
  }

  def balance(): Int = cur

}
