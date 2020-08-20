trait Account {
  // DTI: cur >= 0
  // Abs: cur := current balance of account

  //pre: x > 0 && cur >= 0
  //post: cur += x
  def deposit(x: Int)

  //pre: x > 0 && cur >= x
  //post: cur -= x
  def withdraw(x: Int)

  // post: cur = cur0 && returns cur
  def balance(): Int

}
