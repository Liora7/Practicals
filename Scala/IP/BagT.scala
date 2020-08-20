class Bag[T](private val f: T => Int) {
  // fB : T → N

  def add(x:T): Bag[T] = {
    new Bag((y => if (y==x) f(x)+1 else f(y)))
  }

  def remove(x:T): Bag[T] = {
    new Bag((y => if (y==x) f(x)-1 else f(y)))
  }

  def count(x:T): Int = {
    f(x)
  }

  def union(that: Bag[T]): Bag[T] = {
    new Bag[T](x => this.f(x) + that.f(x))
  }

}
