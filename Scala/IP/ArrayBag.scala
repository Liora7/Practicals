class ArrayBag extends Bag{
  // Abs: bag = { c(i) -> count | 0<=i<MAX}
  val MAX = 10; val N=15
  val c = new Array[Int](MAX)

  /** Add the maplet Int -> Count to the mapping.*/
  def add(number: Int)={
    c(number) += 1
  }

  /** Return the count of a number.*/
  def find(number: Int) : Int ={
    c(number)
  }

  def countSort(a: Array[Int]): Array[Int]={
	for (i <- 0 until MAX) a(i) += a(i-1)
	// cumulative counts
	val b = new Array[Int](N)
	// new array of size total count
	for (j <- N-1 to 1 by -1){
		b(a(j)) = j
		a(j) -= 1
}
b
}

}
