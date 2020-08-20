class BitMapSet extends IntSet{
  //Abs: IntSet = {i, x_i | a(i) == true iff x_i ∈ [0..N) && x_i ∈ IntSet}

  //DTI: ∀i,j∈[0..N) (i!=j => x_i != x_j)
val N = 20
  private val a = new Array[Boolean](N)

  // Add elem to the set.
  def add(elem: Int)={
    a(elem) = true
  }

  // Does the set contain elem?
  def isIn(elem: Int): Boolean={
    a(elem)
  }

  // Remove elem from the set.
  def remove(elem: Int)={
    a(elem) = false
  }

  // The size of the set.
  def size : Int={
    var size = 0
    for (i<- 0 until N){
      if (a(i)) size +=1
    }
    size
  }
}
