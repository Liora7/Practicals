object Big{

  def tooBig(y: BigInt): Boolean={
    if (y>999) true else false
  }

  def findX(): BigInt={
    var i = 0; var j = 1001
    // Inv I: i-1<=X<j && 0<=i<=j<=1001
    while(i < j){
      val m = (i+j)/2
      // i<=m<j
      if(tooBig(m)) j = m
        // i<=X<m
      else i = m+1
        // m<=X<=j
  }
  // I && i=j=m+1 -> X=m
  i-1
  }

  def bigX(): BigInt={
    var i = 0; var j = 2
    while(!tooBig(j)) j*=2
    // find upper bound for X by exponentially increasing j until X<j
    // Inv I: i-1<=X<j && 0<=i<=j<=1001
    while(i < j){
      val m = (i+j)/2
      // i<=m<j
      if(tooBig(m)) j = m
        // i<=X<m
      else i = m+1
        // m<=X<=j
  }
  // I && i=j=m+1 -> X=m
  i-1
  }

  def main(args: Array[String])={
    println(bigX)
  }
}
