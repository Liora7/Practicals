object Partition{

  var a = Array(6, 5, 4, 3, 2, 1)

  def QSort(left: Int, r: Int) : Unit = {
    var l = left
    while(r-l > 1){ // nothing to do if segment empty or singleton
      val (i, j) = partitions(l,r)
      QSort(l,i)
      l = j+1
    }
  }

  def Sort(l: Int, r: Int) : Unit = {
if(r-l > 1){ // nothing to do if segment empty or singleton
val k = partition(l,r)
Sort(l,k); Sort(k+1,r)
}
}

def partition(l: Int, r: Int) : Int = {
val x = a(l) // pivot
// Invariant a[l+1..i) < x = a(l) <= a[j..r) && l < i <= j <= r
// && a[0..l) = a_0[0..l) && a[r..N) = a_0[r..N)
// && a[l..r) is a permutation of a_0[l..r)
var i = l+1; var j = r
while(i < j){
if(a(i) < x) i += 1
else{ val t = a(i); a(i) = a(j-1); a(j-1) = t; j -= 1 }
}
// swap pivot into position
a(l) = a(i-1); a(i-1) = x
i-1 // position of the pivot
}

def partitions(l: Int, r: Int) : (Int,Int) = {
  val x = a(l) // pivot
          // Invariant a[l+1..i) < x = a(l) = a[i..j) < a[k..r) && l <= i < j <= k <= r
          //    && a[0..l) = a_0[0..l) && a[r..N) = a_0[r..N)
          //    && a[l..r) is a permutation of a_0[l..r)
          var i = l+1; var k = r; var j = i
          while(j < k){
                    if(a(j)<x){ val t = a(i); a(i) = a(j); a(j) = t; i += 1; j += 1 }
                    else if(a(j)>x){ val t = a(j); a(j) = a(k-1); a(k-1) = t; k -= 1 }
                    else j += 1
                  }
          // swap pivot into position
          a(l) = a(i-1); a(i-1) = x
          (i-1, j-1) // values of i and j s.t. a[l+1..i) < x = a[i..j) < a[i..r)
}

  def main(args: Array[String])={
    QSort(args(0).toInt, args(1).toInt)
    println()
    for (i<- 0 until a.size) print(a(i) + " ")
    println()
  }

}
