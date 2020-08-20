object search{

def search(a: Array[Int], x: Int) : Int = {
  val N = a.size
// invariant I: a[0..i) < x <= a[j..N) && 0 <= i <= j <= N
  var i = 0; var j = N
  while(i < j){
  val m = (i+j)/2 // i <= m < j
  if(a(m) < x) i = m+1 else j = m
}
// I && i = j, so a[0..i) < x <= a[i..N)
i
}

def intsqrt(y: Int): Int = {
  var i=0; var j = y+1
  while (i<j){
    var lower = (i+j)/3
    var upper = (2*(i+j))/3
    // Inv I: 0<=lower<=upper<=y && 0<=i<=j<=y && i^2<=sqrt(y)<=j^2
    if(lower*lower<y){
      if(upper*upper<=y) i=upper
      // I
      else{
        i=lower; j=upper-1
        // I
    }
  }
    else j=lower
    // I
  }
  // i=j ** i^2=sqrt(y)=j^2
  i
}

def main(args: Array[String]){
  val a = Array[Int]()
  println(intsqrt(args(0).toInt))
}

}
