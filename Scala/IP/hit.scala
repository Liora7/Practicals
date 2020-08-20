object hit {

  def hit(arr: Array[Int]): Int = {
    var i = 0; var j = 1
    val n = arr.size
    var count = 0
    // Invariant I: count = hits a([0..j)) && 0<=i<j<=n
    while (j<n) {
      i = 0
      var hit = true
      while (i<j) {
        if (arr(i)>=arr(j)) hit = false
        i += 1
      }
      if (hit==true) count += 1
      // count = hits a([0..j+1))
      j += 1
      // count = hits a([0..j)) && 0<=i<j<=n
    }
    // I && j=n: count = hits a([o..j))
    count
  }

  def linhit(arr: Array[Int]): Int = {
    var j = 1
    val n = arr.size
    var count = 0
    var lasth = 0
    // Invariant I: count = hits a([0..j)) && 0<=i<j<=n
    while (j<n) {
      var hit = true
      var i = lasth
      if (arr(lasth)>=arr(j)) hit = false
      while (i<j) {
        if (arr(i)>=arr(j)) hit = false
        i += 1
      }
      if (hit==true) {
        count += 1
        lasth = j
      }
      // count = hits a([0..j+1))
      j += 1
      // count = hits a([0..j)) && 0<=i<j<=n
    }
    // I && j=n: count = hits a([o..j))
    count
  }

  def main(args: Array[String]){
  println(hit(args.map(_.toInt)))
  println(linhit(args.map(_.toInt)))

  }
}
