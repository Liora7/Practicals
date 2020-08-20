object Log {

  def log3(x: Int): Int = {
    var y=0; var exp = 3
    // Inv I: 3^y<=exp && 0<=y && 1<=exp<=x
    while(exp<=x){
      y += 1
      // 3^(y+1)<=x
      exp *= 3
      // I && exp<=x
    }
    // I && exp<=x -> 3^y<=x
    y
  }

  def main(args: Array[String]) = {
    println(log3(args(0).toInt))
  }

}
