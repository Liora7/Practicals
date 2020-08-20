object ds{

def main(args: Array[String])={
  val x = scala.math.pow(2, 32)
  var one = 0
  var two = 0
  var three = 0
  var four = 0
  var five = 0
  var zero = 0
  var i = 0
  while (i < x){
    if (i%6==0) zero += 1
    else if (i%6==1) one += 1
    else if (i%6==2) two += 1
    else if (i%6==3) three += 1
    else if (i%6==4) four += 1
    else if (i%6==5) five += 1
    i += 1
  }
  println(zero)
  println(one)
  println(two)
  println(three)
  println(four)
  println(five)
}
}
