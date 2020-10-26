val x = new();;
val r = new();;
r := 1;;
x := 5;;
let val l = label() in
  if !x =  0
  then !r
  else (r := !r*!x; x := !x-1; goto(l))
;;