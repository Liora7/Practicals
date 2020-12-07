rec loop0() =
  let val x = 0 in
  loop (
    x := x+2;
    if x > 3 then exit else nil;
    x := x+3
  );
  x;;

rec test1() =
  let rec f() = exit in loop f();;

rec test2() =
  loop (let rec f() = exit in f());;

rec test3() =
  loop (let rec f() = exit in loop f());;
