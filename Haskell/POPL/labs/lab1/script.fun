
rec append(xs, ys) =
  if xs = nil then ys else (head(xs) : append(tail(xs), ys));;

-- append was tested with all possible combinations of empty and non-empty lists

rec concat(xss) =
  if xss = nil then nil else append(head(xss), concat(tail(xss)));;

-- concat was tested on the empty list, the list containing the empty list, and lists containing several empty and non-empty lists, as well as nested lists


val yyy = list(1, list(2, 3), list(4, list(5), 6));;

rec flatten(x) =
  if integer(x) then list(x) else if x = nil then nil else append(flatten(head(x)), flatten(tail(x)));;

-- flatten was tested on the same lists as concat, as well as yyy and lists of empty and non-empty lists which were nested to varying amounts, e.g. list(list(), list(1), list(), list(2, list(3), list(list())))

rec flatsum(xs) =
  if integer(xs) then xs else if xs = nil then 0 else (flatsum(head(xs)) +  flatsum(tail(xs)));;

-- flatsum was tested on the same lists as flatten


val flatsum1(xs) =
  let rec loop(stack, s) =
    if stack = nil then s else
      if integer(head(stack)) then loop(tail(stack), s+head(stack)) else if head(stack) = nil then loop(tail(stack), s) else loop(head(head(stack)):tail(head(stack)):tail(stack), s) in
  loop(xs, 0);;

-- flatsum1 was tested on the same lists as flatsum

val flatsum2(xs) =
  let val stack = new() in let val s = new() in let val h = new() in
    stack := xs; s:= 0; h := nil;
    while !stack <> nil do
      (h := head(!stack);
      if integer(!h) then (stack := tail(!stack); s := !s + !h)
      else (if !h = nil then (stack := tail(!stack))
      else (stack := head(!h):tail(!h):tail(!stack))));
    !s;;
