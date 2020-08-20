(* lab1/binSearch.p *)

begin
  a := 1;
  n := 200000000;
  b := 1;
  while b * b < n do
    a := b;
    b := 2 * b;
  end;
  while a < b do
    m := (a + b) div 2;
    if m * m > n then
      b := m - 1;
    else
      a := m;
    end;
  end;
  print a;
end.

(*<<
 14142
>>*)
