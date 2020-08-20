var x: integer;
begin
[ x:=1 : x:=2 ];
(* swapping would lead to a different answer *)
print_num(x);
end.

(*<<
Possibly incorrect
>>*)
