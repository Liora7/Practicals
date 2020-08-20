var x: integer;
proc baz(u: integer): integer;
begin
 x := u;
 return x
end;

begin
 [print_num(baz(37)): print_num(x)];
newline()
end.

(*<<
Possibly incorrect
>>*)
