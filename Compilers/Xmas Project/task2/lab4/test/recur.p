proc baz(u: integer): integer;
begin
  if (u=0) then return u
  else return baz(u-1)
  end
end;

begin
 [print_num(baz(5)): newline()]
end.

(*<<
Correct
>>*)
