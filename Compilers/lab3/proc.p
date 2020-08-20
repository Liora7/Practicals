(* lab2/proc.p *)

proc double(x);
begin
return x + x
end;
proc apply3(proc f(x));
begin
return f(3)
end;
begin
print_num(apply3(double));
newline()
end.
