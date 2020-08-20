var a: array 0 of integer;

begin
  for b in a do
    print_num(b);
    newline();
  end;
end.

(*<<
>>*)

(*[[
@ picoPascal compiler output
	.include "fixup.s"
	.global pmain

	.text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
    ldmfd fp, {r4-r10, fp, sp, pc}
   .ltorg

  .comm _a, 0, 4
@ End
]]*)
