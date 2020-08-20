var a: array 1 of integer;

begin
  for b in a do
    print_num(b);
    newline();
  end;
end.

(*<<
0
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
@   for b in a do
	mov r4, #0
.L2:
	cmp r4, #0
  bgt .L3
  set r0, _a
	lsl r1, r4, #2
  add r0, r0, r1
	ldr r5, [r0]
	set r0, _b
	str r5, [r0]
@     print_num(b);
	mov r0, r5
	bl print_num
@     newline();
	bl newline
@   end;
	add r4, r4, #1
  b .L2
.L3:
  ldmfd fp, {r4-r10, fp, sp, pc}
  .ltorg

  .comm _a, 4, 4
@ End
]]*)
