var a: array 3 of boolean;
var b: integer;

begin
  b := 5;
  for b in a do
    if b then print_num(4) else end;
    newline();
  end;
  print_num(b);
end.

(*<<
5
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
@   b := 5;
	mov r0, #5
	set r1, _b
  	str r0, [r1]
@   for b in a do
	mov r4, #0
.L2:
	cmp r4, #2
	bgt .L3
	set r5, _b
	set r0, _a
	add r0, r0, r4
	ldr r0, [r0]
	str r0, [r5]
@     if b then print_num(4) else end;
	ldrb r0, [r5]
	cmp r0, #0
	beq .L6
	mov r0, #4
	bl print_num
.L6:
@     newline();
	bl newline
@   end;
	add r4, r4, #1
	b .L2
.L3:
@     print_num(b);
	set r0, _b
	ldr r0, [r0]
	bl print_num
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

	.comm _a, 3, 4
	.comm _b, 4, 4
@ End
]]*)
