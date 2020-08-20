var a: array 10 of integer;
var i: integer;

begin
  for i := 0 to 9 do
    a[i] := i;
  end;
  for b in a do
    print_num(b);
    newline()
  end
end.

(*<<
0
1
2
3
4
5
6
7
8
9
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
@   for i := 0 to 9 do
  mov r0, #0
  set r1, _i
  str r0, [r1]
	mov r4, #9
.L2:
  set r6, _i
  ldr r7, [r6]
  cmp r7, r4
  bgt .L3
@     a[i] := i;
  set r0, _a
	lsl r1, r7, #2
	add r0, r0, r1
	str r7, [r0]
@   end;
  ldr r0, [r6]
	add r0, r0, #1
	str r0, [r6]
	b .L2
.L3:
@   for b in a do
	mov r5, #0
.L4:
	cmp r5, #9
  bgt .L1
	set r0, _a
	lsl r1, r5, #2
	add r0, r0, r1
	ldr r6, [r0]
 	set r0, _b
	str r6, [r0]
@     print_num(b);
  mov r0, r6
  bl print_num
@     newline()
	bl newline
	add r5, r5, #1
	b .L4
.L1:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

  .comm _a, 40, 4
	.comm _i, 4, 4
@ End
]]*)
