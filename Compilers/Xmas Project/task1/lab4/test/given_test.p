type point = record x,y:integer end;
var a: array 10 of point;
var i, sum: integer;

begin
  i := 0;
  for r in a do
    r.x := i mod 3;
    r.y := i mod 5;
    i := i+1
  end;
  (* x values: 0, 1, 2, 0, 1, 2, 0, 1, 2, 0 *)
  (* y values: 0, 1, 2, 3, 4, 0, 1, 2, 3, 4 *)
  sum := 0;
  for r in a do
    sum := sum + r.x * r.y
  end;
  (* sum = 0*0 + 1*1 + 2*2 + 0*3 + 1*4 + 2*0 + 0*1 + 1*2 + 2*3 + 0*4 = 17 *)
  print_num(sum); newline()
end.

(*<<
17
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
@ i := 0;
	mov r0, #0
	set r1, _i
	str r0, [r1]
@ for r in a do
	mov r4, #0
.L2:
	cmp r4, #9
	bgt .L3
	set r6, _r
	set r0, _a
	lsl r1, r4, #3
	add r0, r0, r1
	ldr r0, [r0]
	str r0, [r6]
@ r.x := i mod 3;
	set r7, _i
	mov r1, #3
	ldr r0, [r7]
	bl int_mod
	str r0, [r6]
@ r.y := i mod 5;
	mov r1, #5
	ldr r0, [r7]
	bl int_mod
	str r0, [r6, #4]
@ i := i+1
	ldr r0, [r7]
	add r0, r0, #1
	str r0, [r7]
	add r4, r4, #1
	b .L2
.L3:
@ sum := 0;
	mov r0, #0
	set r1, _sum
	str r0, [r1]
@ for r in a do
	mov r5, #0
.L4:
	cmp r5, #9
	bgt .L5
	set r0, _a
	lsl r1, r5, #3
	add r0, r0, r1
	ldr r6, [r0]
	set r7, _r
	str r6, [r7]
@ sum := sum + r.x * r.y
	set r8, _sum
	ldr r0, [r8]
	ldr r1, [r7, #4]
	mul r1, r6, r1
	add r0, r0, r1
	str r0, [r8]
	add r5, r5, #1
	b .L4
.L5:
@ print_num(sum); newline()
	set r0, _sum
	ldr r0, [r0]
	bl print_num
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

	.comm _a, 80, 4
	.comm _i, 4, 4
	.comm _sum, 4, 4
@ End
]]*)
