        .syntax unified
        .global foo

        .text
        .thumb_func
foo:
@ ----------------
@ Two parameters are in registers r0 and r1

        movs r2, #0
        movs r3, r1
        lsrs r4, r3, #1
        cmp r4, r0
loop1:  bls shift
back:   lsrs r4, r3, #1
        cmp r4, r0
        bls loop1
        movs r4, #0
loop2:  cmp r0, r3
        sbcs r0, r0, r3
        adcs r2, r2, r2
        lsrs r3, #1
        movs r4, r3
        cmp r3, r1
        bhs loop2
done:   movs r0, r4
        bx lr
shift:  lsrs r4, r3, #1
        movs r3, r4
       @ b back

@ Result is now in register r0
@ ----------------

