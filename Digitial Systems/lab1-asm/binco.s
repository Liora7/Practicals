        .syntax unified
        .global foo

        .text
        .thumb_func
foo:
        push {r4-r7, lr}                                @ Save all registers
        ldr r4, =row @ Set r4 to base of array
        @ r4 points to the array throughout the subroutine
        movs r3, #1                                     @ row[0] = 1
        str r3, [r4, #0]
        movs r7, #0                                     @ r7 = a

recur:
        movs r3, #1                                     @ r3 = b
inner:
        lsls r3, r3, #2
        ldr r5, [r3, r4]                                @ r5 = row[b]
        subs r3, r3, #4                                 @ b -= 1
        ldr r6, [r3, r4]                                @ r6 = row[b-1]
        adds r5, r5, r6                                 @ r5 = row[b] + row[b-1]
        adds r3, r3, #4                                 @ b += 1
        str r5, [r4, r3]                                @ row[b] = row[b] + row[b-1]
        adds r3, r3, #4                                 @ b += 1
        lsrs r3, r3, #2
        cmp r3, r7
        bls inner                                       @ if b<=a, repeat

        adds r7, r7, #1                                 @ a += 1
        cmp r0, r7
        bhi recur                                       @ if a<=n, repeat

        lsls r2, r1, #2                                 @ return row[k]
        ldr r0, [r4, r2]
        pop {r4-r7, pc}                                 @ Restore regs and return
    @ Statically allocate 256 words for the array
        .bss
        .align 2
    row:
        .space 1024

@ Result is now in register r0
@ ----------------

