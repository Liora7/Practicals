! keiko/lib.c
! Copyright (c) 2017 J. M. Spivey

MODULE Lib 0 0
ENDHDR

! Primitives for labs 1--3
PROC lib.print 0 0 0
CONST 32
CONST 0
GLOBAL lib.print_char
PCALL 1
LDLW 16
CONST 0
GLOBAL lib.print_num
PCALL 1
RETURN
END

PROC lib.newline 0 0 0
CONST 10
CONST 0
GLOBAL lib.print_char
PCALL 1
RETURN
END

! Primitives for ppc
PRIMDEF lib.new scratch_alloc PSI
PRIMDEF lib.open_in lib_open_in ISP
PRIMDEF lib.close_in lib_close_in VS
PRIMDEF lib.read_char lib_read_char VSP
PRIMDEF lib.print_num lib_print_num VSI
PRIMDEF lib.print_string lib_print_string VSP
PRIMDEF lib.print_char lib_print_char VSC
PRIMDEF lib.argc lib_argc IS
PRIMDEF lib.argv lib_argv VSIP
PRIMDEF lib.exit exit VSI
