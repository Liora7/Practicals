/* lib.c */
/* Copyright (c) 2017 J. M. Spivey */

#include "primtab.h"

int lib_argc(void) {
     return saved_argc;
}

void lib_argv(int n, char *s) {
     /* Buffer overflow waiting to happen */
     strcpy(s, saved_argv[n]);
}

void lib_print_num(int n) {
     printf("%d", n);
}

void lib_print_string(char *s) {
     fputs(s, stdout);
}

void lib_print_char(char c) {
     putchar(c);
}

static FILE *infile = NULL;

int lib_open_in(char *name) {
     FILE *f = fopen(name, "r");
     if (f == NULL) return 0;
     if (infile != NULL) fclose(infile);
     infile = f;
     return 1;
}

void lib_close_in(void) {
     if (infile == NULL) return;
     fclose(infile);
     infile = NULL;
}

void lib_read_char(char *p) {
     FILE *f = (infile == NULL ? stdin : infile);
     int ch = fgetc(f);
     *p = (ch == EOF ? 127 : ch);
}

#define PRIMS(direct, indirect, wrapper) \
     wrapper(scratch_alloc, P, I) \
     wrapper(lib_argc, I) \
     wrapper(lib_argv, V, I, P) \
     wrapper(lib_print_num, V, I) \
     wrapper(lib_print_string, V, P) \
     wrapper(lib_print_char, V, C) \
     wrapper(lib_open_in, I, P) \
     wrapper(lib_close_in, V) \
     wrapper(lib_read_char, V, P) \
     wrapper(exit, V, I)

/* Using PPRIMTAB adds an offset to compensate for static links */
PPRIMTAB(PRIMS)
