# keiko/Makefile

# Config stuff
AR = ar
RANLIB = ranlib
CFLAGS = -g -O2
RTFLAGS = $(CFLAGS) -fno-strict-aliasing
GC_PAGESIZE = 4096
# End

include config.mk

all :: ppx pplink

ALL_CFLAGS = $(CFLAGS) -Wall $(INCLUDE)
ALL_RTFLAGS = $(RTFLAGS) -Wall $(HOST_DEFINES) $(INCLUDE)

PPLINK = oblink.o linker.o symtab.o template.o util.o
pplink : $(PPLINK) 
	$(CC) $(CFLAGS) -o $@ $^

$(PPLINK) : %.o : %.c
	$(CC) $(ALL_CFLAGS) -c $< -o $@

COMMON = support.o loader.o gc0.o dynlink.o
PACK = $(AR) cr $@ $^ && $(RANLIB) $@

ppx: obx-t.a lib.o
	$(CC) $(ALL_RTFLAGS) $^ -o $@

obx-t.a : interp-t.o xmain-t.o trace.o $(COMMON);		$(PACK)

# Cancel default rule
%.o: %.c

xmain.o xmain-t.o oblink.o: CFLAGS += -DREVID=\"compilers\" 

interp.o trace.o lib.o $(COMMON): %.o : %.c
	$(CC) $(ALL_RTFLAGS) -c $< -o $@

%-t.o : %.c
	$(CC) $(ALL_RTFLAGS) -DTRACE -c $< -o $@


## Cleanup

# clean: remove all object files
clean:
	rm -f pplink ppx *.[ao]

.DELETE_ON_ERROR:

###

support.o interp.o interp-t.o interp-p.o interp-d.o profile.o \
xmain.o xmain-t.o xmain-p.o xmain-d.o xmain-j.o debug.o \
trace.o loader.o jit.o jitlab.o gc0.o: \
	obx.h obcommon.h config.h exec.h

interp.o interp-t.o xmain.o xmain-t.o trace.o: \
	keiko.h

linker.o oblink.o symtab.o template.o util.o: \
	oblink.h obcommon.h config.h exec.h util.h keiko.h
