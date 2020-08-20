# Choose C compiler depending on host type

HOST := $(shell uname -s)-$(shell uname -m)

CC-Linux-i686 = gcc
CC-Linux-x86_64 = gcc
CC-Darwin-i386 = gcc -m32
CC-Darwin-x86_64 = gcc -m32
CC-Linux-armv6l = gcc
CC-Linux-armv7l = gcc

DEF-Linux-x86_64 = -DM64X32

_CC := $(CC-$(HOST))
HOST_DEFINES = $(DEF-$(HOST))

ifndef _CC
    $(error Can't configure for host type $(HOST))
endif

CC = $(_CC) -std=gnu99
