/*
 * gc0.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: gc.c 1557 2010-01-24 20:59:31Z mike $
 */

/* keiko/gc0.c */
/* Copyright (c) 2017 J. M. Spivey */

#include "obx.h"

#include <sys/mman.h>

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

#ifdef M64X32
#define MAP_FLAGS MAP_PRIVATE|MAP_32BIT|MAP_ANONYMOUS
#else
#define MAP_FLAGS MAP_PRIVATE|MAP_ANONYMOUS
#endif

static void *get_memory(unsigned size) {
     void *p = mmap(NULL, size, PROT_READ|PROT_WRITE,
                    MAP_FLAGS, -1, 0);
     if (p == MAP_FAILED) panic("Out of memory");
     return p;
}

#define CHUNK 16384

static unsigned char *alloc = NULL;
static unsigned char *limit = NULL;

#define roundup(x, n) (((x)+(n-1))&~(n-1))

void *scratch_alloc(unsigned size) {
     if (size % PAGESIZE == 0 || size > CHUNK)
          return get_memory(roundup(size, PAGESIZE));
     
     if (alloc == NULL || size > limit - alloc) {
          alloc = get_memory(CHUNK);
          limit = alloc + CHUNK;
     }

     void *p = alloc;
     alloc += size;
     return p;
}

/* gc_init -- initialise everything */
void gc_init(void) {
}
