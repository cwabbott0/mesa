/*
 * Copyright © 2014 Intel Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * Authors:
 *    Jason Ekstrand (jason@jlekstrand.net)
 *
 */

#pragma once

#ifndef _NIR_WORKLIST_
#define _NIR_WORKLIST_

#include "nir.h"

#ifdef __cplusplus
extern "C" {
#endif

/** Represents a double-ended queue of unique indexed entries
 *
 * The worklist datastructure guarantees that each entry is in the queue at
 * most once.  Pushing an entry onto either end of the queue is a no-op if
 * the entry is already in the queue.  In order for this to work, the entries
 * must have an index that's given to the worker functions.
 */
typedef struct {
   /* The total size of the worklist */
   unsigned size;

   /* The number of entries currently in the worklist */
   unsigned count;

   /* The offset in the array of entries at which the list starts */
   unsigned start;

   /* A bitset of all of the entries currently present in the worklist */
   BITSET_WORD *entries_present;

   /* The actual worklist */
   void **entries;
} nir_worklist;

void nir_worklist_init(nir_worklist *w, unsigned num_entries,
                       void *mem_ctx);
void nir_worklist_fini(nir_worklist *w);


static inline bool
nir_worklist_is_empty(const nir_worklist *w)
{
   return w->count == 0;
}

void nir_worklist_push_head(nir_worklist *w, void *entry, unsigned idx);

void *nir_worklist_peek_head(nir_worklist *w);

/* note that peek_head() must be called first to get the index */
void *nir_worklist_pop_head(nir_worklist *w, unsigned idx);

void nir_worklist_push_tail(nir_worklist *w, void *entry, unsigned idx);

void *nir_worklist_peek_tail(nir_worklist *w);

/* note that peek_tail() must be called first to get the index */
void *nir_worklist_pop_tail(nir_worklist *w, unsigned idx);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _NIR_WORKLIST_ */
