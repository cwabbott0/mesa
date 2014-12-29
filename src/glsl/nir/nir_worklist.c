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

#include "nir_worklist.h"

void
nir_worklist_init(nir_worklist *w, unsigned num_entries,
                  void *mem_ctx)
{
   w->size = num_entries;
   w->count = 0;
   w->start = 0;

   w->entries_present = rzalloc_array(mem_ctx, BITSET_WORD,
                                     BITSET_WORDS(num_entries));
   w->entries = ralloc_array(mem_ctx, void *, num_entries);
}

void
nir_worklist_fini(nir_worklist *w)
{
   ralloc_free(w->entries_present);
   ralloc_free(w->entries);
}

void
nir_worklist_push_head(nir_worklist *w, void *entry, unsigned idx)
{
   /* Pushing an entry we already have is a no-op */
   if (BITSET_TEST(w->entries_present, idx))
      return;

   assert(w->count < w->size);

   if (w->start == 0)
      w->start = w->size - 1;
   else
      w->start--;

   w->count++;

   w->entries[w->start] = entry;
   BITSET_SET(w->entries_present, idx);
}

void *
nir_worklist_peek_head(nir_worklist *w)
{
   assert(w->count > 0);

   return w->entries[w->start];
}

void *
nir_worklist_pop_head(nir_worklist *w, unsigned idx)
{
   assert(w->count > 0);

   unsigned head = w->start;

   w->start = (w->start + 1) % w->size;
   w->count--;
   
   BITSET_CLEAR(w->entries_present, idx);
   return w->entries[head];
}

void
nir_worklist_push_tail(nir_worklist *w, void *entry, unsigned idx)
{
   /* Pushing a block we already have is a no-op */
   if (BITSET_TEST(w->entries_present, idx))
      return;

   assert(w->count < w->size);

   w->count++;

   unsigned tail = w->start = (w->start + w->count - 1) % w->size;

   w->entries[tail] = entry;
   BITSET_SET(w->entries_present, idx);
}

void *
nir_worklist_peek_tail(nir_worklist *w)
{
   assert(w->count > 0);

   unsigned tail = w->start = (w->start + w->count - 1) % w->size;

   return w->entries[tail];
}

void *
nir_worklist_pop_tail(nir_worklist *w, unsigned idx)
{
   assert(w->count > 0);

   unsigned tail = w->start = (w->start + w->count - 1) % w->size;

   w->count--;

   BITSET_CLEAR(w->entries_present, idx);
   return w->entries[tail];
}
