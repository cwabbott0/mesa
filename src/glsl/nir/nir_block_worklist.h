/*
 * Copyright © 2014 Connor Abbott
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
 *    Connor Abbott (cwabbott0@gmail.com)
 *
 */

#include "nir_worklist.h"

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

typedef nir_worklist nir_block_worklist;

static inline void
nir_block_worklist_init(nir_block_worklist *w, unsigned num_blocks,
                        void *mem_ctx)
{
   nir_worklist_init(w, num_blocks, mem_ctx);
}

static inline void
nir_block_worklist_fini(nir_block_worklist *w)
{
   nir_worklist_fini(w);
}

static bool
block_worklist_add_block(nir_block *block, void *w)
{
   nir_worklist_push_tail((nir_worklist *)w, block, block->index);
   return true;
}

static inline void
nir_block_worklist_add_all(nir_block_worklist *w, nir_function_impl *impl)
{
   nir_foreach_block(impl, block_worklist_add_block, w);
}

static inline bool
nir_block_worklist_is_empty(nir_block_worklist *w)
{
   return nir_worklist_is_empty(w);
}

static inline void
nir_block_worklist_push_head(nir_block_worklist *w, nir_block *block)
{
   nir_worklist_push_head(w, block, block->index);
}

static inline nir_block *
nir_block_worklist_peek_head(nir_block_worklist *w)
{
   return (nir_block *) nir_worklist_peek_head(w);
}

static inline nir_block *
nir_block_worklist_pop_head(nir_block_worklist *w)
{
   nir_block *block = nir_block_worklist_peek_head(w);
   nir_worklist_pop_head(w, block->index);
   return block;
}

static inline void
nir_block_worklist_push_tail(nir_block_worklist *w, nir_block *block)
{
   nir_worklist_push_tail(w, block, block->index);
}

static inline nir_block *
nir_block_worklist_peek_tail(nir_block_worklist *w)
{
   return (nir_block *) nir_worklist_peek_tail(w);
}

static inline nir_block *
nir_block_worklist_pop_tail(nir_block_worklist *w)
{
   nir_block *block = nir_block_worklist_peek_tail(w);
   nir_worklist_pop_tail(w, block->index);
   return block;
}


#ifdef __cplusplus
}
#endif


