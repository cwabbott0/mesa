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

typedef nir_worklist nir_ssa_def_worklist;

static inline void
nir_ssa_def_worklist_init(nir_ssa_def_worklist *w, unsigned num_ssa_defs,
                        void *mem_ctx)
{
   nir_worklist_init(w, num_ssa_defs, mem_ctx);
}

static inline void
nir_ssa_def_worklist_fini(nir_ssa_def_worklist *w)
{
   nir_worklist_fini(w);
}

static inline bool
nir_ssa_def_worklist_is_empty(nir_ssa_def_worklist *w)
{
   return nir_worklist_is_empty(w);
}

static inline void
nir_ssa_def_worklist_push_head(nir_ssa_def_worklist *w, nir_ssa_def *ssa_def)
{
   nir_worklist_push_head(w, ssa_def, ssa_def->index);
}

static inline nir_ssa_def *
nir_ssa_def_worklist_peek_head(nir_ssa_def_worklist *w)
{
   return (nir_ssa_def *) nir_worklist_peek_head(w);
}

static inline nir_ssa_def *
nir_ssa_def_worklist_pop_head(nir_ssa_def_worklist *w)
{
   nir_ssa_def *ssa_def = nir_ssa_def_worklist_peek_head(w);
   nir_worklist_pop_head(w, ssa_def->index);
   return ssa_def;
}

static inline void
nir_ssa_def_worklist_push_tail(nir_ssa_def_worklist *w, nir_ssa_def *ssa_def)
{
   nir_worklist_push_tail(w, ssa_def, ssa_def->index);
}

static inline nir_ssa_def *
nir_ssa_def_worklist_peek_tail(nir_ssa_def_worklist *w)
{
   return (nir_ssa_def *) nir_worklist_peek_tail(w);
}

static inline nir_ssa_def *
nir_ssa_def_worklist_pop_tail(nir_ssa_def_worklist *w)
{
   nir_ssa_def *ssa_def = nir_ssa_def_worklist_peek_tail(w);
   nir_worklist_pop_tail(w, ssa_def->index);
   return ssa_def;
}


#ifdef __cplusplus
}
#endif



