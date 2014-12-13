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

#include "nir.h"

/*
 * Implements "copy splitting" which is similar to structure splitting only
 * it works on copy operations rather than the datatypes themselves.  The
 * GLSL language allows you to copy one variable to another an entire
 * structure (which may contain arrays or other structures) at a time.
 * Normally, in a language such as C this would be handled by a "structure
 * splitting" pass that breaks up the structures.  Unfortunately for us,
 * structures used in inputs or outputs can't be split.  Therefore,
 * regardlesss of what we do, we have to be able to copy to/from
 * structures.
 *
 * The primary purpose of structure splitting is to allow you to better
 * optimize variable access and lower things to registers where you can.
 * The primary issue here is that, if you lower the copy to a bunch of
 * loads and stores, you loose a lot of information about the copy
 * operation that you would like to keep around.  To solve this problem, we
 * have a "copy splitting" pass that, instead of splitting the structures
 * or lowering the copy into loads and storres, splits the copy operation
 * into a bunch of copy operations one for each leaf of the structure tree.
 * If an intermediate array is encountered, it is referenced with a
 * wildcard reference to indicate that the entire array is to be copied.
 *
 * As things become direct, array copies may be able to be losslessly
 * lowered to having fewer and fewer wildcards.  However, until that
 * happens we want to keep the information about the arrays intact.
 *
 * Prior to the copy splitting pass, there are no wildcard references but
 * there may be incomplete references where the tail of the deref chain is
 * an array or a structure and not a specific element.  After the copy
 * splitting pass has completed, every variable deref will be a full-length
 * dereference pointing to a single leaf in the structure type tree with
 * possibly a few wildcard array dereferences.
 */

struct split_var_copies_state {
   void *mem_ctx;
   void *dead_ctx;
};

static nir_deref *
get_deref_tail(nir_deref *deref)
{
   while (deref->child != NULL)
      deref = deref->child;
   return deref;
}

static void
nir_split_var_copy_instr(nir_intrinsic_instr *old_copy,
                         nir_deref *dest_head, nir_deref *src_head,
                         nir_deref *dest_tail, nir_deref *src_tail,
                         struct split_var_copies_state *state)
{
   assert(src_tail->type == dest_tail->type);

   switch (glsl_get_base_type(src_tail->type)) {
   case GLSL_TYPE_ARRAY: {
      nir_deref_array *deref = nir_deref_array_create(state->dead_ctx);
      deref->deref.type = glsl_get_array_element(src_tail->type);
      deref->deref_array_type = nir_deref_array_type_wildcard;

      src_tail->child = &deref->deref;
      dest_tail->child = &deref->deref;
      nir_split_var_copy_instr(old_copy, dest_head, src_head,
                               dest_tail->child, src_tail->child, state);
      src_tail->child = NULL;
      dest_tail->child = NULL;
      break;
   }

   case GLSL_TYPE_STRUCT:
      for (unsigned i = 0; i < glsl_get_length(src_tail->type); i++) {
         nir_deref_struct *deref = nir_deref_struct_create(state->dead_ctx, i);
         deref->deref.type = glsl_get_struct_field(src_tail->type, i);

         src_tail->child = &deref->deref;
         dest_tail->child = &deref->deref;

         nir_split_var_copy_instr(old_copy, dest_head, src_head,
                                  dest_tail->child, src_tail->child, state);
      }
      src_tail->child = NULL;
      dest_tail->child = NULL;
      break;

   case GLSL_TYPE_UINT:
   case GLSL_TYPE_INT:
   case GLSL_TYPE_FLOAT:
   case GLSL_TYPE_BOOL:
      if (glsl_type_is_matrix(src_tail->type)) {
         nir_deref_array *deref = nir_deref_array_create(state->dead_ctx);
         deref->deref.type = glsl_get_column_type(src_tail->type);
         deref->deref_array_type = nir_deref_array_type_wildcard;

         src_tail->child = &deref->deref;
         dest_tail->child = &deref->deref;
         nir_split_var_copy_instr(old_copy, dest_head, src_head,
                                  dest_tail->child, src_tail->child, state);
         src_tail->child = NULL;
         dest_tail->child = NULL;
      } else {
         nir_intrinsic_instr *new_copy =
            nir_intrinsic_instr_create(state->mem_ctx, nir_intrinsic_copy_var);

         nir_deref *src = nir_copy_deref(state->mem_ctx, src_head);
         nir_deref *dest = nir_copy_deref(state->mem_ctx, dest_head);

         new_copy->variables[0] = nir_deref_as_var(dest);
         new_copy->variables[1] = nir_deref_as_var(src);

         nir_instr_insert_after(&old_copy->instr, &new_copy->instr);
      }
      break;

   case GLSL_TYPE_SAMPLER:
   case GLSL_TYPE_IMAGE:
   case GLSL_TYPE_ATOMIC_UINT:
   case GLSL_TYPE_INTERFACE:
   default:
      unreachable("Cannot copy these types");
   }
}

static bool
nir_split_var_copies_block(nir_block *block, void *void_state)
{
   struct split_var_copies_state *state = void_state;

   nir_foreach_instr_safe(block, instr) {
      if (instr->type != nir_instr_type_intrinsic)
         continue;

      nir_intrinsic_instr *intrinsic = nir_instr_as_intrinsic(instr);
      if (intrinsic->intrinsic != nir_intrinsic_copy_var)
         continue;

      nir_deref *dest_head = &intrinsic->variables[0]->deref;
      nir_deref *src_head = &intrinsic->variables[1]->deref;
      nir_deref *dest_tail = get_deref_tail(dest_head);
      nir_deref *src_tail = get_deref_tail(src_head);

      switch (glsl_get_base_type(src_tail->type)) {
      case GLSL_TYPE_ARRAY:
      case GLSL_TYPE_STRUCT:
         nir_split_var_copy_instr(intrinsic, dest_head, src_head,
                                  dest_tail, src_tail, state);
         nir_instr_remove(&intrinsic->instr);
         ralloc_steal(state->dead_ctx, instr);
         break;
      case GLSL_TYPE_FLOAT:
      case GLSL_TYPE_INT:
      case GLSL_TYPE_UINT:
      case GLSL_TYPE_BOOL:
         if (glsl_type_is_matrix(src_tail->type)) {
            nir_split_var_copy_instr(intrinsic, dest_head, src_head,
                                     dest_tail, src_tail, state);
            nir_instr_remove(&intrinsic->instr);
            ralloc_steal(state->dead_ctx, instr);
         }
         break;
      default:
         unreachable("Invalid type");
         break;
      }
   }

   return true;
}

static void
nir_split_var_copies_impl(nir_function_impl *impl)
{
   struct split_var_copies_state state;

   state.mem_ctx = ralloc_parent(impl);
   state.dead_ctx = ralloc_context(NULL);

   nir_foreach_block(impl, nir_split_var_copies_block, &state);

   ralloc_free(state.dead_ctx);
}

void
nir_split_var_copies(nir_shader *shader)
{
   nir_foreach_overload(shader, overload) {
      if (overload->impl)
         nir_split_var_copies_impl(overload->impl);
   }
}
