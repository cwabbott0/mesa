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
 *    Connor Abbott (cwabbott0@gmail.com)
 *
 */

#include "glsl_to_nir.h"
#include "ir_visitor.h"
#include "ir_hierarchical_visitor.h"
#include "ir.h"

/*
 * pass to lower GLSL IR to NIR
 *
 * This will lower variable dereferences to loads/stores of corresponding
 * variables in NIR - the variables will be converted to registers in a later
 * pass.
 */

namespace {

class nir_visitor : public ir_visitor
{
public:
   nir_visitor(nir_shader *shader, bool supports_ints);
   ~nir_visitor();

   virtual void visit(ir_variable *);
   virtual void visit(ir_function *);
   virtual void visit(ir_function_signature *);
   virtual void visit(ir_loop *);
   virtual void visit(ir_if *);
   virtual void visit(ir_discard *);
   virtual void visit(ir_loop_jump *);
   virtual void visit(ir_return *);
   virtual void visit(ir_call *);
   virtual void visit(ir_assignment *);
   virtual void visit(ir_emit_vertex *);
   virtual void visit(ir_end_primitive *);
   virtual void visit(ir_expression *);
   virtual void visit(ir_swizzle *);
   virtual void visit(ir_texture *);
   virtual void visit(ir_constant *);
   virtual void visit(ir_dereference_variable *);
   virtual void visit(ir_dereference_record *);
   virtual void visit(ir_dereference_array *);

   void create_function(ir_function *ir);

private:
   void create_overload(ir_function_signature *ir, nir_function *function);
   void add_instr(nir_instr *instr, unsigned num_components);
   nir_src evaluate_rvalue(ir_rvalue *ir);

   nir_alu_instr *emit(nir_op op, unsigned dest_size, nir_src *srcs);
   nir_alu_instr *emit(nir_op op, unsigned dest_size, nir_src src1);
   nir_alu_instr *emit(nir_op op, unsigned dest_size, nir_src src1,
		       nir_src src2);
   nir_alu_instr *emit(nir_op op, unsigned dest_size, nir_src src1,
		       nir_src src2, nir_src src3);

   bool supports_ints;

   nir_shader *shader;
   nir_function_impl *impl;
   exec_list *cf_node_list;
   nir_instr *result; /* result of the expression tree last visited */

   /* the head of the dereference chain we're creating */
   nir_deref_var *deref_head;
   /* the tail of the dereference chain we're creating */
   nir_deref *deref_tail;

   nir_variable *var; /* variable created by ir_variable visitor */

   /* whether the IR we're operating on is per-function or global */
   bool is_global;

   /* map of ir_variable -> nir_variable */
   struct hash_table *var_table;

   /* map of ir_function_signature -> nir_function_overload */
   struct hash_table *overload_table;
};

/*
 * This visitor runs before the main visitor, calling create_function() for
 * each function so that the main visitor can resolve forward references in
 * calls.
 */

class nir_function_visitor : public ir_hierarchical_visitor
{
public:
   nir_function_visitor(nir_visitor *v) : visitor(v)
   {
   }
   virtual ir_visitor_status visit_enter(ir_function *);

private:
   nir_visitor *visitor;
};

}; /* end of anonymous namespace */

nir_shader *
glsl_to_nir(exec_list *ir, _mesa_glsl_parse_state *state,
	    bool native_integers)
{
   nir_shader *shader = nir_shader_create(NULL);

   if (state) {
      shader->num_user_structures = state->num_user_structures;
      shader->user_structures = ralloc_array(shader, glsl_type *,
					     shader->num_user_structures);
      memcpy(shader->user_structures, state->user_structures,
	    shader->num_user_structures * sizeof(glsl_type *));
   } else {
      shader->num_user_structures = 0;
      shader->user_structures = NULL;
   }

   nir_visitor v1(shader, native_integers);
   nir_function_visitor v2(&v1);
   v2.run(ir);
   visit_exec_list(ir, &v1);

   return shader;
}

nir_visitor::nir_visitor(nir_shader *shader, bool supports_ints)
{
   this->supports_ints = supports_ints;
   this->shader = shader;
   this->is_global = true;
   this->var_table = _mesa_hash_table_create(NULL, _mesa_key_pointer_equal);
   this->overload_table = _mesa_hash_table_create(NULL,
						  _mesa_key_pointer_equal);
}

nir_visitor::~nir_visitor()
{
   _mesa_hash_table_destroy(this->var_table, NULL);
   _mesa_hash_table_destroy(this->overload_table, NULL);
}

static nir_constant *
constant_copy(ir_constant *ir, void *mem_ctx)
{
   if (ir == NULL)
      return NULL;

   nir_constant *ret = ralloc(mem_ctx, nir_constant);

   unsigned total_elems = ir->type->components();
   unsigned i;
   switch (ir->type->base_type) {
      case GLSL_TYPE_UINT:
	 for (i = 0; i < total_elems; i++)
	    ret->value.u[i] = ir->value.u[i];
	 break;

      case GLSL_TYPE_INT:
	 for (i = 0; i < total_elems; i++)
	    ret->value.i[i] = ir->value.i[i];
	 break;

      case GLSL_TYPE_FLOAT:
	 for (i = 0; i < total_elems; i++)
	    ret->value.f[i] = ir->value.f[i];
	 break;

      case GLSL_TYPE_BOOL:
	 for (i = 0; i < total_elems; i++)
	    ret->value.b[i] = ir->value.b[i];
	 break;

      case GLSL_TYPE_STRUCT:
	 ret->elements = ralloc_array(mem_ctx, nir_constant *,
				      ir->type->length);
	 i = 0;
	 foreach_in_list(ir_constant, field, &ir->components) {
	    ret->elements[i] = constant_copy(field, mem_ctx);
	    i++;
	 }
	 break;

      case GLSL_TYPE_ARRAY:
	 ret->elements = ralloc_array(mem_ctx, nir_constant *,
				      ir->type->length);

	 for (i = 0; i < ir->type->length; i++)
	    ret->elements[i] = constant_copy(ir->array_elements[i], mem_ctx);
	 break;

      default:
	 assert(0);
	 break;
   }

   return ret;
}

void
nir_visitor::visit(ir_variable *ir)
{
   nir_variable *var = ralloc(shader, nir_variable);
   var->type = ir->type;
   var->name = ralloc_strdup(var, ir->name);

   if (ir->max_ifc_array_access != NULL) {
      unsigned size = ir->get_interface_type()->length;
      var->max_ifc_array_access = ralloc_array(var, unsigned, size);
      memcpy(var->max_ifc_array_access, ir->max_ifc_array_access,
	     size * sizeof(unsigned));
   } else {
      var->max_ifc_array_access = NULL;
   }

   var->data.read_only = ir->data.read_only;
   var->data.centroid = ir->data.centroid;
   var->data.sample = ir->data.sample;
   var->data.invariant = ir->data.invariant;

   switch(ir->data.mode) {
      case ir_var_auto:
      case ir_var_temporary:
	 if (is_global)
	    var->data.mode = nir_var_global;
	 else
	    var->data.mode = nir_var_local;
	 break;

      case ir_var_function_in:
      case ir_var_function_out:
      case ir_var_function_inout:
      case ir_var_const_in:
	 var->data.mode = nir_var_local;
	 break;

      case ir_var_shader_in:
	 var->data.mode = nir_var_shader_in;
	 break;

      case ir_var_shader_out:
	 var->data.mode = nir_var_shader_out;
	 break;

      case ir_var_uniform:
	 var->data.mode = nir_var_uniform;
	 break;


      case ir_var_system_value:
	 var->data.mode = nir_var_system_value;
	 break;

      default:
	 assert(0);
	 break;
   }

   var->data.interpolation = ir->data.interpolation;
   var->data.origin_upper_left = ir->data.origin_upper_left;
   var->data.pixel_center_integer = ir->data.pixel_center_integer;
   var->data.explicit_location = ir->data.explicit_location;
   var->data.explicit_index = ir->data.explicit_index;
   var->data.explicit_binding = ir->data.explicit_binding;
   var->data.has_initializer = ir->data.has_initializer;
   var->data.is_unmatched_generic_inout = ir->data.is_unmatched_generic_inout;
   var->data.location_frac = ir->data.location_frac;
   var->data.from_named_ifc_block_array = ir->data.from_named_ifc_block_array;
   var->data.from_named_ifc_block_nonarray = ir->data.from_named_ifc_block_nonarray;

   switch (ir->data.depth_layout) {
      case ir_depth_layout_none:
	 var->data.depth_layout = nir_depth_layout_none;
	 break;
      case ir_depth_layout_any:
	 var->data.depth_layout = nir_depth_layout_any;
	 break;
      case ir_depth_layout_greater:
	 var->data.depth_layout = nir_depth_layout_greater;
	 break;
      case ir_depth_layout_less:
	 var->data.depth_layout = nir_depth_layout_less;
	 break;
      case ir_depth_layout_unchanged:
	 var->data.depth_layout = nir_depth_layout_unchanged;
	 break;
      default:
	 assert(0);
	 break;
   }

   var->data.location = ir->data.location;
   var->data.index = ir->data.index;
   var->data.binding = ir->data.binding;
   var->data.atomic.buffer_index = ir->data.atomic.buffer_index;
   var->data.atomic.offset = ir->data.atomic.offset;
   var->data.image.read_only = ir->data.image.read_only;
   var->data.image.write_only = ir->data.image.write_only;
   var->data.image.coherent = ir->data.image.coherent;
   var->data.image._volatile = ir->data.image._volatile;
   var->data.image.restrict_flag = ir->data.image.restrict_flag;
   var->data.image.format = ir->data.image.format;
   var->data.max_array_access = ir->data.max_array_access;

   var->num_state_slots = ir->num_state_slots;
   var->state_slots = ralloc_array(var, nir_state_slot, var->num_state_slots);
   for (unsigned i = 0; i < var->num_state_slots; i++) {
      for (unsigned j = 0; j < 5; j++)
	 var->state_slots[i].tokens[j] = ir->state_slots[i].tokens[j];
      var->state_slots[i].swizzle = ir->state_slots[i].swizzle;
   }

   var->constant_value = constant_copy(ir->constant_value, var);
   var->constant_initializer = constant_copy(ir->constant_initializer, var);

   var->interface_type = ir->get_interface_type();

   switch (var->data.mode) {
      case nir_var_local:
	 exec_list_push_tail(&impl->locals, &var->node);
	 break;

      case nir_var_global:
	 exec_list_push_tail(&shader->globals, &var->node);
	 break;

      case nir_var_shader_in:
	 _mesa_hash_table_insert(shader->inputs, _mesa_hash_string(var->name),
				 var->name, var);
	 break;

      case nir_var_shader_out:
	 _mesa_hash_table_insert(shader->outputs, _mesa_hash_string(var->name),
				 var->name, var);
	 break;

      case nir_var_uniform:
	 _mesa_hash_table_insert(shader->uniforms, _mesa_hash_string(var->name),
				 var->name, var);
	 break;

      case nir_var_system_value:
	 exec_list_push_tail(&shader->system_values, &var->node);
	 break;

      default:
	 assert(0);
	 break;
   }

   _mesa_hash_table_insert(var_table, _mesa_hash_pointer(ir), ir, var);
   this->var = var;
}

ir_visitor_status
nir_function_visitor::visit_enter(ir_function *ir)
{
   visitor->create_function(ir);
   return visit_continue_with_parent;
}


void
nir_visitor::create_function(ir_function *ir)
{
   nir_function *func = nir_function_create(this->shader, ir->name);
   foreach_in_list(ir_function_signature, sig, &ir->signatures) {
      create_overload(sig, func);
   }
}



void
nir_visitor::create_overload(ir_function_signature *ir, nir_function *function)
{
   if (ir->is_intrinsic)
      return;

   nir_function_overload *overload = nir_function_overload_create(function);

   unsigned num_params = ir->parameters.length();
   overload->num_params = num_params;
   overload->params = ralloc_array(shader, nir_parameter, num_params);

   unsigned i = 0;
   foreach_in_list(ir_variable, param, &ir->parameters) {
      switch (param->data.mode) {
	 case ir_var_function_in:
	    overload->params[i].param_type = nir_parameter_in;
	    break;

	 case ir_var_function_out:
	    overload->params[i].param_type = nir_parameter_out;
	    break;

	 case ir_var_function_inout:
	    overload->params[i].param_type = nir_parameter_inout;
	    break;

	 default:
	    assert(0);
	    break;
      }

      overload->params[i].type = param->type;
      i++;
   }

   overload->return_type = ir->return_type;

   _mesa_hash_table_insert(this->overload_table, _mesa_hash_pointer(ir), ir,
			   overload);
}

void
nir_visitor::visit(ir_function *ir)
{
   foreach_in_list(ir_function_signature, sig, &ir->signatures)
      sig->accept(this);
}

void
nir_visitor::visit(ir_function_signature *ir)
{
   if (ir->is_intrinsic)
      return;

   struct hash_entry *entry = _mesa_hash_table_search(this->overload_table,
						      _mesa_hash_pointer(ir),
						      ir);

   assert(entry);
   nir_function_overload *overload = (nir_function_overload *) entry->data;

   if (ir->is_defined) {
      nir_function_impl *impl = nir_function_impl_create(overload);
      this->impl = impl;

      unsigned num_params = overload->num_params;
      impl->num_params = num_params;
      impl->params = ralloc_array(this->shader, nir_variable *, num_params);
      unsigned i = 0;
      foreach_in_list(ir_variable, param, &ir->parameters) {
	 param->accept(this);
	 impl->params[i] = this->var;
	 i++;
      }

      if (overload->return_type == glsl_type::void_type) {
	 impl->return_var = NULL;
      } else {
	 impl->return_var = ralloc(this->shader, nir_variable);
	 impl->return_var->name = ralloc_strdup(impl->return_var,
						"return_var");
	 impl->return_var->type = overload->return_type;
      }

      this->is_global = false;

      this->cf_node_list = &impl->body;
      visit_exec_list(&ir->body, this);

      this->is_global = true;
   } else {
      overload->impl = NULL;
   }
}

void
nir_visitor::visit(ir_loop *ir)
{
   exec_list *old_list = this->cf_node_list;

   nir_loop *loop = nir_loop_create(this->shader);
   nir_cf_node_insert_end(old_list, &loop->cf_node);
   this->cf_node_list = &loop->body;
   visit_exec_list(&ir->body_instructions, this);

   this->cf_node_list = old_list;
}

void
nir_visitor::visit(ir_if *ir)
{
   nir_src condition = evaluate_rvalue(ir->condition);

   exec_list *old_list = this->cf_node_list;

   nir_if *if_stmt = nir_if_create(this->shader);
   if_stmt->condition = condition;
   nir_cf_node_insert_end(old_list, &if_stmt->cf_node);

   this->cf_node_list = &if_stmt->then_list;
   visit_exec_list(&ir->then_instructions, this);

   this->cf_node_list = &if_stmt->else_list;
   visit_exec_list(&ir->else_instructions, this);

   this->cf_node_list = old_list;
}

void
nir_visitor::visit(ir_discard *ir)
{
   /*
    * discards aren't treated as control flow, because before we lower them
    * they can appear anywhere in the shader and the stuff after them may still
    * be executed (yay, crazy GLSL rules!). However, after lowering, all the
    * discards will be immediately followed by a return.
    */

   nir_intrinsic_instr *discard =
      nir_intrinsic_instr_create(this->shader, nir_intrinsic_discard);
   nir_instr_insert_after_cf_list(this->cf_node_list, &discard->instr);
}

void
nir_visitor::visit(ir_emit_vertex *ir)
{
   nir_intrinsic_instr *instr =
      nir_intrinsic_instr_create(this->shader, nir_intrinsic_emit_vertex);
   instr->const_index[0] = ir->stream_id();
   nir_instr_insert_after_cf_list(this->cf_node_list, &instr->instr);
}

void
nir_visitor::visit(ir_end_primitive *ir)
{
   nir_intrinsic_instr *instr =
      nir_intrinsic_instr_create(this->shader, nir_intrinsic_end_primitive);
   instr->const_index[0] = ir->stream_id();
   nir_instr_insert_after_cf_list(this->cf_node_list, &instr->instr);
}

void
nir_visitor::visit(ir_loop_jump *ir)
{
   nir_jump_type type;
   switch (ir->mode) {
      case ir_loop_jump::jump_break:
	 type = nir_jump_break;
	 break;
      case ir_loop_jump::jump_continue:
	 type = nir_jump_continue;
	 break;
      default:
	 assert(0);
	 break;
   }

   nir_jump_instr *instr = nir_jump_instr_create(this->shader, type);
   nir_instr_insert_after_cf_list(this->cf_node_list, &instr->instr);
}

void
nir_visitor::visit(ir_return *ir)
{
   if (ir->value != NULL) {
      ir->value->accept(this);
      nir_intrinsic_instr *copy =
	 nir_intrinsic_instr_create(this->shader, nir_intrinsic_copy_var);

      copy->variables[0] = nir_deref_var_create(this->shader,
						this->impl->return_var);
      copy->variables[1] = this->deref_head;
   }

   nir_jump_instr *instr = nir_jump_instr_create(this->shader, nir_jump_return);
   nir_instr_insert_after_cf_list(this->cf_node_list, &instr->instr);
}

void
nir_visitor::visit(ir_call *ir)
{
   if (ir->callee->is_intrinsic) {
      nir_intrinsic_op op;
      if (strcmp(ir->callee_name(), "__intrinsic_atomic_read") == 0) {
	 op = nir_intrinsic_atomic_counter_read_var;
      } else if (strcmp(ir->callee_name(), "__intrinsic_atomic_increment") == 0) {
	 op = nir_intrinsic_atomic_counter_inc_var;
      } else if (strcmp(ir->callee_name(), "__intrinsic_atomic_predecrement") == 0) {
	 op = nir_intrinsic_atomic_counter_dec_var;
      } else {
	 assert(0);
      }

      nir_register *reg = nir_local_reg_create(impl);
      reg->num_components = 1;

      nir_intrinsic_instr *instr = nir_intrinsic_instr_create(shader, op);
      ir_dereference *param =
	 (ir_dereference *) ir->actual_parameters.get_head();
      param->accept(this);
      instr->variables[0] = this->deref_head;
      instr->dest.reg.reg = reg;

      nir_instr_insert_after_cf_list(this->cf_node_list, &instr->instr);

      nir_intrinsic_instr *store_instr =
	 nir_intrinsic_instr_create(shader, nir_intrinsic_store_var_vec1);

      ir->return_deref->accept(this);
      store_instr->variables[0] = this->deref_head;
      store_instr->src[0].reg.reg = reg;

      nir_instr_insert_after_cf_list(this->cf_node_list, &store_instr->instr);

      return;
   }

   struct hash_entry *entry =
      _mesa_hash_table_search(this->overload_table,
			      _mesa_hash_pointer(ir->callee), ir->callee);
   assert(entry);
   nir_function_overload *callee = (nir_function_overload *) entry->data;

   nir_call_instr *instr = nir_call_instr_create(this->shader, callee);

   unsigned i = 0;
   foreach_in_list(ir_dereference, param, &ir->actual_parameters) {
      param->accept(this);
      instr->params[i] = this->deref_head;
      i++;
   }

   ir->return_deref->accept(this);
   instr->return_deref = this->deref_head;
   nir_instr_insert_after_cf_list(this->cf_node_list, &instr->instr);
}

void
nir_visitor::visit(ir_assignment *ir)
{
   if (ir->write_mask != (1 << ir->lhs->type->vector_elements) - 1 &&
       ir->write_mask != 0) {
      /*
       * We have no good way to update only part of a variable, so just load
       * the LHS into a register, do a writemasked move, and then store it
       * back into the LHS. Copy propagation should get rid of the mess.
       */

      ir->lhs->accept(this);
      nir_deref_var *lhs_deref = this->deref_head;
      nir_register *reg = nir_local_reg_create(this->impl);
      reg->num_components = ir->lhs->type->vector_elements;

      nir_intrinsic_op op;
      switch (ir->lhs->type->vector_elements) {
	 case 1: op = nir_intrinsic_load_var_vec1; break;
	 case 2: op = nir_intrinsic_load_var_vec2; break;
	 case 3: op = nir_intrinsic_load_var_vec3; break;
	 case 4: op = nir_intrinsic_load_var_vec4; break;
	 default: assert(0); break;
      }

      nir_intrinsic_instr *load = nir_intrinsic_instr_create(this->shader, op);
      load->dest.reg.reg = reg;
      load->variables[0] = lhs_deref;
      nir_instr_insert_after_cf_list(this->cf_node_list, &load->instr);

      nir_alu_instr *move =
         nir_alu_instr_create(this->shader,
                              supports_ints ? nir_op_fmov : nir_op_imov);
      move->dest.dest.reg.reg = reg;
      move->dest.write_mask = ir->write_mask;
      move->src[0].src = evaluate_rvalue(ir->rhs);

      /*
       * GLSL IR will give us the input to the write-masked assignment in a
       * single packed vector, whereas we expect each input component to be in
       * the same channel as the writemask. So, for example, if the writemask
       * is xzw, then we have to swizzle x -> x, y -> z, and z -> w.
       */

      unsigned component = 0;
      for (unsigned i = 0; i < 4; i++) {
	 if ((ir->write_mask >> i) & 1) {
	    move->src[0].swizzle[i] = component++;
	 } else {
	    move->src[0].swizzle[i] = 0;
	 }
      }

      if (ir->condition != NULL) {
	 move->has_predicate = true;
	 move->predicate = evaluate_rvalue(ir->condition);
      }

      nir_instr_insert_after_cf_list(this->cf_node_list, &move->instr);

      switch (ir->lhs->type->vector_elements) {
	 case 1: op = nir_intrinsic_store_var_vec1; break;
	 case 2: op = nir_intrinsic_store_var_vec2; break;
	 case 3: op = nir_intrinsic_store_var_vec3; break;
	 case 4: op = nir_intrinsic_store_var_vec4; break;
	 default: assert(0); break;
      }

      nir_intrinsic_instr *store = nir_intrinsic_instr_create(this->shader, op);
      nir_deref *store_deref = nir_copy_deref(this->shader, &lhs_deref->deref);
      store->variables[0] = nir_deref_as_var(store_deref);
      store->src[0].reg.reg = reg;
      nir_instr_insert_after_cf_list(this->cf_node_list, &store->instr);
      return;
   }

   if (ir->rhs->as_dereference() || ir->rhs->as_constant()) {
      /* we're copying structs or arrays, so emit a copy_var */
      nir_intrinsic_instr *copy =
	 nir_intrinsic_instr_create(this->shader, nir_intrinsic_copy_var);

      ir->lhs->accept(this);
      copy->variables[0] = this->deref_head;

      ir->rhs->accept(this);
      copy->variables[1] = this->deref_head;

      if (ir->condition != NULL) {
	 copy->has_predicate = true;
	 copy->predicate = evaluate_rvalue(ir->condition);
      }
      nir_instr_insert_after_cf_list(this->cf_node_list, &copy->instr);
      return;
   }

   assert(ir->rhs->type->is_scalar() || ir->rhs->type->is_vector());

   nir_intrinsic_op op;
   switch (ir->lhs->type->vector_elements) {
      case 1: op = nir_intrinsic_store_var_vec1; break;
      case 2: op = nir_intrinsic_store_var_vec2; break;
      case 3: op = nir_intrinsic_store_var_vec3; break;
      case 4: op = nir_intrinsic_store_var_vec4; break;
      default: assert(0); break;
   }

   nir_intrinsic_instr *store = nir_intrinsic_instr_create(this->shader, op);

   ir->lhs->accept(this);
   store->variables[0] = this->deref_head;
   store->src[0] = evaluate_rvalue(ir->rhs);

   if (ir->condition != NULL) {
      store->has_predicate = true;
      store->predicate = evaluate_rvalue(ir->condition);
   }

   nir_instr_insert_after_cf_list(this->cf_node_list, &store->instr);
}

/*
 * Given an instruction, returns a pointer to its destination or NULL if there
 * is no destination.
 *
 * Note that this only handles instructions we generate at this level.
 */
static nir_dest *
get_instr_dest(nir_instr *instr)
{
   nir_alu_instr *alu_instr;
   nir_intrinsic_instr *intrinsic_instr;
   nir_tex_instr *tex_instr;
   nir_load_const_instr *load_const_instr;

   switch (instr->type) {
      case nir_instr_type_alu:
	 alu_instr = nir_instr_as_alu(instr);
	 return &alu_instr->dest.dest;

      case nir_instr_type_intrinsic:
	 intrinsic_instr = nir_instr_as_intrinsic(instr);
	 if (nir_intrinsic_infos[intrinsic_instr->intrinsic].has_dest)
	    return &intrinsic_instr->dest;
	 else
	    return NULL;

      case nir_instr_type_texture:
	 tex_instr = nir_instr_as_texture(instr);
	 return &tex_instr->dest;

      case nir_instr_type_load_const:
	 load_const_instr = nir_instr_as_load_const(instr);
	 return &load_const_instr->dest;

      default:
	 assert(0);
	 break;
   }

   return NULL;
}

void
nir_visitor::add_instr(nir_instr *instr, unsigned num_components)
{
   nir_dest *dest = get_instr_dest(instr);

   dest->reg.reg = nir_local_reg_create(this->impl);
   dest->reg.reg->num_components = num_components;

   nir_instr_insert_after_cf_list(this->cf_node_list, instr);
   this->result = instr;
}

nir_src
nir_visitor::evaluate_rvalue(ir_rvalue* ir)
{
   ir->accept(this);
   if (ir->as_dereference() || ir->as_constant()) {
      /*
       * A dereference is being used on the right hand side, which means we
       * must emit a variable load.
       */

      nir_intrinsic_op op;
      switch (ir->type->vector_elements) {
	 case 1:
	    op = nir_intrinsic_load_var_vec1;
	    break;
	 case 2:
	    op = nir_intrinsic_load_var_vec2;
	    break;
	 case 3:
	    op = nir_intrinsic_load_var_vec3;
	    break;
	 case 4:
	    op = nir_intrinsic_load_var_vec4;
	    break;
      }

      nir_intrinsic_instr *load_instr =
	 nir_intrinsic_instr_create(this->shader, op);
      load_instr->variables[0] = this->deref_head;
      add_instr(&load_instr->instr, ir->type->vector_elements);
   }

   /*
    * instr doesn't have a destination right now, give it one and then set up
    * the source so that it points to it.
    *
    * TODO: once we support SSA plumb through a use_ssa boolean and use SSA
    * here instead of creating a register.
    */
   nir_dest *dest = get_instr_dest(this->result);
   assert(dest->reg.reg);
   nir_src src;

   src.is_ssa = false;
   src.reg.base_offset = 0;
   src.reg.indirect = NULL;
   src.reg.reg = dest->reg.reg;

   return src;
}

nir_alu_instr *
nir_visitor::emit(nir_op op, unsigned dest_size, nir_src *srcs)
{
   nir_alu_instr *instr = nir_alu_instr_create(this->shader, op);
   for (unsigned i = 0; i < nir_op_infos[op].num_inputs; i++)
      instr->src[i].src = srcs[i];
   instr->dest.write_mask = (1 << dest_size) - 1;
   add_instr(&instr->instr, dest_size);
   return instr;
}

nir_alu_instr *
nir_visitor::emit(nir_op op, unsigned dest_size, nir_src src1)
{
   assert(nir_op_infos[op].num_inputs == 1);
   return emit(op, dest_size, &src1);
}

nir_alu_instr *
nir_visitor::emit(nir_op op, unsigned dest_size, nir_src src1,
		  nir_src src2)
{
   assert(nir_op_infos[op].num_inputs == 2);
   nir_src srcs[] = { src1, src2 };
   return emit(op, dest_size, srcs);
}

nir_alu_instr *
nir_visitor::emit(nir_op op, unsigned dest_size, nir_src src1,
		  nir_src src2, nir_src src3)
{
   assert(nir_op_infos[op].num_inputs == 3);
   nir_src srcs[] = { src1, src2, src3 };
   return emit(op, dest_size, srcs);
}

void
nir_visitor::visit(ir_expression *ir)
{
   if (ir->operation == ir_binop_ubo_load) {
      ir_constant *const_index = ir->operands[1]->as_constant();
      
      nir_intrinsic_op op;
      if (const_index) {
         switch (ir->type->vector_elements) {
            case 1: op = nir_intrinsic_load_ubo_vec1; break;
            case 2: op = nir_intrinsic_load_ubo_vec2; break;
            case 3: op = nir_intrinsic_load_ubo_vec3; break;
            case 4: op = nir_intrinsic_load_ubo_vec4; break;
            default: assert(0); break;
         }
      } else {
         switch (ir->type->vector_elements) {
            case 1: op = nir_intrinsic_load_ubo_vec1_indirect; break;
            case 2: op = nir_intrinsic_load_ubo_vec2_indirect; break;
            case 3: op = nir_intrinsic_load_ubo_vec3_indirect; break;
            case 4: op = nir_intrinsic_load_ubo_vec4_indirect; break;
            default: assert(0); break;
         }
      }
      nir_intrinsic_instr *load = nir_intrinsic_instr_create(this->shader, op);
      load->const_index[0] = ir->operands[0]->as_constant()->value.u[0];
      load->const_index[1] = const_index ? const_index->value.u[0] : 0; /* base offset */
      load->const_index[2] = 1; /* number of vec4's */
      if (!const_index)
         load->src[0] = evaluate_rvalue(ir->operands[1]);
      add_instr(&load->instr, ir->type->vector_elements);
      
      /*
       * In UBO's, a true boolean value is any non-zero value, but we consider
       * a true boolean to be ~0. Fix this up with a != 0 comparison.
       */
      
      if (ir->type->base_type == GLSL_TYPE_BOOL) {
         nir_load_const_instr *const_zero = nir_load_const_instr_create(shader);
         const_zero->num_components = 1;
         const_zero->value.u[0] = 0;
         const_zero->dest.reg.reg = nir_local_reg_create(this->impl);
         const_zero->dest.reg.reg->num_components = 1;
         nir_instr_insert_after_cf_list(this->cf_node_list, &const_zero->instr);
         
         nir_alu_instr *compare = nir_alu_instr_create(shader, nir_op_ine);
         compare->src[0].src.reg.reg = load->dest.reg.reg;
         compare->src[1].src.reg.reg = const_zero->dest.reg.reg;
         for (unsigned i = 0; i < ir->type->vector_elements; i++)
            compare->src[1].swizzle[i] = 0;
         compare->dest.write_mask = (1 << ir->type->vector_elements) - 1;
         
         add_instr(&compare->instr, ir->type->vector_elements);
      }
      
      return;
   }

   nir_src srcs[4];
   for (unsigned i = 0; i < ir->get_num_operands(); i++)
      srcs[i] = evaluate_rvalue(ir->operands[i]);

   glsl_base_type types[4];
   for (unsigned i = 0; i < ir->get_num_operands(); i++)
      if (supports_ints)
	 types[i] = ir->operands[i]->type->base_type;
      else
	 types[i] = GLSL_TYPE_FLOAT;

   glsl_base_type out_type;
   if (supports_ints)
      out_type = ir->type->base_type;
   else
      out_type = GLSL_TYPE_FLOAT;

   unsigned dest_size = ir->type->vector_elements;

   nir_alu_instr *instr;
   nir_op op;

   switch (ir->operation) {
      case ir_unop_bit_not: emit(nir_op_inot, dest_size, srcs); break;
      case ir_unop_logic_not:
	 emit(supports_ints ? nir_op_inot : nir_op_fnot, dest_size, srcs);
	 break;
      case ir_unop_neg:
	 instr = emit(types[0] == GLSL_TYPE_FLOAT ? nir_op_fmov : nir_op_imov,
		      dest_size, srcs);
	 instr->src[0].negate = true;
	 break;
      case ir_unop_abs:
	 instr = emit(types[0] == GLSL_TYPE_FLOAT ? nir_op_fmov : nir_op_imov,
                      dest_size, srcs);
         instr->src[0].abs = true;
	 break;
      case ir_unop_sign:
	 emit(types[0] == GLSL_TYPE_FLOAT ? nir_op_fsign : nir_op_isign,
	      dest_size, srcs);
	 break;
      case ir_unop_rcp:  emit(nir_op_frcp, dest_size, srcs);  break;
      case ir_unop_rsq:  emit(nir_op_frsq, dest_size, srcs);  break;
      case ir_unop_sqrt: emit(nir_op_fsqrt, dest_size, srcs); break;
      case ir_unop_exp:  emit(nir_op_fexp, dest_size, srcs);  break;
      case ir_unop_log:  emit(nir_op_flog, dest_size, srcs);  break;
      case ir_unop_exp2: emit(nir_op_fexp2, dest_size, srcs); break;
      case ir_unop_log2: emit(nir_op_flog2, dest_size, srcs); break;
      case ir_unop_i2f:
	 emit(supports_ints ? nir_op_i2f : nir_op_fmov, dest_size, srcs);
	 break;
      case ir_unop_u2f:
	 emit(supports_ints ? nir_op_u2f : nir_op_fmov, dest_size, srcs);
	 break;
      case ir_unop_b2f:
	 emit(supports_ints ? nir_op_b2f : nir_op_fmov, dest_size, srcs);
	 break;
      case ir_unop_f2i:  emit(nir_op_f2i, dest_size, srcs);   break;
      case ir_unop_f2u:  emit(nir_op_f2u, dest_size, srcs);   break;
      case ir_unop_f2b:  emit(nir_op_f2b, dest_size, srcs);   break;
      case ir_unop_i2b:  emit(nir_op_i2b, dest_size, srcs);   break;
      case ir_unop_b2i:  emit(nir_op_b2i, dest_size, srcs);   break;
      case ir_unop_i2u:
      case ir_unop_u2i:
      case ir_unop_bitcast_i2f:
      case ir_unop_bitcast_f2i:
      case ir_unop_bitcast_u2f:
      case ir_unop_bitcast_f2u:
	 /* no-op */
	 emit(nir_op_imov, dest_size, srcs);
	 break;
      case ir_unop_any:
	 switch (ir->operands[0]->type->vector_elements) {
	    case 2:
	       emit(supports_ints ? nir_op_bany2 : nir_op_fany2,
		    dest_size, srcs);
	       break;
	    case 3:
	       emit(supports_ints ? nir_op_bany3 : nir_op_fany3,
		    dest_size, srcs);
	       break;
	    case 4:
	       emit(supports_ints ? nir_op_bany4 : nir_op_fany4,
		    dest_size, srcs);
	       break;
	    default:
	       assert(0);
	       break;
	 }
	 break;
      case ir_unop_trunc: emit(nir_op_ftrunc, dest_size, srcs); break;
      case ir_unop_ceil:  emit(nir_op_fceil,  dest_size, srcs); break;
      case ir_unop_floor: emit(nir_op_ffloor, dest_size, srcs); break;
      case ir_unop_fract: emit(nir_op_ffract, dest_size, srcs); break;
      case ir_unop_round_even: emit(nir_op_fround_even, dest_size, srcs); break;
      case ir_unop_sin:   emit(nir_op_fsin,   dest_size, srcs); break;
      case ir_unop_cos:   emit(nir_op_fcos,   dest_size, srcs); break;
      case ir_unop_sin_reduced:
	 emit(nir_op_fsin_reduced, dest_size, srcs);
	 break;
      case ir_unop_cos_reduced:
	 emit(nir_op_fcos_reduced, dest_size, srcs);
	 break;
      case ir_unop_dFdx:  emit(nir_op_fddx,   dest_size, srcs); break;
      case ir_unop_dFdy:  emit(nir_op_fddy,   dest_size, srcs); break;
      case ir_unop_pack_snorm_2x16:
	 emit(nir_op_pack_snorm_2x16, dest_size, srcs);
	 break;
      case ir_unop_pack_snorm_4x8:
	 emit(nir_op_pack_snorm_4x8, dest_size, srcs);
	 break;
      case ir_unop_pack_unorm_2x16:
	 emit(nir_op_pack_unorm_2x16, dest_size, srcs);
	 break;
      case ir_unop_pack_unorm_4x8:
	 emit(nir_op_pack_unorm_4x8, dest_size, srcs);
	 break;
      case ir_unop_pack_half_2x16:
	 emit(nir_op_pack_half_2x16, dest_size, srcs);
	 break;
      case ir_unop_unpack_snorm_2x16:
	 emit(nir_op_unpack_snorm_2x16, dest_size, srcs);
	 break;
      case ir_unop_unpack_snorm_4x8:
	 emit(nir_op_unpack_snorm_4x8, dest_size, srcs);
	 break;
      case ir_unop_unpack_unorm_2x16:
	 emit(nir_op_unpack_unorm_2x16, dest_size, srcs);
	 break;
      case ir_unop_unpack_unorm_4x8:
	 emit(nir_op_unpack_unorm_4x8, dest_size, srcs);
	 break;
      case ir_unop_unpack_half_2x16:
	 emit(nir_op_unpack_half_2x16, dest_size, srcs);
	 break;
      case ir_unop_unpack_half_2x16_split_x:
	 emit(nir_op_unpack_half_2x16_split_x, dest_size, srcs);
	 break;
      case ir_unop_unpack_half_2x16_split_y:
	 emit(nir_op_unpack_half_2x16_split_y, dest_size, srcs);
	 break;
      case ir_unop_bitfield_reverse:
	 emit(nir_op_bitfield_reverse, dest_size, srcs);
	 break;
      case ir_unop_bit_count: emit(nir_op_bit_count, dest_size, srcs); break;
      case ir_unop_find_msb:  emit(nir_op_find_msb,  dest_size, srcs); break;
      case ir_unop_find_lsb:  emit(nir_op_find_lsb,  dest_size, srcs); break;
      case ir_unop_noise:
	 switch (ir->type->vector_elements) {
	    case 1:
	       switch (ir->operands[0]->type->vector_elements) {
		  case 1: emit(nir_op_fnoise1_1, dest_size, srcs); break;
		  case 2: emit(nir_op_fnoise1_2, dest_size, srcs); break;
		  case 3: emit(nir_op_fnoise1_3, dest_size, srcs); break;
		  case 4: emit(nir_op_fnoise1_4, dest_size, srcs); break;
		  default: assert(0); break;
	       }
	       break;
	    case 2:
	       switch (ir->operands[0]->type->vector_elements) {
		  case 1: emit(nir_op_fnoise2_1, dest_size, srcs); break;
		  case 2: emit(nir_op_fnoise2_2, dest_size, srcs); break;
		  case 3: emit(nir_op_fnoise2_3, dest_size, srcs); break;
		  case 4: emit(nir_op_fnoise2_4, dest_size, srcs); break;
		  default: assert(0); break;
	       }
	       break;
	    case 3:
	       switch (ir->operands[0]->type->vector_elements) {
		  case 1: emit(nir_op_fnoise3_1, dest_size, srcs); break;
		  case 2: emit(nir_op_fnoise3_2, dest_size, srcs); break;
		  case 3: emit(nir_op_fnoise3_3, dest_size, srcs); break;
		  case 4: emit(nir_op_fnoise3_4, dest_size, srcs); break;
		  default: assert(0); break;
	       }
	       break;
	    case 4:
	       switch (ir->operands[0]->type->vector_elements) {
		  case 1: emit(nir_op_fnoise4_1, dest_size, srcs); break;
		  case 2: emit(nir_op_fnoise4_2, dest_size, srcs); break;
		  case 3: emit(nir_op_fnoise4_3, dest_size, srcs); break;
		  case 4: emit(nir_op_fnoise4_4, dest_size, srcs); break;
		  default: assert(0); break;
	       }
	       break;
	    default:
	       assert(0);
	       break;
	 }
	 break;
      case ir_binop_add:
      case ir_binop_sub:
      case ir_binop_mul:
      case ir_binop_div:
      case ir_binop_mod:
      case ir_binop_min:
      case ir_binop_max:
      case ir_binop_pow:
      case ir_binop_bit_and:
      case ir_binop_bit_or:
      case ir_binop_bit_xor:
      case ir_binop_lshift:
      case ir_binop_rshift:
	 switch (ir->operation) {
	    case ir_binop_add:
	       if (out_type == GLSL_TYPE_FLOAT)
		  op = nir_op_fadd;
	       else
		  op = nir_op_iadd;
	       break;
	    case ir_binop_sub:
	       if (out_type == GLSL_TYPE_FLOAT)
		  op = nir_op_fsub;
	       else
		  op = nir_op_isub;
	       break;
	    case ir_binop_mul:
	       if (out_type == GLSL_TYPE_FLOAT)
		  op = nir_op_fmul;
	       else
		  op = nir_op_imul;
	       break;
	    case ir_binop_div:
	       if (out_type == GLSL_TYPE_FLOAT)
		  op = nir_op_fdiv;
	       else if (out_type == GLSL_TYPE_INT)
		  op = nir_op_idiv;
	       else
		  op = nir_op_udiv;
	       break;
	    case ir_binop_mod:
	       if (out_type == GLSL_TYPE_FLOAT)
		  op = nir_op_fmod;
	       else
		  op = nir_op_umod;
	       break;
	    case ir_binop_min:
	       if (out_type == GLSL_TYPE_FLOAT)
		  op = nir_op_fmin;
	       else if (out_type == GLSL_TYPE_INT)
		  op = nir_op_imin;
	       else
		  op = nir_op_umin;
	       break;
	    case ir_binop_max:
	       if (out_type == GLSL_TYPE_FLOAT)
		  op = nir_op_fmax;
	       else if (out_type == GLSL_TYPE_INT)
		  op = nir_op_imax;
	       else
		  op = nir_op_umax;
	       break;
	    case ir_binop_bit_and:
	       op = nir_op_iand;
	       break;
	    case ir_binop_bit_or:
	       op = nir_op_ior;
	       break;
	    case ir_binop_bit_xor:
	       op = nir_op_ixor;
	       break;
	    case ir_binop_lshift:
	       op = nir_op_ishl;
	       break;
	    case ir_binop_rshift:
	       if (out_type == GLSL_TYPE_INT)
		  op = nir_op_ishr;
	       else
		  op = nir_op_ushr;
	       break;
	    case ir_binop_pow:
	       op = nir_op_fpow;
	       break;

	    default:
	       assert(0);
	       break;
	 }

	 instr = emit(op, dest_size, srcs);

	 if (ir->operands[0]->type->vector_elements != 1 &&
	     ir->operands[1]->type->vector_elements == 1) {
	    for (unsigned i = 0; i < ir->operands[0]->type->vector_elements;
		 i++) {
	       instr->src[1].swizzle[i] = 0;
	    }
	 }

	 if (ir->operands[1]->type->vector_elements != 1 &&
	     ir->operands[0]->type->vector_elements == 1) {
	    for (unsigned i = 0; i < ir->operands[1]->type->vector_elements;
		 i++) {
	       instr->src[0].swizzle[i] = 0;
	    }
	 }

	 break;
      case ir_binop_imul_high:
	 emit(out_type == GLSL_TYPE_UINT ? nir_op_umul_high : nir_op_imul_high,
	      dest_size, srcs);
	 break;
      case ir_binop_carry:  emit(nir_op_uadd_carry, dest_size, srcs);  break;
      case ir_binop_borrow: emit(nir_op_usub_borrow, dest_size, srcs); break;
      case ir_binop_less:
	 if (supports_ints) {
	    if (types[0] == GLSL_TYPE_FLOAT)
	       emit(nir_op_flt, dest_size, srcs);
	    else if (types[0] == GLSL_TYPE_INT)
	       emit(nir_op_ilt, dest_size, srcs);
	    else
	       emit(nir_op_ult, dest_size, srcs);
	 } else {
	    emit(nir_op_slt, dest_size, srcs);
	 }
	 break;
      case ir_binop_greater:
	 if (supports_ints) {
	    if (types[0] == GLSL_TYPE_FLOAT)
	       emit(nir_op_flt, dest_size, srcs[1], srcs[0]);
	    else if (types[0] == GLSL_TYPE_INT)
	       emit(nir_op_ilt, dest_size, srcs[1], srcs[0]);
	    else
	       emit(nir_op_ult, dest_size, srcs[1], srcs[0]);
	 } else {
	    emit(nir_op_slt, dest_size, srcs[1], srcs[0]);
	 }
	 break;
      case ir_binop_lequal:
	 if (supports_ints) {
	    if (types[0] == GLSL_TYPE_FLOAT)
	       emit(nir_op_fge, dest_size, srcs[1], srcs[0]);
	    else if (types[0] == GLSL_TYPE_INT)
	       emit(nir_op_ige, dest_size, srcs[1], srcs[0]);
	    else
	       emit(nir_op_uge, dest_size, srcs[1], srcs[0]);
	 } else {
	    emit(nir_op_slt, dest_size, srcs[1], srcs[0]);
	 }
	 break;
      case ir_binop_gequal:
	 if (supports_ints) {
	    if (types[0] == GLSL_TYPE_FLOAT)
	       emit(nir_op_fge, dest_size, srcs);
	    else if (types[0] == GLSL_TYPE_INT)
	       emit(nir_op_ige, dest_size, srcs);
	    else
	       emit(nir_op_uge, dest_size, srcs);
	 } else {
	    emit(nir_op_slt, dest_size, srcs);
	 }
	 break;
      case ir_binop_equal:
	 if (supports_ints) {
	    if (types[0] == GLSL_TYPE_FLOAT)
	       emit(nir_op_feq, dest_size, srcs);
	    else
	       emit(nir_op_ieq, dest_size, srcs);
	 } else {
	    emit(nir_op_seq, dest_size, srcs);
	 }
	 break;
      case ir_binop_nequal:
	 if (supports_ints) {
	    if (types[0] == GLSL_TYPE_FLOAT)
	       emit(nir_op_fne, dest_size, srcs);
	    else
	       emit(nir_op_ine, dest_size, srcs);
	 } else {
	    emit(nir_op_sne, dest_size, srcs);
	 }
	 break;
      case ir_binop_all_equal:
	 if (supports_ints) {
	    if (types[0] == GLSL_TYPE_FLOAT) {
	       switch (ir->operands[0]->type->vector_elements) {
		  case 1: emit(nir_op_feq, dest_size, srcs); break;
		  case 2: emit(nir_op_ball_fequal2, dest_size, srcs); break;
		  case 3: emit(nir_op_ball_fequal3, dest_size, srcs); break;
		  case 4: emit(nir_op_ball_fequal4, dest_size, srcs); break;
		  default:
		     assert(0);
		     break;
	       }
	    } else {
	       switch (ir->operands[0]->type->vector_elements) {
		  case 1: emit(nir_op_ieq, dest_size, srcs); break;
		  case 2: emit(nir_op_ball_iequal2, dest_size, srcs); break;
		  case 3: emit(nir_op_ball_iequal3, dest_size, srcs); break;
		  case 4: emit(nir_op_ball_iequal4, dest_size, srcs); break;
		  default:
		     assert(0);
		     break;
	       }
	    }
	 } else {
	    switch (ir->operands[0]->type->vector_elements) {
	       case 1: emit(nir_op_seq, dest_size, srcs); break;
	       case 2: emit(nir_op_fall_equal2, dest_size, srcs); break;
	       case 3: emit(nir_op_fall_equal3, dest_size, srcs); break;
	       case 4: emit(nir_op_fall_equal4, dest_size, srcs); break;
	       default:
		  assert(0);
		  break;
	    }
	 }
	 break;
      case ir_binop_any_nequal:
	 if (supports_ints) {
	    if (types[0] == GLSL_TYPE_FLOAT) {
	       switch (ir->operands[0]->type->vector_elements) {
		  case 1: emit(nir_op_fne, dest_size, srcs); break;
		  case 2: emit(nir_op_bany_fnequal2, dest_size, srcs); break;
		  case 3: emit(nir_op_bany_fnequal3, dest_size, srcs); break;
		  case 4: emit(nir_op_bany_fnequal4, dest_size, srcs); break;
		  default:
		     assert(0);
		     break;
	       }
	    } else {
	       switch (ir->operands[0]->type->vector_elements) {
		  case 1: emit(nir_op_ine, dest_size, srcs); break;
		  case 2: emit(nir_op_bany_inequal2, dest_size, srcs); break;
		  case 3: emit(nir_op_bany_inequal3, dest_size, srcs); break;
		  case 4: emit(nir_op_bany_inequal4, dest_size, srcs); break;
		  default:
		     assert(0);
		     break;
	       }
	    }
	 } else {
	    switch (ir->operands[0]->type->vector_elements) {
	       case 1: emit(nir_op_sne, dest_size, srcs); break;
	       case 2: emit(nir_op_fany_nequal2, dest_size, srcs); break;
	       case 3: emit(nir_op_fany_nequal3, dest_size, srcs); break;
	       case 4: emit(nir_op_fany_nequal4, dest_size, srcs); break;
	       default:
		  assert(0);
		  break;
	    }
	 }
	 break;
      case ir_binop_logic_and:
	 if (supports_ints)
	    emit(nir_op_iand, dest_size, srcs);
	 else
	    emit(nir_op_fand, dest_size, srcs);
	 break;
      case ir_binop_logic_or:
	 if (supports_ints)
	    emit(nir_op_ior, dest_size, srcs);
	 else
	    emit(nir_op_for, dest_size, srcs);
	 break;
      case ir_binop_logic_xor:
	 if (supports_ints)
	    emit(nir_op_ixor, dest_size, srcs);
	 else
	    emit(nir_op_fxor, dest_size, srcs);
	 break;
      case ir_binop_dot:
	 switch (ir->operands[0]->type->vector_elements) {
	    case 2: emit(nir_op_fdot2, dest_size, srcs); break;
	    case 3: emit(nir_op_fdot3, dest_size, srcs); break;
	    case 4: emit(nir_op_fdot4, dest_size, srcs); break;
	    default:
	       assert(0);
	       break;
	 }
	 break;

      case ir_binop_pack_half_2x16_split:
	    emit(nir_op_pack_half_2x16_split, dest_size, srcs);
	    break;
      case ir_binop_bfm:   emit(nir_op_bfm, dest_size, srcs);   break;
      case ir_binop_ldexp: emit(nir_op_ldexp, dest_size, srcs); break;
      case ir_triop_fma:   emit(nir_op_ffma, dest_size, srcs);  break;
      case ir_triop_lrp:
	 instr = emit(nir_op_flrp, dest_size, srcs);
	 if (ir->operands[0]->type->vector_elements != 1 &&
	     ir->operands[2]->type->vector_elements == 1) {
	    for (unsigned i = 0; i < ir->operands[0]->type->vector_elements;
		 i++) {
	       instr->src[2].swizzle[i] = 0;
	    }
	 }
	 break;
      case ir_triop_csel:
	 if (supports_ints)
	    emit(nir_op_bcsel, dest_size, srcs);
	 else
	    emit(nir_op_fcsel, dest_size, srcs);
	 break;
      case ir_triop_bfi:
	 instr = emit(nir_op_bfi, dest_size, srcs);
	 for (unsigned i = 0; i < ir->operands[1]->type->vector_elements; i++) {
	    instr->src[0].swizzle[i] = 0;
	 }
	 break;
      case ir_triop_bitfield_extract:
	 instr = emit(out_type == GLSL_TYPE_INT ? nir_op_ibitfield_extract :
		      nir_op_ubitfield_extract, dest_size, srcs);
	 for (unsigned i = 0; i < ir->operands[0]->type->vector_elements; i++) {
	    instr->src[1].swizzle[i] = 0;
	    instr->src[2].swizzle[i] = 0;
	 }
	 break;
      case ir_quadop_bitfield_insert:
	 instr = emit(nir_op_bitfield_insert, dest_size, srcs);
	 for (unsigned i = 0; i < ir->operands[0]->type->vector_elements; i++) {
	    instr->src[2].swizzle[i] = 0;
	    instr->src[3].swizzle[i] = 0;
	 }
	 break;
      case ir_quadop_vector:
	 switch (ir->type->vector_elements) {
	    case 2: emit(nir_op_vec2, dest_size, srcs); break;
	    case 3: emit(nir_op_vec3, dest_size, srcs); break;
	    case 4: emit(nir_op_vec4, dest_size, srcs); break;
	    default: assert(0); break;
	 }
	 break;

      default:
	 assert(0);
	 break;
   }
}

void
nir_visitor::visit(ir_swizzle *ir)
{
   nir_alu_instr *instr = emit(supports_ints ? nir_op_imov : nir_op_fmov,
                               ir->type->vector_elements,
                               evaluate_rvalue(ir->val));

   unsigned swizzle[4] = { ir->mask.x, ir->mask.y, ir->mask.z, ir->mask.w };
   for (unsigned i = 0; i < ir->type->vector_elements; i++)
      instr->src[0].swizzle[i] = swizzle[i];
}

void
nir_visitor::visit(ir_texture *ir)
{
   unsigned num_srcs;
   nir_texop op;
   switch (ir->op) {
      case ir_tex:
	 op = nir_texop_tex;
	 num_srcs = 1; /* coordinate */
	 break;

      case ir_txb:
      case ir_txl:
	 op = (ir->op == ir_txb) ? nir_texop_txb : nir_texop_txl;
	 num_srcs = 2; /* coordinate, bias/lod */
	 break;

      case ir_txd:
	 op = nir_texop_txd; /* coordinate, dPdx, dPdy */
	 num_srcs = 3;
	 break;

      case ir_txf:
	 op = nir_texop_txf;
	 if (ir->lod_info.lod != NULL)
	    num_srcs = 2; /* coordinate, lod */
	 else
	    num_srcs = 1; /* coordinate */
	 break;

      case ir_txf_ms:
	 op = nir_texop_txf_ms;
	 num_srcs = 2; /* coordinate, sample_index */
	 break;

      case ir_txs:
	 op = nir_texop_txs;
	 if (ir->lod_info.lod != NULL)
	    num_srcs = 1; /* lod */
	 else
	    num_srcs = 0;
	 break;

      case ir_lod:
	 op = nir_texop_lod;
	 num_srcs = 1; /* coordinate */
	 break;

      case ir_tg4:
	 op = nir_texop_tg4;
	 num_srcs = 1; /* coordinate */
	 break;

      case ir_query_levels:
	 op = nir_texop_query_levels;
	 num_srcs = 0;
	 break;

      default:
	 assert(0);
	 break;
   }

   if (ir->projector != NULL)
      num_srcs++;
   if (ir->shadow_comparitor != NULL)
      num_srcs++;
   if (ir->offset != NULL && ir->offset->as_constant() == NULL)
      num_srcs++;

   nir_tex_instr *instr = nir_tex_instr_create(this->shader, num_srcs);

   instr->op = op;
   instr->sampler_dim =
      (glsl_sampler_dim) ir->sampler->type->sampler_dimensionality;
   instr->is_array = ir->sampler->type->sampler_array;
   instr->is_shadow = ir->sampler->type->sampler_shadow;
   if (instr->is_shadow)
      instr->is_new_style_shadow = (ir->type->vector_elements == 1);
   switch (ir->type->base_type) {
      case GLSL_TYPE_FLOAT:
         instr->dest_type = nir_type_float;
         break;
      case GLSL_TYPE_INT:
         instr->dest_type = nir_type_int;
         break;
      case GLSL_TYPE_UINT:
         instr->dest_type = nir_type_unsigned;
         break;
      default:
         assert(0);
   }

   ir->sampler->accept(this);
   instr->sampler = this->deref_head;

   unsigned src_number = 0;

   if (ir->coordinate != NULL) {
      instr->coord_components = ir->coordinate->type->vector_elements;
      instr->src[src_number] = evaluate_rvalue(ir->coordinate);
      instr->src_type[src_number] = nir_tex_src_coord;
      src_number++;
   }

   if (ir->projector != NULL) {
      instr->src[src_number] = evaluate_rvalue(ir->projector);
      instr->src_type[src_number] = nir_tex_src_projector;
      src_number++;
   }

   if (ir->shadow_comparitor != NULL) {
      instr->src[src_number] = evaluate_rvalue(ir->shadow_comparitor);
      instr->src_type[src_number] = nir_tex_src_comparitor;
      src_number++;
   }

   if (ir->offset != NULL) {
      /* we don't support multiple offsets yet */
      assert(ir->offset->type->is_vector() || ir->offset->type->is_scalar());

      ir_constant *const_offset = ir->offset->as_constant();
      if (const_offset != NULL) {
	 for (unsigned i = 0; i < const_offset->type->vector_elements; i++)
	    instr->const_offset[i] = const_offset->value.i[i];
      } else {
	 instr->src[src_number] = evaluate_rvalue(ir->offset);
	 instr->src_type[src_number] = nir_tex_src_offset;
	 src_number++;
      }
   }

   switch (ir->op) {
      case ir_txb:
	 instr->src[src_number] = evaluate_rvalue(ir->lod_info.bias);
	 instr->src_type[src_number] = nir_tex_src_bias;
	 src_number++;
	 break;

      case ir_txl:
      case ir_txf:
      case ir_txs:
	 if (ir->lod_info.lod != NULL) {
	    instr->src[src_number] = evaluate_rvalue(ir->lod_info.lod);
	    instr->src_type[src_number] = nir_tex_src_lod;
	    src_number++;
	 }
	 break;

      case ir_txd:
	 instr->src[src_number] = evaluate_rvalue(ir->lod_info.grad.dPdx);
	 instr->src_type[src_number] = nir_tex_src_ddx;
	 src_number++;
	 instr->src[src_number] = evaluate_rvalue(ir->lod_info.grad.dPdy);
	 instr->src_type[src_number] = nir_tex_src_ddy;
	 src_number++;
	 break;

      case ir_txf_ms:
	 instr->src[src_number] = evaluate_rvalue(ir->lod_info.sample_index);
	 instr->src_type[src_number] = nir_tex_src_ms_index;
	 src_number++;
	 break;

      case ir_tg4:
	 instr->component = ir->lod_info.component->as_constant()->value.u[0];
	 break;

      default:
	 break;
   }

   assert(src_number == num_srcs);

   add_instr(&instr->instr, nir_tex_instr_dest_size(instr));
}

void
nir_visitor::visit(ir_constant *ir)
{
   /*
    * We don't know if this variable is an an array or struct that gets
    * dereferenced, so do the safe thing an make it a variable and return a
    * dereference.
    */

   nir_variable *var = ralloc(this->shader, nir_variable);
   var->name = ralloc_strdup(var, "const_temp");
   var->type = ir->type;
   var->data.mode = nir_var_local;
   var->data.read_only = true;
   var->constant_value = constant_copy(ir, var);
   var->constant_initializer = constant_copy(ir, var);
   exec_list_push_tail(&this->impl->locals, &var->node);

   this->deref_head = nir_deref_var_create(this->shader, var);
   this->deref_tail = &this->deref_head->deref;
}

void
nir_visitor::visit(ir_dereference_variable *ir)
{
   struct hash_entry *entry =
      _mesa_hash_table_search(this->var_table, _mesa_hash_pointer(ir->var),
			      ir->var);
   assert(entry);
   nir_variable *var = (nir_variable *) entry->data;

   nir_deref_var *deref = nir_deref_var_create(this->shader, var);
   this->deref_head = deref;
   this->deref_tail = &deref->deref;
}

void
nir_visitor::visit(ir_dereference_record *ir)
{
   ir->record->accept(this);

   nir_deref_struct *deref = nir_deref_struct_create(this->shader, ir->field);
   deref->deref.type = ir->type;
   this->deref_tail->child = &deref->deref;
   this->deref_tail = &deref->deref;
}

void
nir_visitor::visit(ir_dereference_array *ir)
{
   nir_deref_array *deref = nir_deref_array_create(this->shader);
   deref->deref.type = ir->type;

   ir_constant *const_index = ir->array_index->as_constant();
   if (const_index != NULL) {
      deref->base_offset = const_index->value.u[0];
   } else {
      deref->has_indirect = true;
      deref->indirect = evaluate_rvalue(ir->array_index);
   }

   ir->array->accept(this);

   this->deref_tail->child = &deref->deref;
   this->deref_tail = &deref->deref;
}
