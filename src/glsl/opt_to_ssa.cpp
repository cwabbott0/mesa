/*
 * Copyright © 2013 Connor Abbott (connor@abbott.cx)
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
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include "ir.h"
#include "ir_optimization.h"
#include "ir_hierarchical_visitor.h"
#include "ir_dead_branches.h"
#include "ir_loop_jumps.h"
#include "ir_builder.h"
#include "ralloc.h"
#include "glsl_types.h"
#include "main/hash_table.h"

/**
 * \file opt_to_ssa.cpp
 *
 * This pass will convert all temporaries and local variables to SSA
 * temporaries, except for variables which are derefenced as an array or
 * structure (which we cannot support in SSA form). The algorithm is loosely
 * based on "Efficiently Computing Static Single Assignment Form and the
 * Control Dependence Graph" by Cytron et. al., although there are a number of
 * differences caused by the fact that we are operating on a hierachical tree
 * of if's and loops instead of the graph of basic blocks that Cytron et. al.
 * assume. In particular, instead of explicitly constructing the dominance tree,
 * we use an approximation simple enough that all the information we need can
 * be found on the fly. The approximation we use is this:
 *
 * - The instruction before an if statement dominates the then and else branches
 * as well as the instructions after the branch, unless one of the branches is
 * dead. If, for example, the then branch is dead, then the instruction before
 * the if statement dominates the then branch and the else branch, and the else
 * branch dominates the instruction after the if statement because if we get
 * past the branch then we know we must have gone through the else branch.
 *
 * - The instruction before the loop dominates the instructions inside the loop
 * as well as the instructions after the loop. Here is where the approximation
 * lies: really, since the loop is guarenteed to execute at least once, the
 * instructions after the loop can potentially be dominated by an instruction
 * inside the loop. Computing that instruction, though, would be complicated,
 * and in the end it doesn't hurt much if we ignore that detail. In the end, we
 * may have some phi nodes where all the sources are the same, but these can
 * easily be optimized away.
 *
 * The iterated dominance frontier of an instruction can then be calculated by
 * walking up the stack of control flow elements (if's and loops) that lead to
 * the instruction. If the instruction must lead to a continue or break
 * statement, then we skip up the stack first until we find the loop that it's
 * breaking out of or continuing into. When inserting phi nodes we can
 * simply walk up the stack as described, inserting phi nodes into the join
 * nodes of the if statements and loops that we encounter.
 */

using namespace ir_builder;

namespace {

class ir_ssa_state_visitor;

class ir_ssa_variable_state
{
public:
   ir_ssa_variable_state(ir_variable *var, ir_ssa_state_visitor *v,
			 ir_variable *undefined_var);
   ~ir_ssa_variable_state();

   ir_variable *var; /* the original variable. */

   void stack_push(ir_variable *new_var);

   void stack_pop();

   ir_variable *cur_var(bool);

   ir_variable *new_var();

   ir_variable **stack; /* The stack of replacements for the variable. */
   int num_replaced;
   int num_defs; /* the number of times the variable is assigned */
   int stack_idx; /* the current index into the stack */

   ir_ssa_state_visitor *v;

   ir_variable *undefined_var; /** < used for when var is read before written. */
};

class ir_if_entry;
class ir_loop_entry;

/*
 * sets up a hash table of ir_ssa_variable_state for the main phase of the
 * algorithm
 */

class ir_ssa_state_visitor : public ir_hierarchical_visitor {
public:
   ir_ssa_state_visitor();
   ~ir_ssa_state_visitor(void);

   virtual ir_visitor_status visit(ir_variable *);
   virtual ir_visitor_status visit_enter(ir_dereference_record *);
   virtual ir_visitor_status visit_enter(ir_dereference_array *);
   virtual ir_visitor_status visit(ir_dereference_variable *);

   /**
    * Get the ir_ssa_variable_state corresponding to the original (non-SSA)
    * variable
    */

   ir_ssa_variable_state *get_state(const ir_variable *var);

   /**
    * Get the ir_ssa_variable_state corresponding to the new (SSA)
    * variable
    */

   ir_ssa_variable_state *get_state_ssa(const ir_variable *var);

   void allocate_state_arrays();

   void remove_decls();

private:
   /** mapping of old (non-SSA) variable -> ir_ssa_variable_state */
   struct hash_table *ht;

   /** mapping of new (SSA) variable -> old (non-SSA) variable */
   struct hash_table *new_to_old;

   friend class ir_ssa_variable_state;

   void remove_variable(const ir_variable *var);
};

}; /* private namespace */

ir_variable *
ir_ssa_variable_state::cur_var(bool use_undefined_var)
{
   if (this->stack_idx == -1) {
      if (use_undefined_var) {
	 return this->undefined_var;
      } else {
	 return NULL;
      }
   }

   return this->stack[this->stack_idx];
}

void
ir_ssa_variable_state::stack_push(ir_variable *new_var)
{
   this->stack_idx++;
   this->num_replaced++;
   assert(this->num_replaced <= this->num_defs);
   this->stack[this->stack_idx] = new_var;
   _mesa_hash_table_insert(this->v->new_to_old, _mesa_hash_pointer(new_var),
			   new_var, this->var);
}

void
ir_ssa_variable_state::stack_pop()
{
   assert(this->stack_idx != -1);
   ir_variable *var = this->stack[this->stack_idx];
   struct hash_entry *entry = _mesa_hash_table_search(this->v->new_to_old,
						      _mesa_hash_pointer(var),
						      var);
   _mesa_hash_table_remove(this->v->new_to_old, entry);
   this->stack_idx--;
}

ir_variable *
ir_ssa_variable_state::new_var()
{
   void *mem_ctx = ralloc_parent(this->var);
   char *new_name = ralloc_asprintf(mem_ctx, "%s_%i", this->var->name,
				    this->num_replaced);
   ir_variable *new_var = new(mem_ctx) ir_variable(this->var->type, new_name,
						   ir_var_temporary_ssa);
   this->stack_push(new_var);
   return new_var;
}

ir_ssa_variable_state::ir_ssa_variable_state(ir_variable* var,
					     ir_ssa_state_visitor *v,
					     ir_variable *undefined_var)
   : var(var), v(v), undefined_var(undefined_var)
{
   this->stack = NULL;
   this->num_replaced = 0;
   this->num_defs = 0;
   this->stack_idx = -1;
}

ir_ssa_variable_state::~ir_ssa_variable_state()
{
   assert(this->stack_idx == -1);
   free(this->stack);
}

static void
free_state(struct hash_entry *entry)
{
   ir_ssa_variable_state *isvs = (ir_ssa_variable_state *) entry->data;
   delete isvs;
}

ir_ssa_state_visitor::ir_ssa_state_visitor()
{
   this->ht = _mesa_hash_table_create(NULL, _mesa_key_pointer_equal);
   this->new_to_old = _mesa_hash_table_create(NULL, _mesa_key_pointer_equal);
}

ir_ssa_state_visitor::~ir_ssa_state_visitor(void)
{
   _mesa_hash_table_destroy(this->ht, free_state);
   _mesa_hash_table_destroy(this->new_to_old, NULL);
}

ir_visitor_status
ir_ssa_state_visitor::visit(ir_variable *var)
{
   if (var->data.mode == ir_var_auto || var->data.mode == ir_var_temporary) {
      void *mem_ctx = ralloc_parent(var);
      ir_assignment *assign = ssa_assign("undefined",
					 ir_constant::zero(mem_ctx, var->type));
      ir_variable *undefined_var = assign->lhs->as_dereference_variable()->var;
      var->insert_after(assign);
      ir_ssa_variable_state *entry = new ir_ssa_variable_state(var, this,
							       undefined_var);
      _mesa_hash_table_insert(this->ht, _mesa_hash_pointer(var), var, entry);
   }

   return visit_continue;
}

/*
 * We currently have no way to convert variables referenced as records and
 * arrays to SSA form, so don't track them.
 */

ir_visitor_status
ir_ssa_state_visitor::visit_enter(ir_dereference_record *deref)
{
   const ir_variable *var = deref->variable_referenced();

   if (var) {
      remove_variable(var);
   }

   return visit_continue;
}

ir_visitor_status
ir_ssa_state_visitor::visit_enter(ir_dereference_array *deref)
{
   const ir_variable *var = deref->variable_referenced();

   if (var) {
      remove_variable(var);
   }

   return visit_continue;
}

ir_visitor_status
ir_ssa_state_visitor::visit(ir_dereference_variable *deref)
{
   const ir_variable *var = deref->variable_referenced();

   if (var && this->in_assignee) {
      ir_ssa_variable_state *isvs =  this->get_state(var);
      if (isvs) {
	 isvs->num_defs++;
      }
   }

   return visit_continue;
}

ir_ssa_variable_state *
ir_ssa_state_visitor::get_state(const ir_variable *var)
{
   hash_entry *entry = _mesa_hash_table_search(this->ht, _mesa_hash_pointer(var), var);

   if (entry) {
      return (ir_ssa_variable_state *) entry->data;
   }

   return NULL;
}

ir_ssa_variable_state *
ir_ssa_state_visitor::get_state_ssa(const ir_variable* var)
{
   hash_entry *entry = _mesa_hash_table_search(this->new_to_old,
					       _mesa_hash_pointer(var), var);

   if (!entry) {
      /*
       * some SSA variables created (i.e. wrmask_temp) don't correspond to a
       * non-SSA variable, so we need to return NULL here
       */

      return NULL;
   }

   return this->get_state((const ir_variable *) entry->data);
}

void
ir_ssa_state_visitor::allocate_state_arrays()
{
   struct hash_entry *entry;

   hash_table_foreach(this->ht, entry) {
      ir_ssa_variable_state *isvs = (ir_ssa_variable_state *) entry->data;
      isvs->stack = (ir_variable**) malloc(sizeof(ir_variable *) * isvs->num_defs);
   }
}

/**
 * Remove the (now unused) variable declarations
 */

void
ir_ssa_state_visitor::remove_decls()
{
   struct hash_entry *entry;
   hash_table_foreach(this->ht, entry) {
      ir_variable *var = (ir_variable *) entry->key;
      var->remove();
   }
}


void
ir_ssa_state_visitor::remove_variable(const ir_variable *var)
{
   struct hash_entry *entry =
      _mesa_hash_table_search(this->ht, _mesa_hash_pointer(var), var);

   if (entry) {
      free_state(entry);
      _mesa_hash_table_remove(this->ht, entry);
   }
}

namespace {

/*
 * Rewrites out and inout parameters of functions to use a separate temporary.
 * For example if we have:
 *
 * void foo(out vec4 arg1, inout vec4 arg2);
 *
 * and it gets called like:
 *
 * foo(bar, baz);
 *
 * Then assuming bar and baz are local variables to be transformed into SSA, it
 * will be rewritten as
 *
 * vec4 tmp1, tmp2 = baz;
 * foo(tmp1, tmp2);
 * bar = tmp1;
 * baz = tmp2;
 *
 * This captures the correct semantics of the original while still allowing us
 * to convert bar and baz to SSA variables; in effect, this limits the
 * "non-SSA-ness" to those four statements, hopefully allowing more
 * optimizations to occur than if we simply prevented bar and baz from being
 * transformed into SSA form.
 */

class ir_parameter_visitor : public ir_hierarchical_visitor
{
public:
   ir_parameter_visitor(ir_ssa_state_visitor *ssv) : ssv(ssv)
   {
   }

   virtual ir_visitor_status visit_enter(ir_call *call);

private:
   ir_ssa_state_visitor *ssv;
};

}; /* private namespace */

ir_visitor_status
ir_parameter_visitor::visit_enter(ir_call *ir)
{
   void *mem_ctx = ralloc_parent(ir);

   ir_function_signature * callee = ir->callee;
   exec_node *formal_param_node = callee->parameters.head;
   exec_node *actual_param_node = ir->actual_parameters.head;

   while (!formal_param_node->is_tail_sentinel()) {
      ir_variable *formal_param
	 = (ir_variable *) formal_param_node;
      ir_rvalue *actual_param
	 = (ir_rvalue *) actual_param_node;

      /*
       * actual_param will get repurposed here, going from function parameter to
       * rhs of an assignment, and so we need to save a pointer to the next
       * actual parameter before the pointer in actual_param_node gets
       * destroyed.
       */

      exec_node *actual_param_next = actual_param_node->next;

      if (formal_param->data.mode == ir_var_function_out
          || formal_param->data.mode == ir_var_function_inout) {
         ir_variable *actual_param_var = actual_param->variable_referenced();
	 ir_ssa_variable_state *isvs = this->ssv->get_state(actual_param_var);
	 if (isvs != NULL) {
	    ir_variable *tmp = new(mem_ctx) ir_variable(actual_param_var->type,
							"function_temp",
							ir_var_temporary);

	    ir->insert_before(tmp);
	    if (formal_param->data.mode == ir_var_function_inout) {
	       ir_rvalue *actual_param_copy = actual_param->clone(mem_ctx, NULL);
	       ir->insert_before(assign(tmp, actual_param_copy));
	    }

	    ir_dereference_variable *deref
	       = new(mem_ctx) ir_dereference_variable(tmp);
	    actual_param_node->insert_before(deref);
	    actual_param_node->remove();

	    deref = new(mem_ctx) ir_dereference_variable(tmp);
	    ir_assignment *assign = new(mem_ctx) ir_assignment(actual_param, deref);
	    ir->insert_after(assign);
	    isvs->num_defs++;
	 }
      }

      formal_param_node = formal_param_node->next;
      actual_param_node = actual_param_next;
   }

   return visit_continue_with_parent;
}

namespace {


class ir_control_flow_entry : public exec_node
{
public:
   virtual class ir_if_entry *as_if_entry() { return NULL; }
   virtual class ir_loop_entry *as_loop_entry() { return NULL; };
};

class ir_if_entry : public ir_control_flow_entry
{
public:
   ir_if_entry(ir_if *ir) : ir(ir), in_then(false) {}

   virtual class ir_if_entry *as_if_entry() { return this; }

   ir_if *ir;
   bool in_then;
};

class ir_loop_entry : public ir_control_flow_entry
{
public:
   ir_loop_entry(ir_loop *loop) : loop(loop) {}

   virtual class ir_loop_entry *as_loop_entry() { return this; }

   ir_loop *loop;
};

class ir_phi_insertion_visitor : public ir_hierarchical_visitor
{
public:
   ir_phi_insertion_visitor(ir_ssa_state_visitor *ssv,
			    ir_dead_branches_visitor *dbv,
			    ir_loop_jumps_visitor *ljv)
      : ssv(ssv), dbv(dbv), ljv(ljv)
   {
   }

   virtual ir_visitor_status visit_enter(ir_if *);
   virtual ir_visitor_status visit_enter(ir_loop *);
   virtual ir_visitor_status visit(ir_dereference_variable *);

private:
   void add_phi(ir_if *ir, ir_variable *var);
   void add_phi(ir_loop *loop, ir_variable *var);

   ir_ssa_state_visitor *ssv;
   ir_dead_branches_visitor *dbv;
   ir_loop_jumps_visitor *ljv;

   exec_list cf_stack;
};

}; /* private namespace */

ir_visitor_status
ir_phi_insertion_visitor::visit_enter(ir_if *ir)
{
   //before doing anything, visit the condition, since it's really part of
   //the block before this
   ir->condition->accept(this);

   ir_if_entry entry(ir);

   this->cf_stack.push_head(&entry);
   entry.in_then = true;
   visit_list_elements(this, &ir->then_instructions);
   entry.in_then = false;
   visit_list_elements(this, &ir->else_instructions);
   this->cf_stack.pop_head();

   return visit_continue_with_parent;
}

ir_visitor_status
ir_phi_insertion_visitor::visit_enter(ir_loop *ir)
{
   ir_loop_entry entry(ir);

   this->cf_stack.push_head(&entry);
   visit_list_elements(this, &ir->body_instructions);
   this->cf_stack.pop_head();

   return visit_continue_with_parent;
}

ir_visitor_status
ir_phi_insertion_visitor::visit(ir_dereference_variable *ir)
{
   if (!this->in_assignee || this->cf_stack.is_empty()
       || !this->ssv->get_state(ir->var))
      return visit_continue;

   exec_node *cur_node = this->cf_stack.head;

   ir_control_flow_entry *cf_entry = (ir_control_flow_entry *) cur_node;
   ir_if_entry *if_entry = cf_entry->as_if_entry();
   if (if_entry) {
      ir_dead_branches *db = this->dbv->get_dead_branches(if_entry->ir);
      if ((db->then_dead && if_entry->in_then) ||
	  (db->else_dead && !if_entry->in_then)) {
	 if ((db->then_dead_return && if_entry->in_then) ||
	     (db->else_dead_return && !if_entry->in_then)) {
	    //The branch we're on leads to a return or discard, so the
	    //assignment can't lead to any join nodes
	    return visit_continue;
	 }

	 /*
	  * The branch we're on leads to a break or continue.
	  * We may need a phi node at the beginning, end, or both, depending
	  * on if we exit through a continue, break, or both, repsectively.
	  * We use an approximation here, and simply add a phi node to the
	  * beginning and end. The worst thing that can happen is that we wind
	  * up with a phi node where all the sources are the same, which can
	  * be easily optimized away in a later pass.
	  */

	 do {
	    cur_node = cur_node->next;
	    cf_entry = (ir_control_flow_entry *) cur_node;
	 } while (!cf_entry->as_loop_entry());
      }
   }

   /*
    * walk the stack of control flow elements, placing phi nodes as
    * necessary
    */

   for (; cur_node->next != NULL; cur_node = cur_node->next) {
      cf_entry = (ir_control_flow_entry *) cur_node;

      if_entry = cf_entry->as_if_entry();
      if (if_entry) {
	 this->add_phi(if_entry->ir, ir->var);
      } else {
	 ir_loop_entry *loop_entry = cf_entry->as_loop_entry();
	 this->add_phi(loop_entry->loop, ir->var);
      }
   }

   return visit_continue;
}

static bool
phi_exists(exec_list list, ir_variable *dest)
{
   foreach_list(n, &list) {
      ir_phi *phi = (ir_phi *) n;
      if (phi->dest == dest) {
	 return true;
      }
   }

   return false;
}

void
ir_phi_insertion_visitor::add_phi(ir_if *ir, ir_variable *var)
{
   void *mem_ctx = ralloc_parent(ir);

   //Don't duplicate phi nodes
   if (phi_exists(ir->phi_nodes, var)) {
      return;
   }

   ir_phi_if *phi = new (mem_ctx) ir_phi_if(var, var, var);
   ir->phi_nodes.push_tail(phi);

   ir_ssa_variable_state *isvs = this->ssv->get_state(var);
   isvs->num_defs++;
}

void
ir_phi_insertion_visitor::add_phi(ir_loop *loop, ir_variable *var)
{
   void *mem_ctx = ralloc_parent(loop);

   //Don't duplicate phi nodes
   if (phi_exists(loop->begin_phi_nodes, var)) {
      return;
   }

   ir_loop_jumps *loop_jumps = this->ljv->get_loop_jumps(loop);

   ir_phi_loop_begin *phi_begin = new(mem_ctx) ir_phi_loop_begin(var, var, var);

   foreach_list(n, &loop_jumps->continues) {
      ir_loop_jump_entry *entry = (ir_loop_jump_entry *) n;

      ir_phi_jump_src *src = new(mem_ctx) ir_phi_jump_src();
      src->jump = entry->ir;
      src->src = var;

      phi_begin->continue_srcs.push_tail(src);
   }

   loop->begin_phi_nodes.push_tail(phi_begin);

   ir_phi_loop_end *phi_end = new(mem_ctx) ir_phi_loop_end(var);

   foreach_list(n, &loop_jumps->breaks) {
      ir_loop_jump_entry *entry = (ir_loop_jump_entry *) n;

      ir_phi_jump_src *src = new(mem_ctx) ir_phi_jump_src();
      src->jump = entry->ir;
      src->src = var;

      phi_end->break_srcs.push_tail(src);
   }

   loop->end_phi_nodes.push_tail(phi_end);

   ir_ssa_variable_state *isvs = this->ssv->get_state(var);
   isvs->num_defs += 2;
}

namespace {

class ir_rewrite_forward_visitor : public ir_hierarchical_visitor
{
public:
   ir_rewrite_forward_visitor(ir_ssa_state_visitor *ssv) : ssv(ssv)
   {
   }

   virtual ir_visitor_status visit_enter(ir_assignment *ir);
   virtual ir_visitor_status visit_enter(ir_call *ir);
   virtual ir_visitor_status visit(ir_dereference_variable *ir);

private:
   ir_ssa_state_visitor *ssv;
};

class ir_rewrite_backward_visitor : public ir_hierarchical_visitor
{
public:
   ir_rewrite_backward_visitor(ir_ssa_state_visitor *ssv) : ssv(ssv)
   {
   }

   virtual ir_visitor_status visit(ir_dereference_variable *ir);

private:
   ir_ssa_state_visitor *ssv;
};

}; /* private namespace */

ir_visitor_status
ir_rewrite_forward_visitor::visit_enter(ir_assignment *ir)
{
   //visit the rhs first, since variables are read before they are written
   ir->rhs->accept(this);

   ir_dereference_variable *deref = ir->lhs->as_dereference_variable();
   if (!deref) {
      /*
       * We are dereferencing an array or structure, which we cannot handle, but
       * there might still be variables referenced as indexes, which we need to
       * convert in the same manner we would convert the rhs
       */
      ir->lhs->accept(this);
      return visit_continue_with_parent;
   }

   ir_variable *var = deref->var;
   ir_ssa_variable_state *isvs = this->ssv->get_state(var);
   if (!isvs) {
      return visit_continue_with_parent;
   }

   void *mem_ctx = ralloc_parent(var);

   //handle writemask by lowering to quadop_vector
   if (var->type->is_vector()
       && ir->write_mask != (1 << var->type->vector_elements) - 1) {
      ir_assignment *temp_assign = ssa_assign("wrmask_temp", ir->rhs);
      ir_variable *temp = temp_assign->whole_variable_written();
      this->base_ir->insert_before(temp_assign);

      ir_rvalue *inputs[4];
      int i, j = 0;

      for (i = 0; i < var->type->vector_elements; i++) {
	 if (ir->write_mask & (1 << i)) {
	    inputs[i] = swizzle_component(temp, j++);
	 } else {
	    inputs[i] = swizzle_component(isvs->cur_var(true), i);
	 }
      }
      for (; i < 4; i++) {
	 inputs[i] = NULL;
      }

      ir->rhs = new(mem_ctx) ir_expression(ir_quadop_vector, var->type,
					   inputs[0], inputs[1], inputs[2],
					   inputs[3]);

      ir->write_mask = (1 << var->type->vector_elements) - 1;
   }

   //handle conditional assignment
   if (ir->condition && !ir->condition->is_one()) {
      ir->condition->accept(this);

      //replace the conditional assignment by a conditional select
      ir_variable *old_var = isvs->cur_var(true);

      ir->rhs = csel(ir->condition, ir->rhs, old_var);

      ir->condition = NULL;
   }

   ir_variable *new_var = isvs->new_var();
   new_var->ssa_assignment = ir;

   deref->var = new_var;

   return visit_continue_with_parent;
}

ir_visitor_status
ir_rewrite_forward_visitor::visit_enter(ir_call *ir)
{
   visit_list_elements(this, &ir->actual_parameters, false);

   if (ir->return_deref != NULL) {
      ir_dereference_variable *deref =
	 ir->return_deref->as_dereference_variable();

      if (!deref) {
	 ir->return_deref->accept(this);
	 return visit_continue_with_parent;
      }

      ir_variable *var = deref->var;
      ir_ssa_variable_state *isvs = this->ssv->get_state(var);
      if (!isvs) {
	 return visit_continue_with_parent;
      }

      ir_variable *new_var = isvs->new_var();
      new_var->ssa_call = ir;

      deref->var = new_var;
   }

   return visit_continue_with_parent;
}

ir_visitor_status
ir_rewrite_forward_visitor::visit(ir_dereference_variable *ir)
{
   ir_ssa_variable_state *isvs = this->ssv->get_state(ir->var);
   if (isvs) {
      ir->var = isvs->cur_var(true);
   }
   return visit_continue;
}

ir_visitor_status
ir_rewrite_backward_visitor::visit(ir_dereference_variable *ir)
{
   if (this->in_assignee && ir->var->data.mode == ir_var_temporary_ssa) {
      ir_ssa_variable_state *isvs = this->ssv->get_state_ssa(ir->var);
      if (isvs) {
	 isvs->stack_pop();
      }
   }
   return visit_continue;
}

namespace {

class ir_rewrite_visitor {
public:
   ir_rewrite_visitor(ir_ssa_state_visitor *, ir_dead_branches_visitor *);

   void rewrite(exec_list *instructions);

private:
   void rewrite_forwards(exec_list *instructions);
   void rewrite_backwards(exec_list *instructions);

   void rewrite(ir_if *);
   void rewrite(ir_loop *);
   void rewrite(ir_loop_jump *);

   void rewrite_backwards(ir_if *);
   void rewrite_backwards(ir_loop *);

   void rewrite_phi_dest(ir_phi *);

   ir_ssa_state_visitor *ssv;
   ir_dead_branches_visitor *dbv;
   ir_rewrite_forward_visitor rfv;
   ir_rewrite_backward_visitor rbv;

   ir_loop *outer_loop;
};

}; /* private namespace */

ir_rewrite_visitor::ir_rewrite_visitor(ir_ssa_state_visitor *ssv,
				       ir_dead_branches_visitor* dbv)
   : ssv(ssv), dbv(dbv), rfv(ssv), rbv(ssv)
{
   outer_loop = NULL;
}

void
ir_rewrite_visitor::rewrite(exec_list *instructions)
{
   this->rewrite_forwards(instructions);
   this->rewrite_backwards(instructions);
}

void
ir_rewrite_visitor::rewrite_forwards(exec_list *instructions)
{
   foreach_list(n, instructions) {
      ir_instruction *ir = (ir_instruction *) n;

      switch (ir->ir_type) {
	 case ir_type_if:
	    this->rewrite(ir->as_if());
	    break;

	 case ir_type_loop:
	    this->rewrite(ir->as_loop());
	    break;

	 case ir_type_loop_jump:
	    this->rewrite(ir->as_loop_jump());
	    break;

	 case ir_type_variable:
	    break;

	 default:
	    //rfv needs to know this to know where to insert the writemask temp
	    this->rfv.base_ir = ir;
	    ir->accept(&this->rfv);
	    break;
      }
   }
}

void
ir_rewrite_visitor::rewrite_backwards(exec_list *instructions)
{
   foreach_list_reverse(n, instructions) {
      ir_instruction *ir = (ir_instruction *) n;

      switch (ir->ir_type) {
	 case ir_type_if:
	    this->rewrite_backwards(ir->as_if());
	    break;

	 case ir_type_loop:
	    this->rewrite_backwards(ir->as_loop());
	    break;

	 default:
	    ir->accept(&this->rbv);
	    break;
      }
   }
}

void
ir_rewrite_visitor::rewrite(ir_if *ir)
{
   ir->condition->accept(&this->rfv);

   ir_dead_branches *dead_branches = this->dbv->get_dead_branches(ir);
   if (dead_branches->then_dead) {
      if (dead_branches->else_dead) {
	 this->rewrite(&ir->then_instructions);
	 this->rewrite(&ir->else_instructions);
      } else {
	 this->rewrite(&ir->then_instructions);
	 this->rewrite_forwards(&ir->else_instructions);
      }
   } else if (dead_branches->else_dead) {
      this->rewrite(&ir->else_instructions);
      this->rewrite_forwards(&ir->then_instructions);
   } else {
      this->rewrite_forwards(&ir->then_instructions);
      foreach_list(n, &ir->phi_nodes) {
	 ir_phi_if *phi = (ir_phi_if *) n;
	 ir_ssa_variable_state *isvs;

	 isvs = this->ssv->get_state(phi->if_src);
	 phi->if_src = isvs->cur_var(false);
      }
      this->rewrite_backwards(&ir->then_instructions);

      this->rewrite_forwards(&ir->else_instructions);
      foreach_list(n, &ir->phi_nodes) {
	 ir_phi_if *phi = (ir_phi_if *) n;
	 ir_ssa_variable_state *isvs;

	 isvs = this->ssv->get_state(phi->else_src);
	 phi->else_src = isvs->cur_var(false);
      }
      this->rewrite_backwards(&ir->else_instructions);

      foreach_list(n, &ir->phi_nodes) {
	 ir_phi_if *phi = (ir_phi_if *) n;

	 this->rewrite_phi_dest(phi);
      }
   }
}

void
ir_rewrite_visitor::rewrite(ir_loop *ir)
{
   ir_loop *old_outer_loop = this->outer_loop;
   this->outer_loop = ir;

   foreach_list(n, &ir->begin_phi_nodes) {
      ir_phi_loop_begin *phi = (ir_phi_loop_begin *) n;
      ir_ssa_variable_state *isvs;

      isvs = this->ssv->get_state(phi->enter_src);
      phi->enter_src = isvs->cur_var(false);
   }

   foreach_list(n, &ir->begin_phi_nodes) {
      ir_phi_loop_begin *phi = (ir_phi_loop_begin *) n;

      this->rewrite_phi_dest(phi);
   }

   this->rewrite_forwards(&ir->body_instructions);

   foreach_list(n, &ir->begin_phi_nodes) {
      ir_phi_loop_begin *phi = (ir_phi_loop_begin *) n;
      ir_ssa_variable_state *isvs;

      isvs = this->ssv->get_state(phi->repeat_src);
      phi->repeat_src = isvs->cur_var(false);
   }

   this->rewrite_backwards(&ir->body_instructions);

   foreach_list(n, &ir->begin_phi_nodes) {
      ir_phi_loop_begin *phi = (ir_phi_loop_begin *) n;
      ir_ssa_variable_state *isvs = this->ssv->get_state_ssa(phi->dest);
      isvs->stack_pop();
   }

   foreach_list(n, &ir->end_phi_nodes) {
      ir_phi_loop_end *phi = (ir_phi_loop_end *) n;

      this->rewrite_phi_dest(phi);
   }

   this->outer_loop = old_outer_loop;
}

void
ir_rewrite_visitor::rewrite(ir_loop_jump *ir)
{
   switch (ir->mode) {
      case ir_loop_jump::jump_break:
	 foreach_list(node, &this->outer_loop->end_phi_nodes) {
	    ir_phi_loop_end *phi = (ir_phi_loop_end *) node;
	    foreach_list(src_node, &phi->break_srcs) {
	       ir_phi_jump_src *src = (ir_phi_jump_src *) src_node;
	       if (src->jump == ir) {
		  ir_ssa_variable_state *isvs = this->ssv->get_state(src->src);
		  src->src = isvs->cur_var(false);
		  break;
	       }
	    }
	 }
	 break;

      case ir_loop_jump::jump_continue:
	 foreach_list(node, &this->outer_loop->begin_phi_nodes) {
	    ir_phi_loop_begin *phi = (ir_phi_loop_begin *) node;
	    foreach_list(src_node, &phi->continue_srcs) {
	       ir_phi_jump_src *src = (ir_phi_jump_src *) src_node;
	       if (src->jump == ir) {
		  ir_ssa_variable_state *isvs = this->ssv->get_state(src->src);
		  src->src = isvs->cur_var(false);
		  break;
	       }
	    }
	 }
	 break;

      default:
	 assert(0);
	 break;
   }
}

void
ir_rewrite_visitor::rewrite_backwards(ir_if *ir)
{
   ir_dead_branches *dead_branches = this->dbv->get_dead_branches(ir);
   if (dead_branches->then_dead) {
      if (!dead_branches->else_dead) {
	 this->rewrite_backwards(&ir->else_instructions);
      }
   } else if (dead_branches->else_dead) {
      this->rewrite_backwards(&ir->then_instructions);
   } else {
      foreach_list(n, &ir->phi_nodes) {
	 ir_phi_if *phi = (ir_phi_if *) n;
	 ir_ssa_variable_state *isvs = this->ssv->get_state_ssa(phi->dest);
	 isvs->stack_pop();
      }
   }
}

void
ir_rewrite_visitor::rewrite_backwards(ir_loop *ir)
{
   foreach_list(n, &ir->end_phi_nodes) {
      ir_phi_loop_end *phi = (ir_phi_loop_end *) n;
      ir_ssa_variable_state *isvs = this->ssv->get_state_ssa(phi->dest);
      isvs->stack_pop();
   }
}

void
ir_rewrite_visitor::rewrite_phi_dest(ir_phi *ir)
{
   ir_ssa_variable_state *isvs = this->ssv->get_state(ir->dest);
   ir_variable *new_var = isvs->new_var();
   new_var->ssa_phi = ir;
   ir->dest = new_var;
}

static void
convert_to_ssa_function(exec_list *instructions)
{
   ir_dead_branches_visitor dbv;
   dbv.run(instructions);

   ir_loop_jumps_visitor ljv;
   ljv.run(instructions);

   ir_ssa_state_visitor ssv;
   ssv.run(instructions);

   ir_parameter_visitor pv(&ssv);
   pv.run(instructions);

   ir_phi_insertion_visitor piv(&ssv, &dbv, &ljv);
   piv.run(instructions);

   ssv.allocate_state_arrays();

   ir_rewrite_visitor rv(&ssv, &dbv);
   rv.rewrite(instructions);

   ssv.remove_decls();
}

void
convert_to_ssa(exec_list *instructions)
{
   foreach_list(node, instructions) {
      ir_instruction *ir = (ir_instruction *) node;
      ir_function *f = ir->as_function();
      if (f) {
	 foreach_list(sig_node, &f->signatures) {
	    ir_function_signature *sig = (ir_function_signature *) sig_node;

	    convert_to_ssa_function(&sig->body);
	 }
      }
   }
}
