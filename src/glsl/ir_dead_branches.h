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

/**
 * \file ir_dead_branches.h
 *
 * Provides a visitor which determines, for each if instruction, whether
 * control will never flow the from the then-block or else-block
 * to the next instruction due to jump statements (break, continue, return,
 * discard).
 */

#include "ir.h"
#include "ir_visitor.h"

class ir_dead_branches
{
public:
   ir_dead_branches(ir_if *ir);

   ir_if *ir;

   /**
    * whether a jump statement is guarenteed to be hit when the
    * then_instructions are run, making the branch from the then_instructions
    * "dead"
    */
   bool then_dead;
   bool else_dead; /** < ditto for the else_instructions */

   /** whether the then branch is dead due to a return or discard */
   bool then_dead_return;
   bool else_dead_return; /** < ditto for else branch */
};

class ir_dead_branches_visitor : public ir_hierarchical_visitor
{
public:
   ir_dead_branches_visitor();
   ~ir_dead_branches_visitor();

   virtual ir_visitor_status visit_enter(ir_if *);
   virtual ir_visitor_status visit_enter(ir_loop *);
   virtual ir_visitor_status visit(ir_loop_jump *);
   virtual ir_visitor_status visit_enter(ir_return *);
   virtual ir_visitor_status visit_enter(ir_discard *);

   ir_dead_branches *get_dead_branches(ir_if *ir);

private:
   void visit_return();

   ir_if *outer_if;
   bool in_loop;
   bool in_then;

   struct hash_table *ht;
};
