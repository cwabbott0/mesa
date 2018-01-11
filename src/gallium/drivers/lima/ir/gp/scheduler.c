/*
 * Copyright (c) 2017 Lima Project
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sub license,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 */

#include <limits.h>

#include "gpir.h"

/*
 * GP schedule algorithm (by Connor Abbott <cwabbott0@gmail.com>)
 *
 * Pre schedule phase:
 * 1. order all nodes in a sequence
 * 2. convert the real reg read/write to GP load/store node, now all
 *    variable is SSA
 * 3. do reg alloc for all SSA with 11 reg (value reg) and spill with
 *    load/store to real reg if needed
 * 4. add fake dependency like this:
 *    after step 3, node sequence is
 *      01: r1=r2+r3
 *      02: r4=r1+r2
 *      03: r1=r5+r6
 *    we should add a fake dependency of node 3 to node 2 like a
 *    write-after-read dep. But this is not really write-after-read
 *    dep because there's no r1 really, because it's a value register.
 *    We need this fake dep in the schedule phase to make sure in any
 *    schedule point, there're only <=11 input needed by the past
 *    scheduled nodes.
 * 5. build DAG according to all the real and fake dep
 *
 * Schedule phase:
 * 1. Compute the nodes ready to schedule, if no nodes, exit
 * 2. Create a new GP instruction, and call it as current instr
 * 3. For any nodes with a use 2 cycles ago with a definition ready to
 *    schedule, schedule that definition immediately if possible, or else
 *    schedule a move.
 * 4. For any nodes with a use 2 cycles ago but the definition not
 *    scheduled and not ready to schedule, schedule a move immediately
 *    to prevent the value from falling off the queue.
 * 5. Calculate the number of remaining nodes with a use 1 cycle ago but
 *    the definition not yet scheduled, and if there are more than 5,
 *    schedule moves or definitions for the rest now.
 * 6. Schedule the rest of the available nodes using your favorite heuristic
 *    to current instr.
 * 7. go to step 1
 *
 * Step 5 for the current instruction guarantees that steps 3 and 4 for
 * the next instruction will always succeed, so it's only step 5 that can
 * possibly fail. Now, note that the nodes whose definitions have not yet
 * been scheduled but one or more use has been scheduled, are exactly the
 * nodes that are live in the final schedule. Therefore there will never
 * be more than 11 of them (guarenteed by the 11 value reg alloc and the
 * fake dep added before schedule). The worst case for step 5 is that all of
 * these nodes had a use 1 cycle ago, which means that none of them hit
 * case 3 or 4 already, so there are 6 slots still available so step 5
 * will always succeed. In general, even if there are exactly 11 values
 * live, if n are scheduled in steps 3 and 4, there are 11-n left in step
 * 4 so at most 11-n-5 = 6-n are scheduled in step 5 and therefore 6 are
 * scheduled total, below the limit. So the algorithm will always succeed.
 */

typedef struct {
   struct list_head ready_list;
   int ready_list_slots;
   gpir_instr *instr;
   gpir_block *block;
} sched_ctx;

static int gpir_min_dist_alu(gpir_dep *dep)
{
   switch (dep->pred->op) {
   case gpir_op_load_uniform:
   case gpir_op_load_temp:
   case gpir_op_load_reg:
   case gpir_op_load_attribute:
      return 0;

   case gpir_op_complex1:
      return 2;

   default:
      return 1;
   }
}

static int gpir_get_min_dist(gpir_dep *dep)
{
   switch (dep->type) {
   case GPIR_DEP_INPUT:
      switch (dep->succ->op) {
      case gpir_op_store_temp:
      case gpir_op_store_reg:
      case gpir_op_store_varying:
         /* store must use alu node as input */
         if (dep->pred->type == gpir_node_type_load)
            return INT_MAX >> 2;
         else
            return 0;

      default:
         return gpir_min_dist_alu(dep);
      }

   case GPIR_DEP_OFFSET:
      assert(dep->succ->op == gpir_op_store_temp);
      return gpir_min_dist_alu(dep);

   case GPIR_DEP_READ_AFTER_WRITE:
      switch (dep->succ->op) {
      case gpir_op_load_temp:
         assert(dep->pred->op == gpir_op_store_temp);
         return 4;
      case gpir_op_load_reg:
         assert(dep->pred->op == gpir_op_store_reg);
         return 3;
      case gpir_op_load_uniform:
         assert(dep->pred->op == gpir_op_store_temp_load_off0 ||
                dep->pred->op == gpir_op_store_temp_load_off1 ||
                dep->pred->op == gpir_op_store_temp_load_off2);
         return 4;
      default:
         assert(0);
      }

   case GPIR_DEP_WRITE_AFTER_READ:
      switch (dep->pred->op) {
      case gpir_op_load_temp:
         assert(dep->succ->op == gpir_op_store_temp);
         return -3;
      case gpir_op_load_reg:
         assert(dep->succ->op == gpir_op_store_reg);
         return -2;
      case gpir_op_load_uniform:
         assert(dep->succ->op == gpir_op_store_temp_load_off0 ||
                dep->succ->op == gpir_op_store_temp_load_off1 ||
                dep->succ->op == gpir_op_store_temp_load_off2);
         return -3;
      default:
         assert(0);
      }

   case GPIR_DEP_VREG_WRITE_AFTER_READ:
      return 0;

   case GPIR_DEP_VREG_READ_AFTER_WRITE:
      assert(0); /* not possible, this is GPIR_DEP_INPUT */
   }

   return 0;
}

static int gpir_max_dist_alu(gpir_dep *dep)
{
   switch (dep->pred->op) {
   case gpir_op_load_uniform:
   case gpir_op_load_temp:
      return 0;
   case gpir_op_load_attribute:
      return 1;
   case gpir_op_load_reg:
      if (dep->pred->sched.pos < GPIR_INSTR_SLOT_REG0_LOAD0 ||
          dep->pred->sched.pos > GPIR_INSTR_SLOT_REG0_LOAD3)
         return 0;
      else
         return 1;
   case gpir_op_exp2_impl:
   case gpir_op_log2_impl:
   case gpir_op_rcp_impl:
   case gpir_op_rsqrt_impl:
   case gpir_op_store_temp_load_off0:
   case gpir_op_store_temp_load_off1:
   case gpir_op_store_temp_load_off2:
      return 1;
   case gpir_op_mov:
      if (dep->pred->sched.pos == GPIR_INSTR_SLOT_COMPLEX)
         return 1;
      else
         return 2;
   default:
      return 2;
   }
}

static int gpir_get_max_dist(gpir_dep *dep)
{
   switch (dep->type) {
   case GPIR_DEP_INPUT:
      switch (dep->succ->op) {
      case gpir_op_store_temp:
      case gpir_op_store_reg:
      case gpir_op_store_varying:
         return 0;

      default:
         return gpir_max_dist_alu(dep);
      }

   case GPIR_DEP_OFFSET:
      assert(dep->succ->op == gpir_op_store_temp);
      return gpir_max_dist_alu(dep);

   default:
      return INT_MAX >> 2; /* Don't want to overflow... */
   }
}

static void schedule_update_distance(gpir_node *node)
{
   if (gpir_node_is_leaf(node)) {
      node->sched.dist = 0;
      return;
   }

   gpir_node_foreach_pred(node, dep) {
      gpir_node *pred = dep->pred;

      if (pred->sched.dist < 0)
         schedule_update_distance(pred);

      int dist = pred->sched.dist + gpir_min_dist_alu(dep);
      if (node->sched.dist < dist)
         node->sched.dist = dist;
   }
}

static void schedule_insert_ready_list(struct list_head *ready_list,
                                       gpir_node *insert_node)
{
   /* if this node is fully ready or partially ready
    *   fully ready: all successors have been scheduled
    *   partially ready: part of input successors have been scheduled
    *
    * either fully ready or partially ready node need be inserted to
    * the ready list, but we only schedule a move node for partially
    * ready node.
    */
   bool ready = true, insert = false;
   gpir_node_foreach_succ(insert_node, dep) {
      gpir_node *succ = dep->succ;
      if (succ->sched.instr >= 0) {
         if (dep->type == GPIR_DEP_INPUT)
            insert = true;
      }
      else
         ready = false;
   }

   insert_node->sched.ready = ready;
   /* for root node */
   insert |= ready;

   if (!insert || insert_node->sched.inserted)
      return;

   struct list_head *insert_pos = ready_list;
   list_for_each_entry(gpir_node, node, ready_list, list) {
      if (insert_node->sched.dist > node->sched.dist) {
         insert_pos = &node->list;
         break;
      }
   }

   list_addtail(&insert_node->list, insert_pos);
   insert_node->sched.inserted = true;
}

static int gpir_get_max_start(gpir_node *node)
{
   int max_start = 0;

   /* find the max start instr constrainted by all successors */
   gpir_node_foreach_succ(node, dep) {
      gpir_node *succ = dep->succ;
      if (succ->sched.instr < 0)
         continue;

      int start = succ->sched.instr + gpir_get_min_dist(dep);
      if (start > max_start)
         max_start = start;
   }

   return max_start;
}

static int gpir_get_min_end(gpir_node *node)
{
   int min_end = INT_MAX;

   /* find the min end instr constrainted by all successors */
   gpir_node_foreach_succ(node, dep) {
      gpir_node *succ = dep->succ;
      if (succ->sched.instr < 0)
         continue;

      int end = succ->sched.instr + gpir_get_max_dist(dep);
      if (end < min_end)
         min_end = end;
   }

   return min_end;
}

static gpir_node *gpir_sched_instr_has_load(gpir_instr *instr, gpir_node *node)
{
   gpir_load_node *load = gpir_node_to_load(node);

   for (int i = GPIR_INSTR_SLOT_REG0_LOAD0; i <= GPIR_INSTR_SLOT_MEM_LOAD3; i++) {
      if (!instr->slots[i])
         continue;

      gpir_load_node *iload = gpir_node_to_load(instr->slots[i]);
      if (load->node.op == iload->node.op &&
          load->index == iload->index &&
          load->component == iload->component)
         return &iload->node;
   }
   return NULL;
}

static bool schedule_try_place_node(gpir_instr *instr, gpir_node *node)
{
   if (node->type == gpir_node_type_load) {
      gpir_node *load = gpir_sched_instr_has_load(instr, node);
      if (load) {
         gpir_debug("same load %d in instr %d for node %d\n",
                    load->index, instr->index, node->index);

         /* not really merge two node, just fake scheduled same place */
         node->sched.instr = load->sched.instr;
         node->sched.pos = load->sched.pos;
         return true;
      }
   }

   node->sched.instr = instr->index;

   int *slots = gpir_op_infos[node->op].slots;
   for (int i = 0; slots[i] != GPIR_INSTR_SLOT_END; i++) {
      node->sched.pos = slots[i];
      if (node->sched.instr >= gpir_get_max_start(node) &&
          node->sched.instr <= gpir_get_min_end(node) &&
          gpir_instr_try_insert_node(instr, node))
         return true;
   }

   node->sched.instr = -1;
   node->sched.pos = -1;
   return false;
}

static gpir_node *schedule_create_move_node(gpir_node *node)
{
   gpir_alu_node *move = gpir_node_create(node->block, gpir_op_mov);
   if (unlikely(!move))
      return NULL;

   move->children[0] = node;
   move->num_child = 1;

   move->node.sched.instr = -1;
   move->node.sched.pos = -1;
   move->node.sched.dist = node->sched.dist;

   gpir_debug("create move %d for %d\n", move->node.index, node->index);
   return &move->node;
}

static bool gpir_is_input_node(gpir_node *node)
{
   gpir_node_foreach_succ(node, dep) {
      if (dep->type == GPIR_DEP_INPUT)
         return true;
   }
   return false;
}

/* Get the number of slots required for a node on the ready list.
 */
static int gpir_get_slots_required(gpir_node *node)
{
   if (!gpir_is_input_node(node))
      return 0;

   if (gpir_op_infos[node->op].may_consume_two_slots && node->sched.ready) {
      /* If the node is fully ready, we have to assume that it may get
       * scheduled at any time, at which point it takes up two slots.
       */
      return 2;
   }

   return 1;
}

/* Once we schedule the successor, would the predecessor be fully ready? */
static bool pred_almost_ready(gpir_dep *dep)
{
   bool fully_ready = true;
   gpir_node_foreach_succ(dep->pred, other_dep) {
      gpir_node *succ = other_dep->succ;
      if (succ->sched.instr < 0 && dep->succ != other_dep->succ) {
         fully_ready = false;
         break;
      }
   }

   return fully_ready;
}

/* Speculatively schedule a dep, and count how many slots it consumes.
 */
static int gpir_get_dep_slots(sched_ctx *ctx, gpir_dep *dep)
{
   gpir_node *pred = dep->pred;
   if (!gpir_is_input_node(pred))
      return 0;

   /* Try and speculatively schedule any loads. */
   if (pred->type == gpir_node_type_load && pred_almost_ready(dep) &&
       schedule_try_place_node(ctx->instr, pred)) {
      return 0;
   }

   int total = 0;
   if (!pred->sched.inserted)
      total++;

   if (gpir_op_infos[pred->op].may_consume_two_slots) {
      /* If pred goes from partially ready or not ready to fully ready, then
       * it takes up two slots as per gpir_get_slots_required(), so we need to
       * add an extra slot.
       */
      if (pred_almost_ready(dep))
         total++;
   }

   return total;
}

static void cleanup_speculative_loads(sched_ctx *ctx, gpir_node *node)
{
   gpir_node_foreach_pred(node, dep) {
      gpir_node *pred = dep->pred;
      if (pred->sched.instr >= 0) {
         pred->sched.instr = -1;
         gpir_instr_remove_node(ctx->instr, pred);
      }
   }
}

/* Get the total number of slots on the ready list if this node were to be
 * scheduled.
 */
static int gpir_get_ready_list_slots(sched_ctx *ctx, gpir_node *node)
{
   assert(ctx->ready_list_slots <= GPIR_VALUE_REG_NUM);
   int total = ctx->ready_list_slots - gpir_get_slots_required(node);

   if (!schedule_try_place_node(ctx->instr, node))
      return INT_MAX;

   gpir_node_foreach_pred(node, dep) {
      int slots = gpir_get_dep_slots(ctx, dep);
      if (node->op == gpir_op_mov && slots != 0) {
         /* At this stage, we only insert moves for loads that we couldn't
          * schedule immediately. If we couldn't schedule the load, there's
          * no point scheduling the move.
          */
         cleanup_speculative_loads(ctx, node);
         gpir_instr_remove_node(ctx->instr, node);
         return INT_MAX;
      }
      total += slots;
   }

   /* Cleanup any speculatively scheduled loads. */
   cleanup_speculative_loads(ctx, node);
   gpir_instr_remove_node(ctx->instr, node);

   return total;
}

static bool try_place_node(sched_ctx *ctx, gpir_node *node)
{
   if (!schedule_try_place_node(ctx->instr, node)) {
      gpir_debug("failed to place %d\n", node->index);
      return false;
   }

   gpir_debug("placed node %d\n", node->index);

   list_del(&node->list);
   list_add(&node->list, &ctx->block->node_list);
   gpir_node_foreach_pred(node, dep) {
      gpir_node *pred = dep->pred;
      schedule_insert_ready_list(&ctx->ready_list, pred);
   }

   return true;
}

static gpir_node *create_move(sched_ctx *ctx, gpir_node *node)
{
   gpir_node *move = schedule_create_move_node(node);
   list_del(&node->list);
   node->sched.ready = false;
   node->sched.inserted = false;
   gpir_node_replace_succ(move, node);
   gpir_node_add_dep(move, node, GPIR_DEP_INPUT);
   schedule_insert_ready_list(&ctx->ready_list, move);
   return move;
}

static bool try_schedule_node(sched_ctx *ctx, gpir_node *node)
{
   if (!try_place_node(ctx, node))
      return false;

   gpir_node_foreach_pred(node, dep) {
      gpir_node *pred = dep->pred;
      if (dep->type == GPIR_DEP_INPUT && pred->type == gpir_node_type_load) {
         if (!try_place_node(ctx, pred)) {
            create_move(ctx, pred);
         }
      }
   }

   return true;
}

static int gpir_get_curr_ready_list_slots(sched_ctx *ctx)
{
   int total = 0;
   list_for_each_entry(gpir_node, node, &ctx->ready_list, list) {
      total += gpir_get_slots_required(node);
   }

   return total;
}

/* What gpir_get_min_end() would return if node were replaced with a move
 * instruction not in the complex slot. Normally this is 2 + min_end, except
 * for some store instructions which must have the move node in the same
 * instruction.
 */
static int gpir_get_min_end_move(gpir_node *node)
{
   int min = INT_MAX;
   gpir_node_foreach_succ(node, dep) {
      gpir_node *succ = dep->succ;
      if (succ->sched.instr >= 0 && dep->type == GPIR_DEP_INPUT) {
         int dist = 2;
         switch (succ->op) {
         case gpir_op_store_temp:
         case gpir_op_store_reg:
         case gpir_op_store_varying:
            dist = 0;
            break;
         default:
            break;
         }
         if (min > succ->sched.instr + dist)
            min = succ->sched.instr + dist;
      }
   }
   return min;
}

static bool try_node(sched_ctx *ctx, bool max_only)
{
   gpir_node *min_node = NULL;
   int min_slots = INT_MAX;
   list_for_each_entry(gpir_node, node, &ctx->ready_list, list) {
      if (node->sched.ready) {
         /* First, schedule required stuff */
         if (max_only) {
            int end = gpir_get_min_end(node);
            if (end != ctx->instr->index)
               continue;
         }

         int slots = gpir_get_ready_list_slots(ctx, node);
         if (slots == INT_MAX)
            continue;

         if (node == NULL) {
            min_slots = slots;
            min_node = node;
            continue;
         }
         if (min_slots <= GPIR_VALUE_REG_NUM) {
            if (node->sched.dist <= min_node->sched.dist)
               break;
         }
         if (slots < min_slots) {
            min_slots = slots;
            min_node = node;
         }
      }
   }

   if (min_node && min_slots <= GPIR_VALUE_REG_NUM) {
      gpir_debug("trying to schedule %d (slots = %d)%s\n", min_node->index,
                 min_slots, max_only ? " (max)" : "");
      if (try_schedule_node(ctx, min_node))
         ctx->ready_list_slots = min_slots;
      return true;
   }

   return false;
}

static void place_move(sched_ctx *ctx, gpir_node *node)
{
   gpir_node *move = create_move(ctx, node);
   gpir_node_foreach_succ_safe(move, dep) {
      gpir_node *succ = dep->succ;
      if (succ->sched.instr < 0 ||
          ctx->instr->index < succ->sched.instr + gpir_get_min_dist(dep)) {
         gpir_node_replace_pred(dep, node);
         if (dep->type == GPIR_DEP_INPUT)
            gpir_node_replace_child(succ, move, node);
      }
   }
   MAYBE_UNUSED bool result = try_place_node(ctx, move);
   assert(result);
}

static bool sched_move(sched_ctx *ctx)
{
   list_for_each_entry(gpir_node, node, &ctx->ready_list, list) {
      if (gpir_is_input_node(node) &&
          gpir_get_min_end_move(node) == ctx->instr->index) {
         place_move(ctx, node);
         return true;
      }
   }

   int count = 0;
   list_for_each_entry(gpir_node, node, &ctx->ready_list, list) {
      if (gpir_is_input_node(node) &&
          gpir_get_min_end_move(node) == ctx->instr->index + 1) {
         count += gpir_get_slots_required(node);
      }
   }

   if (count > 5) {
      /* This is a bit tricky... if a two-slot instruction becomes ready, then
       * it could go from counting one slot to counting two. If there was
       * another use one instruction ago, then it would add to "count",
       * possibly making large enough that we can't get it below 5 without
       * running out of slots unless we evict the two-slot instruction itself
       * (that decreases count by 2 while only taking up one slot).
       */
      if (count - 5 > ctx->instr->alu_num_slot_free) {
         list_for_each_entry(gpir_node, node, &ctx->ready_list, list) {
            if (gpir_get_min_end_move(node) == ctx->instr->index + 1 &&
                node->op == gpir_op_complex1 && node->sched.ready) {
               gpir_debug("count > 5\n");
               place_move(ctx, node);
               return true;
            }
         }
      }

      list_for_each_entry(gpir_node, node, &ctx->ready_list, list) {
         /* complex1 has a latency of two cycles, so if it is ready, we want
          * to try not to insert a mov during the cycle after it becomes
          * ready, since this delays when it becomes available by an extra
          * cycle. Other opcodes don't have this problem, i.e. inserting a
          * move won't hurt anything.
          */
         if (gpir_get_min_end_move(node) == ctx->instr->index + 1 &&
             !(node->op == gpir_op_complex1 && node->sched.ready)) {
            gpir_debug("count > 5\n");
            place_move(ctx, node);
            return true;
         }
      }

      /* In the case where everything was complex1, we need to try again. */
      list_for_each_entry(gpir_node, node, &ctx->ready_list, list) {
         if (gpir_get_min_end_move(node) == ctx->instr->index + 1) {
            gpir_debug("count > 5\n");
            place_move(ctx, node);
            return true;
         }
      }
   }

   return false;
}

static bool gpir_sched_instr_pass(sched_ctx *ctx)
{
   /* First, schedule max nodes */
   if (try_node(ctx, true))
      return true;

   /* TODO: try to spill max nodes */

   /* Schedule moves for max nodes if we couldn't schedule them. */
   if (sched_move(ctx))
      return true;

   /* Try and schedule the rest of the nodes now. */
   return try_node(ctx, false);
}

static void schedule_print_pre_one_instr(sched_ctx *ctx)
{
   if (!lima_shader_debug_gp)
      return;

   printf("instr %d for ready list:", ctx->instr->index);
   list_for_each_entry(gpir_node, node, &ctx->ready_list, list) {
      printf(" %d/%c (%d, %d, %d)", node->index, node->sched.ready ? 'r' : 'p',
             node->sched.dist, gpir_get_slots_required(node),
             gpir_get_min_end_move(node));
   }
   printf("\n");
}

static void schedule_print_post_one_instr(gpir_instr *instr)
{
   if (!lima_shader_debug_gp)
      return;

   printf("post schedule instr");
   for (int i = 0; i < GPIR_INSTR_SLOT_NUM; i++) {
      if (instr->slots[i])
         printf(" %d/%d", i, instr->slots[i]->index);
   }
   printf("\n");
}


static bool schedule_one_instr(sched_ctx *ctx)
{
   gpir_instr *instr = gpir_instr_create(ctx->block);
   if (unlikely(!instr))
      return false;
   ctx->instr = instr;

   schedule_print_pre_one_instr(ctx);

   while (gpir_sched_instr_pass(ctx))
      assert(ctx->ready_list_slots == gpir_get_curr_ready_list_slots(ctx));

   schedule_print_post_one_instr(instr);
   return true;
}

static bool schedule_block(gpir_block *block)
{
   /* calculate distance */
   list_for_each_entry(gpir_node, node, &block->node_list, list) {
      if (gpir_node_is_root(node))
         schedule_update_distance(node);
   }

   sched_ctx ctx;
   list_inithead(&ctx.ready_list);
   ctx.block = block;
   ctx.ready_list_slots = 0;

   /* construct the ready list from root nodes */
   list_for_each_entry_safe(gpir_node, node, &block->node_list, list) {
      if (gpir_node_is_root(node))
         schedule_insert_ready_list(&ctx.ready_list, node);
   }

   list_inithead(&block->node_list);
   while (!list_empty(&ctx.ready_list)) {
      if (!schedule_one_instr(&ctx))
         return false;
   }

   return true;
}

static void schedule_build_vreg_dependency(gpir_block *block)
{
   /* merge dummy_f/m to the node created from */
   list_for_each_entry_safe(gpir_node, node, &block->node_list, list) {
      if (node->op == gpir_op_dummy_m) {
         gpir_alu_node *alu = gpir_node_to_alu(node);
         gpir_node *origin = alu->children[0];
         gpir_node *dummy_f = alu->children[1];

         gpir_node_foreach_succ(node, dep) {
            gpir_node *succ = dep->succ;
            /* origin and node may have same succ (by VREG/INPUT or
             * VREG/VREG dep), so use gpir_node_add_dep() instead of
             * gpir_node_replace_pred() */
            gpir_node_add_dep(succ, origin, dep->type);
            gpir_node_replace_child(succ, node, origin);
         }
         gpir_node_delete(dummy_f);
         gpir_node_delete(node);
      }
   }
}

static void schedule_build_preg_dependency(gpir_compiler *comp)
{
   /* merge reg with the same index */
   gpir_reg *regs[GPIR_VALUE_REG_NUM] = {0};
   list_for_each_entry(gpir_reg, reg, &comp->reg_list, list) {
      if (!regs[reg->index])
         regs[reg->index] = reg;
      else {
         list_splicetail(&reg->defs_list, &regs[reg->index]->defs_list);
         list_splicetail(&reg->uses_list, &regs[reg->index]->uses_list);
      }
   }

   /* calculate physical reg read/write dependency for load/store nodes */
   for (int i = 0; i < GPIR_VALUE_REG_NUM; i++) {
      gpir_reg *reg = regs[i];
      if (!reg)
         continue;

      /* sort reg write */
      struct list_head tmp_list;
      list_replace(&reg->defs_list, &tmp_list);
      list_inithead(&reg->defs_list);
      list_for_each_entry_safe(gpir_store_node, store, &tmp_list, reg_link) {
         struct list_head *insert_pos = &reg->defs_list;
         list_for_each_entry(gpir_store_node, st, &reg->defs_list, reg_link) {
            if (st->node.sched.index > store->node.sched.index) {
               insert_pos = &st->reg_link;
               break;
            }
         }
         list_del(&store->reg_link);
         list_addtail(&store->reg_link, insert_pos);
      }

      /* sort reg read */
      list_replace(&reg->uses_list, &tmp_list);
      list_inithead(&reg->uses_list);
      list_for_each_entry_safe(gpir_load_node, load, &tmp_list, reg_link) {
         struct list_head *insert_pos = &reg->uses_list;
         list_for_each_entry(gpir_load_node, ld, &reg->uses_list, reg_link) {
            if (ld->node.sched.index > load->node.sched.index) {
               insert_pos = &ld->reg_link;
               break;
            }
         }
         list_del(&load->reg_link);
         list_addtail(&load->reg_link, insert_pos);
      }

      /* insert dependency */
      gpir_store_node *store =
         list_first_entry(&reg->defs_list, gpir_store_node, reg_link);
      gpir_store_node *next = store->reg_link.next != &reg->defs_list ?
         list_first_entry(&store->reg_link, gpir_store_node, reg_link) : NULL;

      list_for_each_entry(gpir_load_node, load, &reg->uses_list, reg_link) {
         /* loop until load is between store and next */
         while (next && next->node.sched.index < load->node.sched.index) {
            store = next;
            next = store->reg_link.next != &reg->defs_list ?
               list_first_entry(&store->reg_link, gpir_store_node, reg_link) : NULL;
         }

         gpir_node_add_dep(&load->node, &store->node, GPIR_DEP_READ_AFTER_WRITE);
         if (next)
            gpir_node_add_dep(&next->node, &load->node, GPIR_DEP_WRITE_AFTER_READ);
      }
   }
}

static void print_statistic(gpir_compiler *comp, int save_index)
{
   int num_nodes[gpir_op_num] = {0};
   int num_created_nodes[gpir_op_num] = {0};

   list_for_each_entry(gpir_block, block, &comp->block_list, list) {
      list_for_each_entry(gpir_node, node, &block->node_list, list) {
         num_nodes[node->op]++;
         if (node->index >= save_index)
            num_created_nodes[node->op]++;
      }
   }

   printf("====== gpir scheduler statistic ======\n");
   printf("---- how many nodes are scheduled ----\n");
   int n = 0, l = 0;
   for (int i = 0; i < gpir_op_num; i++) {
      if (num_nodes[i]) {
         printf("%10s:%-6d", gpir_op_infos[i].name, num_nodes[i]);
         n += num_nodes[i];
         if (!(++l % 4))
            printf("\n");
      }
   }
   if (l % 4)
      printf("\n");
   printf("\ntotal: %d\n", n);

   printf("---- how many nodes are created ----\n");
   n = l = 0;
   for (int i = 0; i < gpir_op_num; i++) {
      if (num_created_nodes[i]) {
         printf("%10s:%-6d", gpir_op_infos[i].name, num_created_nodes[i]);
         n += num_created_nodes[i];
         if (!(++l % 4))
            printf("\n");
      }
   }
   if (l % 4)
      printf("\n");
   printf("\ntotal: %d\n", n);
   printf("------------------------------------\n");
}

bool gpir_schedule_prog(gpir_compiler *comp)
{
   int save_index = comp->cur_index;

   /* init schedule info */
   int index = 0;
   list_for_each_entry(gpir_block, block, &comp->block_list, list) {
      block->sched.instr_index = 0;
      list_for_each_entry(gpir_node, node, &block->node_list, list) {
         node->sched.instr = -1;
         node->sched.pos = -1;
         node->sched.index = index++;
         node->sched.dist = -1;
         node->sched.ready = false;
         node->sched.inserted = false;
      }
   }

   /* build fake/virtual dependency */
   list_for_each_entry(gpir_block, block, &comp->block_list, list) {
      schedule_build_vreg_dependency(block);
   }
   schedule_build_preg_dependency(comp);

   //gpir_debug("after scheduler build reg dependency\n");
   //gpir_node_print_prog_dep(comp);

   list_for_each_entry(gpir_block, block, &comp->block_list, list) {
      if (!schedule_block(block)) {
         gpir_error("fail schedule block\n");
         return false;
      }
   }

   if (lima_shader_debug_gp) {
      print_statistic(comp, save_index);
      gpir_instr_print_prog(comp);
   }

   return true;
}
