/*
 * Copyright © 2010 Intel Corporation
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
 *    Eric Anholt <eric@anholt.net>
 *
 */

#pragma once

#include "brw_shader.h"

extern "C" {

#include <sys/types.h>

#include "main/macros.h"
#include "main/shaderobj.h"
#include "main/uniforms.h"
#include "program/prog_parameter.h"
#include "program/prog_print.h"
#include "program/prog_optimize.h"
#include "program/register_allocate.h"
#include "program/sampler.h"
#include "program/hash_table.h"
#include "brw_context.h"
#include "brw_eu.h"
#include "brw_wm.h"
#include "brw_shader.h"
}
#include "gen8_generator.h"
#include "glsl/glsl_types.h"
#include "glsl/ir.h"

#define MAX_SAMPLER_MESSAGE_SIZE 11

class bblock_t;
namespace {
   struct acp_entry;
}

namespace brw {
   class fs_live_variables;
}

class fs_reg {
public:
   DECLARE_RALLOC_CXX_OPERATORS(fs_reg)

   void init();

   fs_reg();
   fs_reg(float f);
   fs_reg(int32_t i);
   fs_reg(uint32_t u);
   fs_reg(struct brw_reg fixed_hw_reg);
   fs_reg(enum register_file file, int reg);
   fs_reg(enum register_file file, int reg, uint32_t type);
   fs_reg(class fs_visitor *v, const struct glsl_type *type);

   bool equals(const fs_reg &r) const;
   bool is_zero() const;
   bool is_one() const;
   bool is_null() const;
   bool is_valid_3src() const;
   fs_reg retype(uint32_t type);

   /** Register file: GRF, MRF, IMM. */
   enum register_file file;
   /**
    * Register number.  For MRF, it's the hardware register.  For
    * GRF, it's a virtual register number until register allocation
    */
   int reg;
   /**
    * Offset from the start of the contiguous register block.
    *
    * For pre-register-allocation GRFs, this is in units of a float per pixel
    * (1 hardware register for SIMD8 mode, or 2 registers for SIMD16 mode).
    * For uniforms, this is in units of 1 float.
    */
   int reg_offset;
   /** Register type.  BRW_REGISTER_TYPE_* */
   int type;
   bool negate;
   bool abs;
   bool sechalf;
   struct brw_reg fixed_hw_reg;
   int smear; /* -1, or a channel of the reg to smear to all channels. */

   /** Value for file == IMM */
   union {
      int32_t i;
      uint32_t u;
      float f;
   } imm;

   fs_reg *reladdr;
};

static const fs_reg reg_undef;
static const fs_reg reg_null_f(retype(brw_null_reg(), BRW_REGISTER_TYPE_F));
static const fs_reg reg_null_d(retype(brw_null_reg(), BRW_REGISTER_TYPE_D));
static const fs_reg reg_null_ud(retype(brw_null_reg(), BRW_REGISTER_TYPE_UD));

class ip_record : public exec_node {
public:
   DECLARE_RALLOC_CXX_OPERATORS(ip_record)

   ip_record(int ip)
   {
      this->ip = ip;
   }

   int ip;
};

class fs_inst : public backend_instruction {
public:
   DECLARE_RALLOC_CXX_OPERATORS(fs_inst)

   void init();

   fs_inst();
   fs_inst(enum opcode opcode);
   fs_inst(enum opcode opcode, fs_reg dst);
   fs_inst(enum opcode opcode, fs_reg dst, fs_reg src0);
   fs_inst(enum opcode opcode, fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst(enum opcode opcode, fs_reg dst,
           fs_reg src0, fs_reg src1,fs_reg src2);

   bool equals(fs_inst *inst);
   bool overwrites_reg(const fs_reg &reg);
   bool is_send_from_grf();
   bool is_partial_write();
   int regs_read(fs_visitor *v, int arg);

   bool reads_flag();
   bool writes_flag();

   fs_reg dst;
   fs_reg src[3];
   bool saturate;
   int conditional_mod; /**< BRW_CONDITIONAL_* */

   /* Chooses which flag subregister (f0.0 or f0.1) is used for conditional
    * mod and predication.
    */
   uint8_t flag_subreg;

   int mlen; /**< SEND message length */
   int regs_written; /**< Number of vgrfs written by a SEND message, or 1 */
   int base_mrf; /**< First MRF in the SEND message, if mlen is nonzero. */
   uint32_t texture_offset; /**< Texture offset bitfield */
   int sampler;
   int target; /**< MRT target. */
   bool eot;
   bool header_present;
   bool shadow_compare;
   bool force_uncompressed;
   bool force_sechalf;
   bool force_writemask_all;
   uint32_t offset; /* spill/unspill offset */

   /** @{
    * Annotation for the generated IR.  One of the two can be set.
    */
   const void *ir;
   const char *annotation;
   /** @} */
};

/**
 * The fragment shader front-end.
 *
 * Translates either GLSL IR or Mesa IR (for ARB_fragment_program) into FS IR.
 */
class fs_visitor : public backend_visitor
{
public:

   fs_visitor(struct brw_context *brw,
              struct brw_wm_compile *c,
              struct gl_shader_program *shader_prog,
              struct gl_fragment_program *fp,
              unsigned dispatch_width);
   ~fs_visitor();

   fs_reg *variable_storage(ir_variable *var);
   int virtual_grf_alloc(int size);
   void import_uniforms(fs_visitor *v);

   void visit(ir_variable *ir);
   void visit(ir_assignment *ir);
   void visit(ir_dereference_variable *ir);
   void visit(ir_dereference_record *ir);
   void visit(ir_dereference_array *ir);
   void visit(ir_expression *ir);
   void visit(ir_texture *ir);
   void visit(ir_if *ir);
   void visit(ir_constant *ir);
   void visit(ir_swizzle *ir);
   void visit(ir_return *ir);
   void visit(ir_loop *ir);
   void visit(ir_loop_jump *ir);
   void visit(ir_discard *ir);
   void visit(ir_call *ir);
   void visit(ir_function *ir);
   void visit(ir_function_signature *ir);
   void visit(ir_phi *ir);
   void visit(ir_phi_if *ir);
   void visit(ir_phi_loop_begin *ir);
   void visit(ir_phi_loop_end *ir);
   void visit(ir_emit_vertex *);
   void visit(ir_end_primitive *);

   uint32_t gather_channel(ir_texture *ir, int sampler);
   void swizzle_result(ir_texture *ir, fs_reg orig_val, int sampler);

   bool can_do_source_mods(fs_inst *inst);

   fs_inst *emit(fs_inst inst);
   fs_inst *emit(fs_inst *inst);
   void emit(exec_list list);

   fs_inst *emit(enum opcode opcode);
   fs_inst *emit(enum opcode opcode, fs_reg dst);
   fs_inst *emit(enum opcode opcode, fs_reg dst, fs_reg src0);
   fs_inst *emit(enum opcode opcode, fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *emit(enum opcode opcode, fs_reg dst,
                 fs_reg src0, fs_reg src1, fs_reg src2);

   fs_inst *MOV(fs_reg dst, fs_reg src);
   fs_inst *NOT(fs_reg dst, fs_reg src);
   fs_inst *RNDD(fs_reg dst, fs_reg src);
   fs_inst *RNDE(fs_reg dst, fs_reg src);
   fs_inst *RNDZ(fs_reg dst, fs_reg src);
   fs_inst *FRC(fs_reg dst, fs_reg src);
   fs_inst *ADD(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *MUL(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *MACH(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *MAC(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *SHL(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *SHR(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *ASR(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *AND(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *OR(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *XOR(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *IF(uint32_t predicate);
   fs_inst *IF(fs_reg src0, fs_reg src1, uint32_t condition);
   fs_inst *CMP(fs_reg dst, fs_reg src0, fs_reg src1,
                uint32_t condition);
   fs_inst *LRP(fs_reg dst, fs_reg a, fs_reg y, fs_reg x);
   fs_inst *DEP_RESOLVE_MOV(int grf);
   fs_inst *BFREV(fs_reg dst, fs_reg value);
   fs_inst *BFE(fs_reg dst, fs_reg bits, fs_reg offset, fs_reg value);
   fs_inst *BFI1(fs_reg dst, fs_reg bits, fs_reg offset);
   fs_inst *BFI2(fs_reg dst, fs_reg bfi1_dst, fs_reg insert, fs_reg base);
   fs_inst *FBH(fs_reg dst, fs_reg value);
   fs_inst *FBL(fs_reg dst, fs_reg value);
   fs_inst *CBIT(fs_reg dst, fs_reg value);
   fs_inst *MAD(fs_reg dst, fs_reg c, fs_reg b, fs_reg a);
   fs_inst *ADDC(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *SUBB(fs_reg dst, fs_reg src0, fs_reg src1);
   fs_inst *SEL(fs_reg dst, fs_reg src0, fs_reg src1);

   int type_size(const struct glsl_type *type);
   fs_inst *get_instruction_generating_reg(fs_inst *start,
					   fs_inst *end,
					   fs_reg reg);

   exec_list VARYING_PULL_CONSTANT_LOAD(fs_reg dst, fs_reg surf_index,
                                        fs_reg varying_offset,
                                        uint32_t const_offset);

   bool run();
   void assign_binding_table_offsets();
   void setup_payload_gen4();
   void setup_payload_gen6();
   void assign_curb_setup();
   void calculate_urb_setup();
   void assign_urb_setup();
   bool assign_regs(bool allow_spilling);
   void assign_regs_trivial();
   void get_used_mrfs(bool *mrf_used);
   void setup_payload_interference(struct ra_graph *g, int payload_reg_count,
                                   int first_payload_node);
   void setup_mrf_hack_interference(struct ra_graph *g,
                                    int first_mrf_hack_node);
   int choose_spill_reg(struct ra_graph *g);
   void spill_reg(int spill_reg);
   void split_virtual_grfs();
   void compact_virtual_grfs();
   void move_uniform_array_access_to_pull_constants();
   void setup_pull_constants();
   void invalidate_live_intervals();
   void calculate_live_intervals();
   void calculate_register_pressure();
   bool opt_algebraic();
   bool opt_cse();
   bool opt_cse_local(bblock_t *block, exec_list *aeb);
   bool opt_copy_propagate();
   bool try_copy_propagate(fs_inst *inst, int arg, acp_entry *entry);
   bool try_constant_propagate(fs_inst *inst, acp_entry *entry);
   bool opt_copy_propagate_local(void *mem_ctx, bblock_t *block,
                                 exec_list *acp);
   bool register_coalesce();
   bool compute_to_mrf();
   bool dead_code_eliminate();
   bool dead_code_eliminate_local();
   bool remove_dead_constants();
   bool remove_duplicate_mrf_writes();
   bool virtual_grf_interferes(int a, int b);
   void schedule_instructions(instruction_scheduler_mode mode);
   void insert_gen4_send_dependency_workarounds();
   void insert_gen4_pre_send_dependency_workarounds(fs_inst *inst);
   void insert_gen4_post_send_dependency_workarounds(fs_inst *inst);
   void fail(const char *msg, ...);
   void lower_uniform_pull_constant_loads();

   void push_force_uncompressed();
   void pop_force_uncompressed();

   void emit_dummy_fs();
   fs_reg *emit_fragcoord_interpolation(ir_variable *ir);
   fs_inst *emit_linterp(const fs_reg &attr, const fs_reg &interp,
                         glsl_interp_qualifier interpolation_mode,
                         bool is_centroid, bool is_sample);
   fs_reg *emit_frontfacing_interpolation(ir_variable *ir);
   fs_reg *emit_samplepos_setup(ir_variable *ir);
   fs_reg *emit_sampleid_setup(ir_variable *ir);
   fs_reg *emit_samplemaskin_setup(ir_variable *ir);
   fs_reg *emit_general_interpolation(ir_variable *ir);
   void emit_interpolation_setup_gen4();
   void emit_interpolation_setup_gen6();
   void compute_sample_position(fs_reg dst, fs_reg int_sample_pos);
   fs_reg rescale_texcoord(ir_texture *ir, fs_reg coordinate,
                           bool is_rect, int sampler, int texunit);
   fs_inst *emit_texture_gen4(ir_texture *ir, fs_reg dst, fs_reg coordinate,
			      fs_reg shadow_comp, fs_reg lod, fs_reg lod2);
   fs_inst *emit_texture_gen5(ir_texture *ir, fs_reg dst, fs_reg coordinate,
                              fs_reg shadow_comp, fs_reg lod, fs_reg lod2,
                              fs_reg sample_index);
   fs_inst *emit_texture_gen7(ir_texture *ir, fs_reg dst, fs_reg coordinate,
                              fs_reg shadow_comp, fs_reg lod, fs_reg lod2,
                              fs_reg sample_index, fs_reg mcs);
   fs_reg emit_mcs_fetch(ir_texture *ir, fs_reg coordinate, int sampler);
   fs_reg fix_math_operand(fs_reg src);
   fs_inst *emit_math(enum opcode op, fs_reg dst, fs_reg src0);
   fs_inst *emit_math(enum opcode op, fs_reg dst, fs_reg src0, fs_reg src1);
   void emit_lrp(fs_reg dst, fs_reg x, fs_reg y, fs_reg a);
   void emit_minmax(uint32_t conditionalmod, fs_reg dst,
                    fs_reg src0, fs_reg src1);
   bool try_emit_saturate(ir_expression *ir);
   bool try_emit_mad(ir_expression *ir, int mul_arg);
   void try_replace_with_sel();
   bool opt_peephole_sel();
   bool opt_peephole_predicated_break();
   void emit_bool_to_cond_code(ir_rvalue *condition);
   void emit_if_gen6(ir_if *ir);
   void emit_unspill(fs_inst *inst, fs_reg reg, uint32_t spill_offset,
                     int count);

   void emit_fragment_program_code();
   void setup_fp_regs();
   fs_reg get_fp_src_reg(const prog_src_register *src);
   fs_reg get_fp_dst_reg(const prog_dst_register *dst);
   void emit_fp_alu1(enum opcode opcode,
                     const struct prog_instruction *fpi,
                     fs_reg dst, fs_reg src);
   void emit_fp_alu2(enum opcode opcode,
                     const struct prog_instruction *fpi,
                     fs_reg dst, fs_reg src0, fs_reg src1);
   void emit_fp_scalar_write(const struct prog_instruction *fpi,
                             fs_reg dst, fs_reg src);
   void emit_fp_scalar_math(enum opcode opcode,
                            const struct prog_instruction *fpi,
                            fs_reg dst, fs_reg src);

   void emit_fp_minmax(const struct prog_instruction *fpi,
                       fs_reg dst, fs_reg src0, fs_reg src1);

   void emit_fp_sop(uint32_t conditional_mod,
                    const struct prog_instruction *fpi,
                    fs_reg dst, fs_reg src0, fs_reg src1, fs_reg one);

   void emit_color_write(int target, int index, int first_color_mrf);
   void emit_alpha_test();
   void emit_fb_writes();

   void emit_shader_time_begin();
   void emit_shader_time_end();
   void emit_shader_time_write(enum shader_time_shader_type type,
                               fs_reg value);

   void emit_untyped_atomic(unsigned atomic_op, unsigned surf_index,
                            fs_reg dst, fs_reg offset, fs_reg src0,
                            fs_reg src1);

   void emit_untyped_surface_read(unsigned surf_index, fs_reg dst,
                                  fs_reg offset);

   bool try_rewrite_rhs_to_dst(ir_assignment *ir,
			       fs_reg dst,
			       fs_reg src,
			       fs_inst *pre_rhs_inst,
			       fs_inst *last_rhs_inst);
   void emit_assignment_writes(fs_reg &l, fs_reg &r,
			       const glsl_type *type, bool predicated);
   void resolve_ud_negate(fs_reg *reg);
   void resolve_bool_comparison(ir_rvalue *rvalue, fs_reg *reg);

   fs_reg get_timestamp();

   struct brw_reg interp_reg(int location, int channel);
   void setup_uniform_values(ir_variable *ir);
   void setup_builtin_uniform_values(ir_variable *ir);
   int implied_mrf_writes(fs_inst *inst);

   virtual void dump_instructions();
   void dump_instruction(backend_instruction *inst);

   void visit_atomic_counter_intrinsic(ir_call *ir);

   struct gl_fragment_program *fp;
   struct brw_wm_compile *c;
   unsigned int sanity_param_count;

   int param_size[MAX_UNIFORMS * 4];

   int *virtual_grf_sizes;
   int virtual_grf_count;
   int virtual_grf_array_size;
   int *virtual_grf_start;
   int *virtual_grf_end;
   brw::fs_live_variables *live_intervals;

   int *regs_live_at_ip;

   /* This is the map from UNIFORM hw_reg + reg_offset as generated by
    * the visitor to the packed uniform number after
    * remove_dead_constants() that represents the actual uploaded
    * uniform index.
    */
   int *params_remap;
   int nr_params_remap;

   struct hash_table *variable_ht;
   fs_reg frag_depth;
   fs_reg sample_mask;
   fs_reg outputs[BRW_MAX_DRAW_BUFFERS];
   unsigned output_components[BRW_MAX_DRAW_BUFFERS];
   fs_reg dual_src_output;
   int first_non_payload_grf;
   /** Either BRW_MAX_GRF or GEN7_MRF_HACK_START */
   int max_grf;

   fs_reg *fp_temp_regs;
   fs_reg *fp_input_regs;

   /** @{ debug annotation info */
   const char *current_annotation;
   const void *base_ir;
   /** @} */

   bool failed;
   char *fail_msg;

   /* Result of last visit() method. */
   fs_reg result;

   fs_reg pixel_x;
   fs_reg pixel_y;
   fs_reg wpos_w;
   fs_reg pixel_w;
   fs_reg delta_x[BRW_WM_BARYCENTRIC_INTERP_MODE_COUNT];
   fs_reg delta_y[BRW_WM_BARYCENTRIC_INTERP_MODE_COUNT];
   fs_reg shader_start_time;

   int grf_used;
   bool spilled_any_registers;

   const unsigned dispatch_width; /**< 8 or 16 */

   int force_uncompressed_stack;
};

/**
 * The fragment shader code generator.
 *
 * Translates FS IR to actual i965 assembly code.
 */
class fs_generator
{
public:
   fs_generator(struct brw_context *brw,
                struct brw_wm_compile *c,
                struct gl_shader_program *prog,
                struct gl_fragment_program *fp,
                bool dual_source_output);
   ~fs_generator();

   const unsigned *generate_assembly(exec_list *simd8_instructions,
                                     exec_list *simd16_instructions,
                                     unsigned *assembly_size);

private:
   void generate_code(exec_list *instructions);
   void generate_fb_write(fs_inst *inst);
   void generate_pixel_xy(struct brw_reg dst, bool is_x);
   void generate_linterp(fs_inst *inst, struct brw_reg dst,
			 struct brw_reg *src);
   void generate_tex(fs_inst *inst, struct brw_reg dst, struct brw_reg src);
   void generate_math1_gen7(fs_inst *inst,
			    struct brw_reg dst,
			    struct brw_reg src);
   void generate_math2_gen7(fs_inst *inst,
			    struct brw_reg dst,
			    struct brw_reg src0,
			    struct brw_reg src1);
   void generate_math1_gen6(fs_inst *inst,
			    struct brw_reg dst,
			    struct brw_reg src);
   void generate_math2_gen6(fs_inst *inst,
			    struct brw_reg dst,
			    struct brw_reg src0,
			    struct brw_reg src1);
   void generate_math_gen4(fs_inst *inst,
			   struct brw_reg dst,
			   struct brw_reg src);
   void generate_math_g45(fs_inst *inst,
			  struct brw_reg dst,
			  struct brw_reg src);
   void generate_ddx(fs_inst *inst, struct brw_reg dst, struct brw_reg src);
   void generate_ddy(fs_inst *inst, struct brw_reg dst, struct brw_reg src,
                     bool negate_value);
   void generate_scratch_write(fs_inst *inst, struct brw_reg src);
   void generate_scratch_read(fs_inst *inst, struct brw_reg dst);
   void generate_scratch_read_gen7(fs_inst *inst, struct brw_reg dst);
   void generate_uniform_pull_constant_load(fs_inst *inst, struct brw_reg dst,
                                            struct brw_reg index,
                                            struct brw_reg offset);
   void generate_uniform_pull_constant_load_gen7(fs_inst *inst,
                                                 struct brw_reg dst,
                                                 struct brw_reg surf_index,
                                                 struct brw_reg offset);
   void generate_varying_pull_constant_load(fs_inst *inst, struct brw_reg dst,
                                            struct brw_reg index,
                                            struct brw_reg offset);
   void generate_varying_pull_constant_load_gen7(fs_inst *inst,
                                                 struct brw_reg dst,
                                                 struct brw_reg index,
                                                 struct brw_reg offset);
   void generate_mov_dispatch_to_flags(fs_inst *inst);

   void generate_set_omask(fs_inst *inst,
                           struct brw_reg dst,
                           struct brw_reg sample_mask);

   void generate_set_sample_id(fs_inst *inst,
                               struct brw_reg dst,
                               struct brw_reg src0,
                               struct brw_reg src1);

   void generate_set_simd4x2_offset(fs_inst *inst,
                                    struct brw_reg dst,
                                    struct brw_reg offset);
   void generate_discard_jump(fs_inst *inst);

   void generate_pack_half_2x16_split(fs_inst *inst,
                                      struct brw_reg dst,
                                      struct brw_reg x,
                                      struct brw_reg y);
   void generate_unpack_half_2x16_split(fs_inst *inst,
                                        struct brw_reg dst,
                                        struct brw_reg src);

   void generate_shader_time_add(fs_inst *inst,
                                 struct brw_reg payload,
                                 struct brw_reg offset,
                                 struct brw_reg value);

   void generate_untyped_atomic(fs_inst *inst,
                                struct brw_reg dst,
                                struct brw_reg atomic_op,
                                struct brw_reg surf_index);

   void generate_untyped_surface_read(fs_inst *inst,
                                      struct brw_reg dst,
                                      struct brw_reg surf_index);

   void mark_surface_used(unsigned surf_index);

   void patch_discard_jumps_to_fb_writes();

   struct brw_context *brw;
   struct gl_context *ctx;

   struct brw_compile *p;
   struct brw_wm_compile *c;

   struct gl_shader_program *prog;
   struct gl_shader *shader;
   const struct gl_fragment_program *fp;

   unsigned dispatch_width; /**< 8 or 16 */

   exec_list discard_halt_patches;
   bool dual_source_output;
   void *mem_ctx;
};

/**
 * The fragment shader code generator.
 *
 * Translates FS IR to actual i965 assembly code.
 */
class gen8_fs_generator : public gen8_generator
{
public:
   gen8_fs_generator(struct brw_context *brw,
                     struct brw_wm_compile *c,
                     struct gl_shader_program *prog,
                     struct gl_fragment_program *fp,
                     bool dual_source_output);
   ~gen8_fs_generator();

   const unsigned *generate_assembly(exec_list *simd8_instructions,
                                     exec_list *simd16_instructions,
                                     unsigned *assembly_size);

private:
   void generate_code(exec_list *instructions);
   void generate_fb_write(fs_inst *inst);
   void generate_linterp(fs_inst *inst, struct brw_reg dst,
                         struct brw_reg *src);
   void generate_tex(fs_inst *inst, struct brw_reg dst, struct brw_reg src);
   void generate_math1(fs_inst *inst, struct brw_reg dst, struct brw_reg src);
   void generate_math2(fs_inst *inst, struct brw_reg dst,
                       struct brw_reg src0, struct brw_reg src1);
   void generate_ddx(fs_inst *inst, struct brw_reg dst, struct brw_reg src);
   void generate_ddy(fs_inst *inst, struct brw_reg dst, struct brw_reg src,
                     bool negate_value);
   void generate_scratch_write(fs_inst *inst, struct brw_reg src);
   void generate_scratch_read(fs_inst *inst, struct brw_reg dst);
   void generate_scratch_read_gen7(fs_inst *inst, struct brw_reg dst);
   void generate_uniform_pull_constant_load(fs_inst *inst,
                                            struct brw_reg dst,
                                            struct brw_reg index,
                                            struct brw_reg offset);
   void generate_varying_pull_constant_load(fs_inst *inst,
                                            struct brw_reg dst,
                                            struct brw_reg index,
                                            struct brw_reg offset);
   void generate_mov_dispatch_to_flags(fs_inst *ir);
   void generate_set_simd4x2_offset(fs_inst *ir,
                                    struct brw_reg dst,
                                    struct brw_reg offset);
   void generate_discard_jump(fs_inst *ir);

   void patch_discard_jumps_to_fb_writes();

   void mark_surface_used(unsigned surf_index);

   struct brw_wm_compile *c;
   const struct gl_fragment_program *fp;

   unsigned dispatch_width; /** 8 or 16 */

   bool dual_source_output;

   exec_list discard_halt_patches;
};

bool brw_do_channel_expressions(struct exec_list *instructions);
bool brw_do_vector_splitting(struct exec_list *instructions);
bool brw_fs_precompile(struct gl_context *ctx, struct gl_shader_program *prog);

struct brw_reg brw_reg_from_fs_reg(fs_reg *reg);
