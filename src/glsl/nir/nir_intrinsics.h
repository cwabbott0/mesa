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

/**
 * This header file defines all the available intrinsics in one place. It
 * expands to a list of macros of the form:
 *
 * INTRINSIC(name, num_srcs, src_components, has_dest, dest_components,
 * 	     num_variables, num_indices, flags)
 *
 * Which should correspond one-to-one with the nir_intrinsic_info structure. It
 * is included in both ir.h to create the nir_intrinsic enum (with members of
 * the form nir_intrinsic_(name)) and and in opcodes.c to create
 * nir_intrinsic_infos, which is a const array of nir_intrinsic_info structures
 * for each intrinsic.
 */

#define ARR(...) { __VA_ARGS__ }


INTRINSIC(load_var_vec1,   0, ARR(), true, 1, 1, 0,
	  NIR_INTRINSIC_CAN_ELIMINATE)
INTRINSIC(load_var_vec2,   0, ARR(), true, 2, 1, 0,
	  NIR_INTRINSIC_CAN_ELIMINATE)
INTRINSIC(load_var_vec3,   0, ARR(), true, 3, 1, 0,
	  NIR_INTRINSIC_CAN_ELIMINATE)
INTRINSIC(load_var_vec4,   0, ARR(), true, 4, 1, 0,
	  NIR_INTRINSIC_CAN_ELIMINATE)
INTRINSIC(store_var_vec1, 1, ARR(1), false, 0, 1, 0, 0)
INTRINSIC(store_var_vec2, 1, ARR(2), false, 0, 1, 0, 0)
INTRINSIC(store_var_vec3, 1, ARR(3), false, 0, 1, 0, 0)
INTRINSIC(store_var_vec4, 1, ARR(4), false, 0, 1, 0, 0)
INTRINSIC(copy_var,       0, ARR(),  false, 0, 2, 0, 0)

/*
 * a barrier is an intrinsic with no inputs/outputs but which can't be moved
 * around/optimized in general
 */
#define BARRIER(name) INTRINSIC(name, 0, ARR(), false, 0, 0, 0, 0)

BARRIER(discard)

INTRINSIC(emit_vertex,   0, ARR(), false, 0, 0, 1, 0)
INTRINSIC(end_primitive, 0, ARR(), false, 0, 0, 1, 0)

/*
 * Atomic counters
 *
 * The *_var variants take an atomic_uint nir_variable, while the other,
 * lowered, variants take a constant buffer index and register offset.
 */

#define ATOMIC(name, flags) \
   INTRINSIC(atomic_counter_##name##_var, 0, ARR(), true, 1, 1, 0, flags) \
   INTRINSIC(atomic_counter_##name, 1, ARR(1), true, 1, 0, 1, flags)

ATOMIC(inc, 0)
ATOMIC(dec, 0)
ATOMIC(read, NIR_INTRINSIC_CAN_ELIMINATE)

#define SYSTEM_VALUE(name, components) \
   INTRINSIC(load_##name, 0, ARR(), true, components, 0, 0, \
   NIR_INTRINSIC_CAN_ELIMINATE | NIR_INTRINSIC_CAN_REORDER)

SYSTEM_VALUE(front_face, 1)
SYSTEM_VALUE(vertex_id, 1)
SYSTEM_VALUE(instance_id, 1)
SYSTEM_VALUE(sample_id, 1)
SYSTEM_VALUE(sample_pos, 2)
SYSTEM_VALUE(sample_mask_in, 1)
SYSTEM_VALUE(invocation_id, 1)

#define LOAD(name, num_indices, flags) \
   INTRINSIC(load_##name##_vec1, 0, ARR(), true, 1, 0, num_indices, \
	     NIR_INTRINSIC_CAN_ELIMINATE | flags) \
   INTRINSIC(load_##name##_vec2, 0, ARR(), true, 2, 0, num_indices, \
	     NIR_INTRINSIC_CAN_ELIMINATE | flags) \
   INTRINSIC(load_##name##_vec3, 0, ARR(), true, 3, 0, num_indices, \
	     NIR_INTRINSIC_CAN_ELIMINATE | flags) \
   INTRINSIC(load_##name##_vec4, 0, ARR(), true, 4, 0, num_indices, \
	     NIR_INTRINSIC_CAN_ELIMINATE | flags) \
   INTRINSIC(load_##name##_vec1_indirect, 1, ARR(1), true, 1, 0, num_indices, \
	     NIR_INTRINSIC_CAN_ELIMINATE | flags) \
   INTRINSIC(load_##name##_vec2_indirect, 1, ARR(1), true, 2, 0, num_indices, \
	     NIR_INTRINSIC_CAN_ELIMINATE | flags) \
   INTRINSIC(load_##name##_vec3_indirect, 1, ARR(1), true, 3, 0, num_indices, \
	     NIR_INTRINSIC_CAN_ELIMINATE | flags) \
   INTRINSIC(load_##name##_vec4_indirect, 1, ARR(1), true, 4, 0, num_indices, \
	     NIR_INTRINSIC_CAN_ELIMINATE | flags) \


/*
 * The first index is the address to load from, and the second index is the
 * number of array elements to load. For UBO's (and SSBO's), the first index
 * is the UBO buffer index (TODO nonconstant UBO buffer index) and the second
 * and third indices play the role of the first and second indices in the other
 * loads. Indirect loads have an additional register input, which is added
 * to the constant address to compute the final address to load from.
 * 
 * For vector backends, the address is in terms of one vec4, and so each array
 * element is +4 scalar components from the previous array element. For scalar
 * backends, the address is in terms of a single 4-byte float/int and arrays
 * elements begin immediately after the previous array element.
 */

LOAD(uniform, 2, NIR_INTRINSIC_CAN_REORDER)
LOAD(ubo, 3, NIR_INTRINSIC_CAN_REORDER)
LOAD(input, 2, NIR_INTRINSIC_CAN_REORDER)
/* LOAD(ssbo, 2, 0) */

#define STORE(name, num_indices, flags) \
   INTRINSIC(store_##name##_vec1, 1, ARR(1), false, 0, 0, num_indices, flags) \
   INTRINSIC(store_##name##_vec2, 1, ARR(2), false, 0, 0, num_indices, flags) \
   INTRINSIC(store_##name##_vec3, 1, ARR(3), false, 0, 0, num_indices, flags) \
   INTRINSIC(store_##name##_vec4, 1, ARR(4), false, 0, 0, num_indices, flags) \
   INTRINSIC(store_##name##_vec1_indirect, 2, ARR(1, 1), false, 0, 0, \
	     num_indices, flags) \
   INTRINSIC(store_##name##_vec2_indirect, 2, ARR(2, 1), false, 0, 0, \
	     num_indices, flags) \
   INTRINSIC(store_##name##_vec3_indirect, 2, ARR(3, 1), false, 0, 0, \
	     num_indices, flags) \
   INTRINSIC(store_##name##_vec4_indirect, 2, ARR(4, 1), false, 0, 0, \
	     num_indices, flags) \

/*
 * Stores work the same way as loads, except now the first register input is
 * the value or array to store and the optional second input is the indirect
 * offset.
 */

STORE(output, 2, 0)
/* STORE(ssbo, 3, 0) */

LAST_INTRINSIC(store_output_vec4_indirect)
