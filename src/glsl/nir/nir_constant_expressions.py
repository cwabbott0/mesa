#! /usr/bin/env python
#
# Copyright (C) 2014 Connor Abbott
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice (including the next
# paragraph) shall be included in all copies or substantial portions of the
# Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.
#
# Authors:
#    Connor Abbott (cwabbott0@gmail.com)

from nir_opcodes import opcodes
from mako.template import Template

# the const_expr string for each opcode has a few shortcuts - most only have
# an expression, and the "dst = (expr);" is implied. In addition,
# per-component inputs and outputs are referenced without any subscripts, so
# we need to create the implicit for-loop for per-component opcodes. In
# addition, we need to expand out the src0, src1, etc. with actual sources
# with the appropriate type using the union.

def wr(string, wrap_bool):
   if wrap_bool:
      return "((" + string + ") ? NIR_TRUE : NIR_FALSE)"
   return string

class Operand(object):
   def __init__(self, name, type_, is_src):
      if type_ == "bool" or type_ == "unsigned":
         prefix = "u"
      elif type_ == "int":
         prefix = "i"
      else:
         prefix = "f"

      wrap_bool = is_src and type_ == "bool"

      def wr(string, wrap=wrap_bool):
         if wrap:
            return "(" + string + " != NIR_FALSE)"
         return string

      self.name = wr(name + "." + prefix + "[_i]")
      self.x = wr(name + "." + prefix + "[0]")
      self.y = wr(name + "." + prefix + "[1]")
      self.z = wr(name + "." + prefix + "[2]")
      self.w = wr(name + "." + prefix + "[3]")

   def __str__(self):
      return self.name

def expand_constexpr(opcode):
   const_expr = opcode.const_expr

   if "dst" not in const_expr:
      if opcode.output_type == "bool":
         # For convenience, insert the conversion to unsigned.
         # Note that we don't do this for things that aren't expressions.
         const_expr = "(" + const_expr + ") ? NIR_TRUE : NIR_FALSE"

      if opcode.output_size == 0:
         const_expr = "{dst} = " + const_expr + ";"
      else:
         # for non-per-component opcodes, assume we broadcast to all components
         const_expr = "\n".join(
               "{dst." + "xyzw"[i] + "} = " + const_expr + ";"
                     for i in range(opcode.output_size))

   replacement_dict = {
      "src" + str(i) : Operand("src[" + str(i) + "]", opcode.input_types[i], True)
         for i in range(opcode.num_inputs)
   }

   replacement_dict["dst"] = Operand("dst", opcode.output_type, False)

   const_expr = const_expr.format(**replacement_dict)

   if opcode.output_size == 0:
      const_expr = "for (unsigned _i = 0; _i < num_components; _i++) {" + const_expr + "}"

   return const_expr


const_exprs = {name : expand_constexpr(opcode)
                 for name, opcode in opcodes.iteritems()}

template = Template("""
#include <math.h>
#include "main/core.h"
#include "nir_constant_expressions.h"

#if defined(_MSC_VER) && (_MSC_VER < 1800)
static int isnormal(double x)
{
   return _fpclass(x) == _FPCLASS_NN || _fpclass(x) == _FPCLASS_PN;
}
#elif defined(__SUNPRO_CC)
#include <ieeefp.h>
static int isnormal(double x)
{
   return fpclass(x) == FP_NORMAL;
}
#endif

#if defined(_MSC_VER)
static double copysign(double x, double y)
{
   return _copysign(x, y);
}
#endif

/**
 * Evaluate one component of packSnorm4x8.
 */
static uint8_t
pack_snorm_1x8(float x)
{
    /* From section 8.4 of the GLSL 4.30 spec:
     *
     *    packSnorm4x8
     *    ------------
     *    The conversion for component c of v to fixed point is done as
     *    follows:
     *
     *      packSnorm4x8: round(clamp(c, -1, +1) * 127.0)
     *
     * We must first cast the float to an int, because casting a negative
     * float to a uint is undefined.
     */
   return (uint8_t) (int8_t)
          _mesa_round_to_even(CLAMP(x, -1.0f, +1.0f) * 127.0f);
}

/**
 * Evaluate one component of packSnorm2x16.
 */
static uint16_t
pack_snorm_1x16(float x)
{
    /* From section 8.4 of the GLSL ES 3.00 spec:
     *
     *    packSnorm2x16
     *    -------------
     *    The conversion for component c of v to fixed point is done as
     *    follows:
     *
     *      packSnorm2x16: round(clamp(c, -1, +1) * 32767.0)
     *
     * We must first cast the float to an int, because casting a negative
     * float to a uint is undefined.
     */
   return (uint16_t) (int16_t)
          _mesa_round_to_even(CLAMP(x, -1.0f, +1.0f) * 32767.0f);
}

/**
 * Evaluate one component of unpackSnorm4x8.
 */
static float
unpack_snorm_1x8(uint8_t u)
{
    /* From section 8.4 of the GLSL 4.30 spec:
     *
     *    unpackSnorm4x8
     *    --------------
     *    The conversion for unpacked fixed-point value f to floating point is
     *    done as follows:
     *
     *       unpackSnorm4x8: clamp(f / 127.0, -1, +1)
     */
   return CLAMP((int8_t) u / 127.0f, -1.0f, +1.0f);
}

/**
 * Evaluate one component of unpackSnorm2x16.
 */
static float
unpack_snorm_1x16(uint16_t u)
{
    /* From section 8.4 of the GLSL ES 3.00 spec:
     *
     *    unpackSnorm2x16
     *    ---------------
     *    The conversion for unpacked fixed-point value f to floating point is
     *    done as follows:
     *
     *       unpackSnorm2x16: clamp(f / 32767.0, -1, +1)
     */
   return CLAMP((int16_t) u / 32767.0f, -1.0f, +1.0f);
}

/**
 * Evaluate one component packUnorm4x8.
 */
static uint8_t
pack_unorm_1x8(float x)
{
    /* From section 8.4 of the GLSL 4.30 spec:
     *
     *    packUnorm4x8
     *    ------------
     *    The conversion for component c of v to fixed point is done as
     *    follows:
     *
     *       packUnorm4x8: round(clamp(c, 0, +1) * 255.0)
     */
   return (uint8_t) _mesa_round_to_even(CLAMP(x, 0.0f, 1.0f) * 255.0f);
}

/**
 * Evaluate one component packUnorm2x16.
 */
static uint16_t
pack_unorm_1x16(float x)
{
    /* From section 8.4 of the GLSL ES 3.00 spec:
     *
     *    packUnorm2x16
     *    -------------
     *    The conversion for component c of v to fixed point is done as
     *    follows:
     *
     *       packUnorm2x16: round(clamp(c, 0, +1) * 65535.0)
     */
   return (uint16_t) _mesa_round_to_even(CLAMP(x, 0.0f, 1.0f) * 65535.0f);
}

/**
 * Evaluate one component of unpackUnorm4x8.
 */
static float
unpack_unorm_1x8(uint8_t u)
{
    /* From section 8.4 of the GLSL 4.30 spec:
     *
     *    unpackUnorm4x8
     *    --------------
     *    The conversion for unpacked fixed-point value f to floating point is
     *    done as follows:
     *
     *       unpackUnorm4x8: f / 255.0
     */
   return (float) u / 255.0f;
}

/**
 * Evaluate one component of unpackUnorm2x16.
 */
static float
unpack_unorm_1x16(uint16_t u)
{
    /* From section 8.4 of the GLSL ES 3.00 spec:
     *
     *    unpackUnorm2x16
     *    ---------------
     *    The conversion for unpacked fixed-point value f to floating point is
     *    done as follows:
     *
     *       unpackUnorm2x16: f / 65535.0
     */
   return (float) u / 65535.0f;
}

/**
 * Evaluate one component of packHalf2x16.
 */
static uint16_t
pack_half_1x16(float x)
{
   return _mesa_float_to_half(x);
}

/**
 * Evaluate one component of unpackHalf2x16.
 */
static float
unpack_half_1x16(uint16_t u)
{
   return _mesa_half_to_float(u);
}

nir_const_value
nir_eval_const_opcode(nir_op op, unsigned num_components,
                      nir_const_value *src)
{
   nir_const_value dst = {
      .u = {0, 0, 0, 0}
   };

   switch (op) {
% for name, const_expr in sorted(const_exprs.iteritems()):
   case nir_op_${name}: {
      ${const_expr}
      break;
   }
% endfor
   case nir_num_opcodes: unreachable("shouldn't get here");
   }

   return dst;
}
""")

print template.render(const_exprs=const_exprs)

