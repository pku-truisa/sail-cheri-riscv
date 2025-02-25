/*=======================================================================================*/
/*  CHERI RISCV Sail Model                                                               */
/*                                                                                       */
/*  This CHERI Sail RISC-V architecture model here, comprising all files and             */
/*  directories except for the snapshots of the Lem and Sail libraries in the            */
/*  prover_snapshots directory (which include copies of their licenses), is subject      */
/*  to the BSD two-clause licence below.                                                 */
/*                                                                                       */
/*  Copyright (c) 2017-2021                                                              */
/*    Alasdair Armstrong                                                                 */
/*    Thomas Bauereiss                                                                   */
/*    Brian Campbell                                                                     */
/*    Jessica Clarke                                                                     */
/*    Nathaniel Wesley Filardo (contributions prior to July 2020, thereafter Microsoft)  */
/*    Alexandre Joannou                                                                  */
/*    Microsoft                                                                          */
/*    Prashanth Mundkur                                                                  */
/*    Robert Norton-Wright (contributions prior to March 2020, thereafter Microsoft)     */
/*    Alexander Richardson                                                               */
/*    Peter Rugg                                                                         */
/*    Peter Sewell                                                                       */
/*                                                                                       */
/*  All rights reserved.                                                                 */
/*                                                                                       */
/*  This software was developed by SRI International and the University of               */
/*  Cambridge Computer Laboratory (Department of Computer Science and                    */
/*  Technology) under DARPA/AFRL contract FA8650-18-C-7809 ("CIFV"), and                 */
/*  under DARPA contract HR0011-18-C-0016 ("ECATS") as part of the DARPA                 */
/*  SSITH research programme.                                                            */
/*                                                                                       */
/*  This software was developed within the Rigorous Engineering of                       */
/*  Mainstream Systems (REMS) project, partly funded by EPSRC grant                      */
/*  EP/K008528/1, at the Universities of Cambridge and Edinburgh.                        */
/*                                                                                       */
/*  This project has received funding from the European Research Council                 */
/*  (ERC) under the European Union’s Horizon 2020 research and innovation                */
/*  programme (grant agreement 789108, ELVER).                                           */
/*                                                                                       */
/*  Redistribution and use in source and binary forms, with or without                   */
/*  modification, are permitted provided that the following conditions                   */
/*  are met:                                                                             */
/*  1. Redistributions of source code must retain the above copyright                    */
/*     notice, this list of conditions and the following disclaimer.                     */
/*  2. Redistributions in binary form must reproduce the above copyright                 */
/*     notice, this list of conditions and the following disclaimer in                   */
/*     the documentation and/or other materials provided with the                        */
/*     distribution.                                                                     */
/*                                                                                       */
/*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''                   */
/*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED                    */
/*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                      */
/*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR                  */
/*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                         */
/*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                     */
/*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                     */
/*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND                  */
/*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,                   */
/*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT                   */
/*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF                   */
/*  SUCH DAMAGE.                                                                         */
/*=======================================================================================*/

/* width of capability in bytes (excluding tag) */
type cap_size : Int = 16
let  cap_size = sizeof(cap_size)
type log2_cap_size : Int = 4
let  log2_cap_size = sizeof(log2_cap_size)
type CapBits = bits(8 * cap_size)
/* width of otype field in bits */
type cap_hperms_width : Int = 12
let  cap_hperms_width = sizeof(cap_hperms_width)
type cap_uperms_width : Int = 4
let  cap_uperms_width = sizeof(cap_uperms_width)
type cap_otype_width : Int = 18
let  cap_otype_width = sizeof(cap_otype_width)
type cap_reserved_width : Int = 2
let  cap_reserved_width = sizeof(cap_reserved_width)
type cap_flags_width : Int = 1
let  cap_flags_width = sizeof(cap_flags_width)
type cap_mantissa_width : Int = 14
let  cap_mantissa_width = sizeof(cap_mantissa_width)
type cap_E_width : Int = 6
let  cap_E_width = sizeof(cap_E_width)
type cap_addr_width : Int = xlen
let  cap_addr_width = sizeof(cap_addr_width)
type cap_len_width : Int = cap_addr_width + 1
let  cap_len_width = sizeof(cap_len_width)

/* For CLoadTags and CClearTags, which operate at cache line granularity */
type caps_per_cache_line : Int = 4
let  caps_per_cache_line = sizeof(caps_per_cache_line)

type internal_E_take_bits : Int = 3
let  internal_E_take_bits = sizeof(internal_E_take_bits)

/* Capability format completely compressed. Tag is out of band */
struct EncCapability = {
  perms      : bits(cap_uperms_width + cap_hperms_width),
  otype      : bits(cap_otype_width),
  reserved   : bits(cap_reserved_width),
  flags      : bits(cap_flags_width),
  internal_E : bit,
  T          : bits(cap_mantissa_width - 2),
  B          : bits(cap_mantissa_width),
  address    : bits(cap_addr_width)
}

function capBitsToEncCapability(c) : CapBits -> EncCapability = struct {
  perms      = c[127..112],
  reserved   = c[111..110],
  flags      = c[109..109],
  otype      = c[108..91],
  internal_E = c[90],
  T          = c[89..78],
  B          = c[77..64],
  address    = c[63..0]
}

function encCapToBits(cap) : EncCapability -> CapBits =
  cap.perms @
  cap.reserved @
  cap.flags @
  cap.otype @
  [cap.internal_E] @
  cap.T @
  cap.B @
  cap.address
