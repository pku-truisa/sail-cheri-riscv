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

let cap_max_addr = MAX(cap_addr_width)

type CapAddrBits  = bits(cap_addr_width)
type CapAddrInt   = range(0, (2 ^ cap_addr_width) - 1)
type CapLenBits   = bits(cap_len_width)
type CapLen       = range(0, (2 ^ cap_len_width))
type CapFlagsBits = bits(cap_flags_width)

/* Reset E and T calculated to make top 2**xlen. */
let cap_max_E   = cap_len_width + 1
let cap_reset_E = to_bits(cap_E_width, cap_max_E)

/*! A partially decompressed version of a capability -- E, B, T,
 * lenMSB, sealed and otype fields are not present in all formats and are
 * populated by capBitsToCapability.
 */
struct Capability = {
  tag                    : bool,
  flag_cap_mode          : bool,
  E                      : bits(cap_E_width),
  address                : bits(cap_addr_width)
}

let null_cap : Capability = struct {
  tag                    = false,
  flag_cap_mode          = false,
  E                      = cap_reset_E,
  address                = zeros()
}

let default_cap : Capability = struct {
  tag                    = true,
  flag_cap_mode          = false,
  E                      = cap_reset_E,
  address                = zeros()
}

/* Convert from capability bits (128 bits with tag) to a more convenient struct. */
function encCapabilityToCapability(t,c) : (bool, EncCapability) -> Capability = {
  return struct {
    tag                    = t,
    flag_cap_mode          = bit_to_bool(c.flags[0]),
    E                      = c.E
    address                = c.address
  }
}

function capBitsToCapability(t, c) : (bool, CapBits) -> Capability = encCapabilityToCapability(t, capBitsToEncCapability(c))

function capToEncCap(cap) : Capability -> EncCapability = {
  return struct {
    flags = bool_to_bits(cap.flag_cap_mode),
    E = cap.E;
    address = cap.address
  };
}

/* Convert from capability struct to bits (no tag) */
function capToBits(cap) : Capability -> CapBits = encCapToBits(capToEncCap(cap))

/* When saving/restoring capabilities xor them with bits of null_cap --
 * this ensures that canonical null_cap is always all-zeros in memory
 * even though it may have bits set logically (e.g. length or exponent
 */

let null_cap_bits : CapBits = capToBits(null_cap)

function capToMemBits(cap) : Capability -> CapBits =
  capToBits(cap) ^ null_cap_bits

function memBitsToCapability(tag, b) : (bool, CapBits) -> Capability =
  capBitsToCapability(tag, b ^ null_cap_bits)

function getCapBoundsBits(c) : Capability -> (CapAddrBits, CapLenBits) =
  let E = min(cap_max_E, unsigned(c.E)) in
  top = base + pow2(E) - 1;
  (base[(cap_addr_width - 1)..0], top)
  }

function getCapBounds(cap) : Capability -> (CapAddrInt, CapLen) =
  let (base, top) = getCapBoundsBits(cap) in
  (unsigned(base), unsigned(top))

/* An 'ideal' version of setCapBounds as described in paper. */
function setCapBounds(cap, base, top) : (Capability, CapAddrBits, CapLenBits) -> (bool, Capability) = {
  /* {cap with base=base; length=(bits(64)) length; offset=0} */
  let ext_base = 0b0 @ base;
  let length = top - ext_base;
  /* Find an exponent that will put the most significant bit of length
   * second from the top as assumed during decoding. We ignore the bottom
   * MW bits because those are handled by the ie = 0 format.
   */
  let e = cap_max_E - count_leading_zeros(length[cap_addr_width..0]); // No mantissa

  let newCap = {cap with address=base, E=to_bits(6, e)};
  let exact = not(lostSignificantBase | lostSignificantTop);
  (exact, newCap)
}

/*!
 * Gets the architecture specific capability flags for given capability.
 */
val getCapFlags : Capability -> CapFlagsBits
function getCapFlags(cap) = bool_to_bits(cap.flag_cap_mode)

/*!
 * THIS`(cap, flags)` sets the architecture specific capability flags on `cap`
 * to `flags` and returns the result as new capability.
 */
val setCapFlags : (Capability, CapFlagsBits) -> Capability
function setCapFlags(cap, flags) = {cap with flag_cap_mode = bit_to_bool(flags[0])}

function getCapBaseBits(c) : Capability -> CapAddrBits =
    let (base, _) = getCapBoundsBits(c) in
    base

function getCapBase(c) : Capability -> CapAddrInt =
    unsigned(getCapBaseBits(c))

function getCapTopBits(c) : Capability -> CapLenBits =
    let (_, top) = getCapBoundsBits(c) in
    top

function getCapTop (c) : Capability -> CapLen =
    unsigned(getCapTopBits(c))

function getCapOffsetBits(c) : Capability -> CapAddrBits =
    let base : CapAddrBits = getCapBaseBits(c) in
    c.address - base

function getCapOffset(c) : Capability -> CapAddrInt =
    unsigned(getCapOffsetBits(c))

function getCapLength(c) : Capability -> CapLen =
    let ('base, 'top) = getCapBounds(c) in {
        /* For valid capabilties we expect top >= base and hence
         * length >= 0 but representation does allow top < base in some
         * cases so might encounter on untagged capabilities. Here we just
         * pretend it is a 65-bit quantitiy and wrap.
         */
        assert (not(c.tag) | top >= base);
        (top - base) % pow2(cap_len_width)
    }

val inCapBounds  : (Capability, CapAddrBits, CapLen) -> bool
function inCapBounds (cap, addr, size) = {
  let (base, top) = getCapBounds(cap);
  let a = unsigned(addr);
  (a >= base) & ((a + size) <= top)
}

function getCapCursor(cap) : Capability -> CapAddrInt = unsigned(cap.address)

function int_to_cap (offset) : CapAddrBits -> Capability =
    {null_cap with address = offset}

function clearTagIf(cap, cond) : (Capability, bool) -> Capability =
    {cap with tag = cap.tag & not(cond)}


function clearTag(cap) : Capability -> Capability =
    {cap with tag = false}

function capBoundsEqual (c1, c2) : (Capability, Capability) -> bool =
    let (base1, top1) = getCapBounds(c1) in
    let (base2, top2) = getCapBounds(c2) in
    (base1 == base2) & (top1 == top2)

val setCapAddr : (Capability, CapAddrBits) -> (bool, Capability)
function setCapAddr(c, addr) =
    let newCap = { c with address = addr } in
    let representable = capBoundsEqual(c, newCap) in
    (representable, newCap)

function setCapAddrChecked(c, addr) : (Capability, CapAddrBits) -> Capability =
    let (representable, newCap) = setCapAddr(c, addr) in
    clearTagIf(newCap, not(representable) | isCapSealed(c))

infix 1 >>_s
overload operator >> = {sail_shiftright}
overload operator << = {sail_shiftleft}
overload operator >>_s = {sail_arith_shiftright}

function fastRepCheck(c, i) : (Capability, CapAddrBits) -> bool=
    let E = unsigned(c.E) in
    if (E >= cap_max_E - 2) then
        true /* in this case representable region is whole address space */
    else
        let i_top    = signed(i >>_s (E + cap_mantissa_width)) in
        let i_mid    = truncate(i >> E, cap_mantissa_width)in
        let a_mid    = truncate(c.address >> E, cap_mantissa_width) in
        let B3 = truncateLSB(c.B, 3) in
        let R3 = B3 - 0b001 in
        let R  = R3 @ zeros(cap_mantissa_width - 3) in
        let diff  = R - a_mid in
        let diff1 = diff - 1 in
        /* i_top determines 1. whether the increment is inRange
           i.e. less than the size of the representable region
           (2**(E+MW)) and 2. whether it is positive or negative. To
           satisfy 1. all top bits must be the same so we are
           interested in the cases i_top is 0 or -1 */
        if (i_top == 0) then
          i_mid <_u diff1
        else if (i_top == -1) then
          (i_mid >=_u diff) & (R != a_mid)
        else
          false

function setCapOffset(c, offset) : (Capability, CapAddrBits) -> (bool, Capability) =
    let base = getCapBaseBits(c) in
    let newAddress = base + offset in
    let newCap     = { c with address = newAddress } in
    /* let representable = capBoundsEqual(c, newCap) in */
    let representable = fastRepCheck(c, (newAddress - c.address)) in
    (representable, newCap)

function setCapOffsetChecked (cap, offset) : (Capability, CapAddrBits) -> Capability =
    let (representable, newCap) = setCapOffset(cap, offset) in
    clearTagIf(newCap, not(representable) | isCapSealed(cap))

function incCapOffset(c, delta) : (Capability, CapAddrBits) -> (bool, Capability) =
    let newAddress : CapAddrBits = c.address + delta in
    let newCap     = { c with address = newAddress } in
    /* let representable = capBoundsEqual(c, newCap) in */
    let representable = fastRepCheck(c, delta) in
    (representable, newCap)


val capToString : (Capability) -> string effect {escape}
function capToString (cap) = {
  let len = getCapLength(cap);
  let len_str = BitStr(to_bits(cap_len_width + 3, len));
  /* Print architectural type which is -1 for unsealed caps */
  concat_str(" t:",
  concat_str(if cap.tag then "1" else "0",
  concat_str(" address:",
  concat_str(BitStr(cap.address),
  concat_str(" base:",
  concat_str(BitStr(getCapBaseBits(cap)),
  concat_str(" length:", len_str)))))))
}

function getRepresentableAlignmentMask(len) : xlenbits -> xlenbits = {
  ones(sizeof(xlen))
}

function getRepresentableLength(len) : xlenbits -> xlenbits = {
  let m = getRepresentableAlignmentMask(len);
  (len + ~(m)) & m
}

/**
 * Returns an integer program counter from a given capability.
 * By default this is equivalent to reading the address field of the capability,
 * but if PCC relocation is enabled (not part of standardized CHERI-RISC-V) it
 * returns the capability offset instead.
 */
function cap_to_integer_pc (cap: Capability) -> xlenbits = {
  if (have_pcc_relocation()) then getCapOffsetBits(cap) else cap.address
}

/**
 * Updates a capability to reference an integer program counter.
 * By default this is equivalent to updating the address field of the capability,
 * but if PCC relocation is enabled (not part of standardized CHERI-RISC-V) it
 * updates the capability offset instead.
 */
function update_cap_with_integer_pc (cap: Capability, pc: xlenbits) -> Capability = {
  if (have_pcc_relocation()) then setCapOffsetChecked(cap, pc) else setCapAddrChecked(cap, pc)
}
