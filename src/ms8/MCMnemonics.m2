(* Ulm's Modula-2 Compiler    Solaris 2.x/SPARCv8
   Copyright (C) 1983-1996 Universitaet Ulm SAI 89069 Ulm Germany
   ----------------------------------------------------------------------------
   Modula-2 has been designed and developed by Niklaus Wirth
   at the Institut fuer Informatik, ETH Zuerich, Switzerland
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Compiler is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   Ulm's Modula-2 Compiler is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   $Id: MCMnemonics.m2,v 0.1 1997/02/21 18:40:23 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCMnemonics.m2,v $
   Revision 0.1  1997/02/21  18:40:23  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCMnemonics; (* AFB 3/96 *)

BEGIN
   Mnem[LDSB] := "ldsb";
   Mnem[LDSH] := "ldsh";
   Mnem[LDUB] := "ldub";
   Mnem[LDUH] := "lduh";
   Mnem[LD] := "ld";
   Mnem[LDD] := "ldd";
   Mnem[LDF] := "ld";
   Mnem[LDDF] := "ldd";
   Mnem[LDFSR] := "ldfsr";
   Mnem[STB] := "stb";
   Mnem[STH] := "sth";
   Mnem[ST] := "st";
   Mnem[STD] := "std";
   Mnem[STF] := "st";
   Mnem[STDF] := "std";
   Mnem[STFSR] := "stfsr";
   Mnem[LDSTUB] := "ldstub";
   Mnem[SWAP] := "swap";
   Mnem[SETHI] := "sethi";
   Mnem[NOP] := "nop";
   Mnem[ANDop] := "and";
   Mnem[ANDN] := "andn";
   Mnem[ORop] := "or";
   Mnem[ORN] := "orn";
   Mnem[XOR] := "xor";
   Mnem[XNOR] := "xnor";
   Mnem[ANDcc] := "andcc";
   Mnem[ANDNcc] := "andncc";
   Mnem[ORcc] := "orcc";
   Mnem[ORNcc] := "orncc";
   Mnem[XORcc] := "xorcc";
   Mnem[XNORcc] := "xnorcc";
   Mnem[SLL] := "sll";
   Mnem[SRL] := "srl";
   Mnem[SRA] := "sra";
   Mnem[ADD] := "add";
   Mnem[ADDcc] := "addcc";
   Mnem[ADDX] := "addx";
   Mnem[ADDXcc] := "addxcc";
   Mnem[TADDcc] := "taddcc";
   Mnem[TADDccTV] := "taddcctv";
   Mnem[SUB] := "sub";
   Mnem[SUBcc] := "subcc";
   Mnem[SUBX] := "subx";
   Mnem[SUBXcc] := "subxcc";
   Mnem[TSUBcc] := "tsubcc";
   Mnem[TSUBccTV] := "tsubcctv";
   Mnem[MULScc] := "mulscc";
   Mnem[UMUL] := "umul";
   Mnem[UMULcc] := "umulcc";
   Mnem[SMUL] := "smul";
   Mnem[SMULcc] := "smulcc";
   Mnem[UDIV] := "udiv";
   Mnem[UDIVcc] := "udivcc";
   Mnem[SDIV] := "sdiv";
   Mnem[SDIVcc] := "sdivcc";
   Mnem[SAVE] := "save";
   Mnem[RESTORE] := "restore";
   Mnem[BA] := "ba";
   Mnem[BN] := "bn";
   Mnem[BNE] := "bne";
   Mnem[BE] := "be";
   Mnem[BG] := "bg";
   Mnem[BLE] := "ble";
   Mnem[BGE] := "bge";
   Mnem[BL] := "bl";
   Mnem[BGU] := "bgu";
   Mnem[BLEU] := "bleu";
   Mnem[BCC] := "bcc";
   Mnem[BCS] := "bcs";
   Mnem[BPOS] := "bpos";
   Mnem[BNEG] := "bneg";
   Mnem[BVC] := "bvc";
   Mnem[BVS] := "bvs";
   Mnem[FBA] := "fba";
   Mnem[FBN] := "fbn";
   Mnem[FBU] := "fbu";
   Mnem[FBG] := "fbg";
   Mnem[FBUG] := "fbug";
   Mnem[FBL] := "fbl";
   Mnem[FBUL] := "fbul";
   Mnem[FBLG] := "fblg";
   Mnem[FBNE] := "fbne";
   Mnem[FBE] := "fbe";
   Mnem[FBUE] := "fbue";
   Mnem[FBGE] := "fbge";
   Mnem[FBUGE] := "fbuge";
   Mnem[FBLE] := "fble";
   Mnem[FBULE] := "fbule";
   Mnem[FBO] := "fbo";
   Mnem[CALL] := "call";
   Mnem[JMPL] := "jmpl";
   Mnem[TA] := "ta";
   Mnem[TN] := "tn";
   Mnem[TNE] := "tne";
   Mnem[TE] := "te";
   Mnem[TG] := "tg";
   Mnem[TLE] := "tle";
   Mnem[TGE] := "tge";
   Mnem[TL] := "tl";
   Mnem[TGU] := "tgu";
   Mnem[TLEU] := "tleu";
   Mnem[TCC] := "tcc";
   Mnem[TCS] := "tcs";
   Mnem[TPOS] := "tpos";
   Mnem[TNEG] := "tneg";
   Mnem[TVC] := "tvc";
   Mnem[TVS] := "tvs";
   Mnem[RDY] := "rd";
   Mnem[WRY] := "wr";
   Mnem[STBAR] := "stbar";
   Mnem[UNIMP] := "unimp";
   Mnem[FLUSH] := "flush";
   Mnem[FiTOs] := "fitos";
   Mnem[FiTOd] := "fitod";
   Mnem[FiTOq] := "fitoq";
   Mnem[FsTOi] := "fstoi";
   Mnem[FdTOi] := "fdtoi";
   Mnem[FqTOi] := "fqtoi";
   Mnem[FsTOd] := "fstod";
   Mnem[FsTOq] := "fstoq";
   Mnem[FdTOs] := "fdtos";
   Mnem[FdTOq] := "fdtoq";
   Mnem[FqTOs] := "fqtos";
   Mnem[FqTOd] := "fqtod";
   Mnem[FMOVs] := "fmovs";
   Mnem[FNEGs] := "fnegs";
   Mnem[FABSs] := "fabss";
   Mnem[FSQRTs] := "fsqrts";
   Mnem[FSQRTd] := "fsqrtd";
   Mnem[FSQRTq] := "fsqrtq";
   Mnem[FADDs] := "fadds";
   Mnem[FADDd] := "faddd";
   Mnem[FADDq] := "faddq";
   Mnem[FSUBs] := "fsubs";
   Mnem[FSUBd] := "fsubd";
   Mnem[FSUBq] := "fsubq";
   Mnem[FMULs] := "fmuls";
   Mnem[FMULd] := "fmuld";
   Mnem[FMULq] := "fmulq";
   Mnem[FsMULd] := "fsmuld";
   Mnem[FsMULq] := "fsmulq";
   Mnem[FDIVs] := "fdivs";
   Mnem[FDIVd] := "fdivd";
   Mnem[FDIVq] := "fdivq";
   Mnem[FCMPs] := "fcmps";
   Mnem[FCMPd] := "fcmpd";
   Mnem[FCMPq] := "fcmpq";
   Mnem[FCMPEs] := "fcmpes";
   Mnem[FCMPEd] := "fcmped";
   Mnem[FCMPEq] := "fcmpeq";
   Mnem[CMP] := "cmp";
   Mnem[JMP] := "jmp";
   Mnem[TST] := "tst";
   Mnem[RET] := "ret";
   Mnem[RETL] := "retl";
   Mnem[SETop] := "set";
   Mnem[NOTop] := "not";
   Mnem[NEG] := "neg";
   Mnem[INCop] := "inc";
   Mnem[INCcc] := "inccc";
   Mnem[DECop] := "dec";
   Mnem[DECcc] := "deccc";
   Mnem[BTST] := "btst";
   Mnem[BSET] := "bset";
   Mnem[BCLR] := "bclr";
   Mnem[BTOG] := "btog";
   Mnem[CLR] := "clr";
   Mnem[CLRB] := "clrb";
   Mnem[CLRH] := "clrh";
   Mnem[MOV] := "mov";

   Mnem[LOAD] := "ld";
   Mnem[STORE] := "st";
   Mnem[FiTO] := "fito";
   Mnem[FSQRT] := "fsqrt";
   Mnem[FADD] := "fadd";
   Mnem[FSUB] := "fsub";
   Mnem[FMUL] := "fmul";
   Mnem[FDIV] := "fdiv";
   Mnem[FCMP] := "fcmp";
   Mnem[FCMPE] := "fcmpe";
END MCMnemonics.
