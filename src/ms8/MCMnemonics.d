(* Ulm's Modula-2 Compiler    Solaris 2.x/SPARCv8
   Copyright (C) 1983-1996 Universitaet Ulm, SAI, 89069 Ulm, Germany
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
   $Id: MCMnemonics.d,v 0.1 1997/02/21 18:40:05 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCMnemonics.d,v $
   Revision 0.1  1997/02/21  18:40:05  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCMnemonics; (* AFB 10/87 *)

   TYPE
      Mnemonic = (
	 (* regular instructions without
	    - privileged instructions
	    - copressor instructions
	 *)
	 LDSB, LDSH, LDUB, LDUH, LD, LDD,
	 LDF, LDDF, LDFSR,
	 STB, STH, ST, STD,
	 STF, STDF, STFSR,
	 LDSTUB, SWAP,
	 SETHI, NOP,
	 ANDop, ANDN, ORop, ORN, XOR, XNOR,
	 ANDcc, ANDNcc, ORcc, ORNcc, XORcc, XNORcc,
	 SLL, SRL, SRA,
	 ADD, ADDcc, ADDX, ADDXcc, TADDcc, TADDccTV,
	 SUB, SUBcc, SUBX, SUBXcc, TSUBcc, TSUBccTV,
	 MULScc,
	 UMUL, UMULcc, SMUL, SMULcc,
	 UDIV, UDIVcc, SDIV, SDIVcc,
	 SAVE, RESTORE,
	 BA, BN, BNE, BE, BG, BLE, BGE, BL, BGU, BLEU, BCC, BCS,
	 BPOS, BNEG, BVC, BVS,
	 FBA, FBN, FBU, FBG, FBUG, FBL, FBUL, FBLG, FBNE, FBE,
	 FBUE, FBGE, FBUGE, FBLE, FBULE, FBO,
	 CALL, JMPL,
	 TA, TN, TNE, TE, TG, TLE, TGE, TL, TGU, TLEU, TCC, TCS,
	 TPOS, TNEG, TVC, TVS,
	 RDY, WRY,
	 STBAR, UNIMP, FLUSH,
	 FiTOs, FiTOd, FiTOq,
	 FsTOi, FdTOi, FqTOi,
	 FsTOd, FsTOq, FdTOs, FdTOq, FqTOs, FqTOd,
	 FMOVs, FNEGs, FABSs,
	 FSQRTs, FSQRTd, FSQRTq,
	 FADDs, FADDd, FADDq, FSUBs, FSUBd, FSUBq,
	 FMULs, FMULd, FMULq, FsMULd, FsMULq, FDIVs, FDIVd, FDIVq,
	 FCMPs, FCMPd, FCMPq, FCMPEs, FCMPEd, FCMPEq,
	 (* synthetic instructions *)
	 CMP, JMP, TST, RET, RETL,
	 SETop, NOTop, NEG, INCop, INCcc, DECop, DECcc,
	 BTST, BSET, BCLR, BTOG,
	 CLR, CLRB, CLRH,
	 MOV,
	 (* internal instructions which must be used in combination
	    of appropriate attributes
	 *)
	 LOAD, STORE, FiTO, FSQRT, FADD, FSUB, FMUL, FDIV, FCMP, FCMPE
      );
      MnemRange = [MIN(Mnemonic)..MAX(Mnemonic)];
      MnemString = ARRAY [0..8] OF CHAR;
      MnemSet = SET OF Mnemonic;

   CONST
      genericMnemonics = MnemSet{LOAD..FCMPE};

   VAR
      Mnem: ARRAY MnemRange OF MnemString;

END MCMnemonics.
