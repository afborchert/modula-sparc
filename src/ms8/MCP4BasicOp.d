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
   $Id: MCP4BasicOp.d,v 0.2 1998/04/23 18:04:24 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4BasicOp.d,v $
   Revision 0.2  1998/04/23  18:04:24  borchert
   RestrictedRelease has been extended by a third parameter that
   decides whether stack reservations are to be freed or not

   Revision 0.1  1997/02/21  18:40:11  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP4BasicOps; (* AFB 3/96 *)

   (* generate basic operations for common use of 4th pass;
      every procedure may generate code and is authorized to
      modify `at' but not `at^.attype'
      `at' and everything in connection with `at' must be valid
   *)

   FROM MCBase IMPORT Offset, Size, Ident, Type, Label;
   FROM MCP4Attributes IMPORT ArithmeticType, Attribute, Reg, TestType;

   VAR
      ccat: Attribute;
	 (* this variable is accessed by any code-generating routine
	    and set to the attribute which reflects the current
	    condition codes;
	    it allows to decide whether a test-instruction is necessary
	    or not;
	    ccat should be set to NIL on generation of
	    cc-destroying instructions
	 *)

   PROCEDURE LowHigh(off: Offset; VAR low, high: Offset);
   PROCEDURE High(off: Offset) : Offset;
   PROCEDURE Low(off: Offset) : Offset;

   PROCEDURE LoadConst(reg: Reg; const: Offset);

   PROCEDURE CreateConstAt(const: Offset; VAR at: Attribute);

   PROCEDURE IsStringConst(at: Attribute) : BOOLEAN;

   PROCEDURE ConvertCharConst(at: Attribute);
      (* convert character array constants into char constants;
	 PRE:  at^.mode = constAt
	 POST: at^.mode = simm13Mode
      *)

   PROCEDURE Address(at: Attribute);
      (* convert `at' and generate code such that
	 `at' is a valid addressing mode for LD and similar instructions
	 and can be given as %a-argument to Emit;
	 `at.mode' must be a leaf-node, i.e. not unaryAt,...
      *)

   PROCEDURE DynArray(at: Attribute);
      (* PRE:  at must be in varAt;
         POST: at is set so that both, the pointer to the array contents
	       and the high value, are accessible;
	       note that regMode may be returned -- in this case
	       the high value may be found in reg+1
      *)

   PROCEDURE DynArrayHigh(at: Attribute);
   PROCEDURE DynArraySize(at: Attribute);

   PROCEDURE AlignReg(srcReg, destReg: Reg);
      (* align contents of `reg' *)

   PROCEDURE Min(reg1, reg2: Reg);
      (* return unsigned minimum of reg1 and reg2 in reg1 *)

   PROCEDURE Max(reg1, reg2: Reg);
      (* return unsigned maximum of reg1 and reg2 in reg1 *)

   PROCEDURE Load(at: Attribute);
      (* load the corresponding value of `at' into a register;
	 `at' must not be a record or an array
      *)

   PROCEDURE LoadReg(at: Attribute; r: Reg);
      (* like Load with destination register `r' *)

   PROCEDURE LoadAddr(at: Attribute);
      (* load address of `at' into a register and
	 convert mode of `at' into addrMode
      *)

   PROCEDURE LoadAddrReg(at: Attribute; r: Reg);
      (* like LoadAddr with destination register `r' *)

   PROCEDURE LoadCond(at: Attribute);
      (* set condition codes of `at';
	 at is in condMode afterwards and ccat equals at
      *)

   PROCEDURE OntoStack(at: Attribute);
      (* put the value onto stack if it is not addressable yet *)

   PROCEDURE AllocOnStack(type: Type; VAR at: Attribute);
      (* return stack allocation represented by at
         which is sufficient to hold a value of the given type
      *)

   PROCEDURE Convert(at: Attribute; dtype: Type);
      (* convert `at' to type `dtype'; at^.attype becomes dtype;
	 `at' may be loaded
      *)

   PROCEDURE DereferenceAt(at: Attribute);
      (* dereferencing of `at'; like '^'-operator *)

   PROCEDURE OffsetAt(at: Attribute; offset: Offset);
      (* `at' must have an addressable addressing mode (i.e. not regMode);
	 the address described by `at' is then incremented by `offset';
	 `offset' may be negative
      *)

   PROCEDURE IndexAtReg(at: Attribute; indexreg: Reg; scalefactor: CARDINAL);
      (* generate []-operation for `at' with index in `indexreg'
	 `indexreg' is released afterwards
      *)

   PROCEDURE ReleaseAt(at: Attribute);
      (* release anything (registers and stack reservations) of `at' *)

   PROCEDURE RestrictedRelease(at: Attribute; r: Reg; keepstack: BOOLEAN);
      (* release anything (registers and stack reservations or only
         registers) of `at' but not the given register
      *)

   PROCEDURE ReturnAt(VAR at: Attribute);
      (* release anything (registers and stack reservations) of `at' and
	 dispose `at'
      *)

   PROCEDURE RestrictedReturnAt(VAR at: Attribute; r: Reg);


   PROCEDURE ConstMulReg(atype: ArithmeticType; r: Reg; value: INTEGER);
   PROCEDURE ConstDivReg(atype: ArithmeticType; r: Reg; value: INTEGER);
   PROCEDURE ConstModReg(atype: ArithmeticType; r: Reg; value: INTEGER);

   PROCEDURE Mult(at: ArithmeticType; desat, opat: Attribute);
      (* generate code for `desat' := `desat' * `opat'
	 and release `opat'
	 `desat' is in regMode afterwards
      *)

   PROCEDURE Div(at: ArithmeticType; desat, opat: Attribute);
      (* generate code for `desat' := `desat' DIV `opat'
	 and release `opat'
	 `desat' is in regMode afterwards
      *)

   PROCEDURE Mod(at: ArithmeticType; desat, opat: Attribute);
      (* generate code for `desat' := `desat' MOD `opat'
	 and release `opat'
	 `desat' is in regMode afterwards
      *)

   PROCEDURE InvertTest(VAR t: TestType);
      (* invert test type (NOT), e.g. `le' becomes `gt' *)

   PROCEDURE ReverseTest(VAR t: TestType);
      (* reverse test type (exchange of operands), e.g. `le' becomes `ge' *)

   PROCEDURE GenTest(t: TestType; atype: ArithmeticType; dest: Label);
      (* condition codes are set; generate code for branching
	 to `dest' if `t' is true, i.e. "bcc test,dest";
	 warning: GenTest does not fill the delay slot
      *)

   PROCEDURE SetBool(at: Attribute; destreg: Reg);
      (* set destreg according to condition codes *)

   PROCEDURE MoveBytes(from, to: Reg; nbytes: Size);
      (* addresses are in `from' and `to'; both registers
	 are released afterwards
      *)

   PROCEDURE MoveBytesAt(from, to: Attribute; nbytes: Size);
      (* both attributes are released afterwards *)

   PROCEDURE MoveVarBytes(from, to, nbytes: Reg);
      (* like MoveBytes but with variable size;
	 nbytes must be a multiple of oneword;
         all registers are released afterwards
      *)

   PROCEDURE LoadPosParams;

   PROCEDURE SignedRangeCheck(at: Attribute; lowerBound, upperBound: INTEGER);
      (* at^.mode = regMode;
	 check at^.reg for being inside [lowerBound..upperBound]
      *)

   PROCEDURE UnsignedRangeCheck(at: Attribute;
                                lowerBound, upperBound: CARDINAL);
      (* at^.mode = regMode;
	 check at^.reg for being inside [lowerBound..upperBound]
      *)

   PROCEDURE DynArrayCheck(indexat, highat: Attribute);
      (* check indexat for being inside [0..highat] *)

   PROCEDURE ConversionCheck(at: Attribute);
      (* at^.attype = intptr or cardptr
	 check at for being convertable from INTEGER to CARDINAL
	 or vice versa
      *)

   PROCEDURE CheckAgainst(at: Attribute; type: Type);
      (* check that at can be safely converted to type *)

   PROCEDURE GenReturn;

END MCP4BasicOps.
