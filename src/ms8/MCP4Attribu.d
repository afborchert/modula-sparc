(* Ulm's Modula-2 Compiler    Solaris 2.x/SPARCv8
   -- partially derived from ETH Zurichs Modula-2 Compiler for Lilith --
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
   $Id: MCP4Attribu.d,v 0.1 1997/02/21 18:40:11 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Attribu.d,v $
   Revision 0.1  1997/02/21  18:40:11  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP4Attributes; (* AFB 3/96 *)

   (* data structures of the code generator *)

   FROM MCBase IMPORT Constval, Symbol, Offset, Size, Type, Ident, Label;

   TYPE
      Reg = (
	 g0, g1, g2, g3, g4, g5, g6, g7,
	 o0, o1, o2, o3, o4, o5, o6, o7,
	 l0, l1, l2, l3, l4, l5, l6, l7,
	 i0, i1, i2, i3, i4, i5, i6, i7,
	 y,
	 noReg,
	 (* FPU *)
	  f0,  f1,  f2,  f3,  f4,  f5,  f6,  f7,
	  f8,  f9, f10, f11, f12, f13, f14, f15,
	 f16, f17, f18, f19, f20, f21, f22, f23,
	 f24, f25, f26, f27, f28, f29, f30, f31,
	 fsr
      );
      RegSet = SET OF Reg;
   CONST
      top = o6;       (* stack pointer *)
      base = i6;      (* pointer to current stack frame *)
      return = i7;    (* holds return address *)
      intrval = i0;   (* integer RETURN values are to be stored here *)
      fprval = f0;    (* REAL RETURN values are to be stored here *)
      global = g5;    (* pointer to current module (modtab + global vars) *)
      tmpreg = g1;    (* may be used for temporary values at any time *)
   TYPE
      ArithmeticType = (
	 intAT, cardAT,
	 float32AT, float64AT, float128AT,
	 bitAT, logAT);
      ArithmeticTypeSet = SET OF ArithmeticType;
      FloatType = [float32AT..float128AT];
      TestType = (lt, le, eq, ne, ge, gt, always, never);

   TYPE
      Attribute = POINTER TO AttrRec;
      AtMode = (constAt, typeAt, varAt, procAt,
		refAt, selectAt, indexAt, fieldAt,
		binaryAt, unaryAt, callAt,
                simm13Mode, regMode, addrMode, indexMode, condMode, cmpMode,
		floatRegMode);
      AtModeSet = SET OF AtMode;
      Simm13 = Offset[-4096..4095]; (* valid range for simm13 fields *)
      AttrRec =
	 RECORD
	    link: Attribute; (* parameter list *)
	    attype: Type; atip: Ident;
	    isaddr: BOOLEAN; (* LoadParams *)
	    CASE mode: AtMode OF
	    | callAt:               firstparam: Attribute; (* param list *)
				    procat: Attribute;
	    | fieldAt:              flevel: CARDINAL;
	    | refAt, selectAt, indexAt:
				    desat: Attribute; (* designator *)
				    CASE (* mode *) : AtMode OF
				    | indexAt:  indexat: Attribute;
				    | selectAt: field: Ident;
				    END;
	    | unaryAt, binaryAt:    opsy: Symbol;
				    rightop: Attribute;
				    CASE (* mode *) : AtMode OF
				    | binaryAt: leftop: Attribute;
				    END;
	    | regMode, addrMode, indexMode, floatRegMode:
				    reg: Reg;
				    CASE (* mode *) : AtMode OF
				    | addrMode, indexMode:
					  tmp: BOOLEAN; (* on stack *)
					  stackoffset: Offset;
					  CASE (* mode *) : AtMode OF
					  | indexMode:
						(* only one of the labels
						   may be given;
						   the label is to be
						   enclosed in %L()
						*)
						labelip: Ident;
						rlabel: Label;
						addr: Simm13;
						xreg: Reg; (* may be noReg *)
					  END;
				    END;
	    | simm13Mode:           simm13val: Simm13;
	    | condMode, cmpMode:    test: TestType;
				    atype: ArithmeticType;
				    CASE (* mode *) : AtMode OF
				    | cmpMode:
					  (* reg2 & cmpval must not be
					     used simultaneously
					  *)
					  reg1, reg2: Reg;
					  cmpval: Simm13;
				    | condMode:
					  tlabel: Label; (* branch on TRUE *)
					  flabel: Label; (* branch on FALSE *)
				    END;
	    ELSE
	       CASE (* mode *) : AtMode OF
	       | constAt:  cval: Constval;
	       END;
	    END;
	 END;

   CONST
      IdModes = AtModeSet{constAt..procAt}; (* attributes with atip-field *)
      DesignatorModes = AtModeSet{varAt, indexAt, selectAt, refAt};
      GenModes = AtModeSet{regMode, addrMode, indexMode,
			   floatRegMode, condMode};
      OpModes = AtModeSet{binaryAt, unaryAt, callAt, constAt, procAt} +
		DesignatorModes + GenModes;  (* attributes with attype-field *)

   TYPE
      CaseLabel = INTEGER;
      Labels = POINTER TO LabelsRec;
      LabelList = POINTER TO LabelRec;
      LabelsRec =
	 RECORD
	    min, max: CaseLabel;
	    count: CARDINAL; (* number of cases *)
	    ltype: Type; (* type of labels *)
	    head, tail: LabelList;
	 END;
      LabelRec =
	 RECORD
	    low, high: CaseLabel;
	    case: CARDINAL; (* each case has a unique number *)
	    link: LabelList;
	 END;

   VAR (* set by CodeGen *)
      procedureAt: Attribute; (* set by `ProcedureBegin' *)

   PROCEDURE GetLabel(VAR l: Label);
      (* get label with unique `n1'; head = 0C and n2 = 0 *)

   PROCEDURE NewAttribute(VAR at: Attribute);
      (* NEW(at) *)

   PROCEDURE DisposeAttribute(VAR at: Attribute);
      (* DISPOSE(at); at := NIL; *)

   PROCEDURE Align(VAR offset: Offset);

END MCP4Attributes.
