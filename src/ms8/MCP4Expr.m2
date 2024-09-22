(* Ulm's Modula-2 Compiler    Solaris 11/SPARCv8
   Copyright (C) 1983-2024 Andreas F. Borchert
   ----------------------------------------------------------------------------
   Modula-2 has been designed and developed by Niklaus Wirth
   at the Institut fuer Informatik, ETH Zuerich, Switzerland
   ----------------------------------------------------------------------------
   Ulm Modula-2 Compiler is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   Ulm Modula-2 Compiler is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4Expr; (* AFB 3/96 *)

   FROM MCBase IMPORT boolptr, Stptr, Symbol, Type, Idclass, Structform,
      SymSet, BitsPerWord, bitsetptr, realptr, longrealptr, Idset, Label,
      oneword, doubleword, byteptr, onebyte, Varkind, Ident, wordptr,
      Constval, Offset, Size, intptr, Stpures, Stfuncs, procmarkspace,
      addrptr, cardptr, charptr;
   FROM MCBigSet IMPORT InitConstSet, TermConstSet, ConstSetElement;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Attributes IMPORT NewAttribute, Attribute, Reg, AtMode,
      TestType, GetLabel, tmpreg, ArithmeticType, fprval, AtModeSet,
      base, top;
   FROM MCP4BasicOps IMPORT LoadCond, InvertTest, UnsignedRangeCheck,
      ReleaseAt, ReturnAt, LoadConst, ccat, Address, RestrictedRelease,
      RestrictedReturnAt, DynArray, Convert, Load, LoadAddrReg, LoadReg,
      DynArraySize, LowHigh, OntoStack, OffsetAt, DereferenceAt,
      IndexAtReg, GenTest, LoadAddr, Mult, Div, Mod, ConstMulReg,
      ConstDivReg, ConstModReg, ReverseTest, AllocOnStack,
      CreateConstAt, ConvertCharConst, IsStringConst, MoveBytes,
      DynArrayHigh, LoadPosParams, CheckAgainst, DynArrayCheck;
   FROM MCP4Blocks IMPORT level;
   FROM MCP4CodeSys IMPORT EmitLabel, Emit, Emit1, Emit2, Emit3, Emit4,
      StrEmit, StrEmit1, StrEmit2;
   FROM MCP4Global IMPORT Assert, Error;
   FROM MCP4Register IMPORT GetReg, FreeReg, Reserved, GetFloatReg,
      SaveRegs, RestoreRegs, ParamReg, FreeParamRegs, SaveParamRegs,
      RestoreParamRegs;
   FROM MCP4Scanner IMPORT GetSymbol, sy, cstPtr, cString, val, nptr,
      controlRangeCheck, arithmeticRangeCheck;
   FROM MCP4Stack IMPORT StackAlloc, StackFree;
   FROM MCP4Statements IMPORT UseWith, WithType;
   FROM MCP4Types IMPORT ResultType, ArithType, IsNumeric, IsSetType,
      IsReal, IsDyn, BaseType;
   FROM Memory IMPORT ALLOCATE, DEALLOCATE;
   FROM Sys IMPORT forksys, exit;
   IMPORT Memory, MCBase, MCP4Stack;

   (* following procedures parse expressions & designators from
      the interpass file and generate an expression tree which
      is represented as an attribute
   *)

   PROCEDURE Unary(VAR at: Attribute; type: Type;
                   op: Attribute; opsymbol: Symbol);
   BEGIN
      NewAttribute(at);
      WITH at^ DO
	 link := NIL; mode := unaryAt; attype := type;
	 opsy := opsymbol; rightop := op;
      END;
   END Unary;

   PROCEDURE Binary(VAR at: Attribute; type: Type;
                    op1, op2: Attribute; opsymbol: Symbol);
   BEGIN
      NewAttribute(at);
      WITH at^ DO
	 link := NIL; mode := binaryAt; attype := type;
	 opsy := opsymbol; leftop := op1; rightop := op2;
      END;
   END Binary;

   PROCEDURE ParseDesignator(VAR at: Attribute);

      VAR
	 dat: Attribute;
	 dtype: Type;

      PROCEDURE PreDesignator(VAR at: Attribute);
	 VAR
	    exprat: Attribute;
	    skip: BOOLEAN;
      BEGIN
	 skip := TRUE;
	 NewAttribute(at);
	 WITH at^ DO
	    link := NIL;
            IF sy = namesy THEN
               WITH nptr^ DO
                  attype := idtyp; atip := nptr;
                  IF klass = vars THEN
		     mode := varAt;
                  ELSIF (klass = pures) OR (klass = funcs) OR
			(klass = mods) THEN
		     mode := procAt;
                  ELSE (* klass = types *)
                     Assert(klass = types);
                     GetSymbol;
                     IF sy = lparent THEN
			GetSymbol;
			ParseExpression(exprat);
			mode := callAt;
			firstparam := exprat; exprat^.link := NIL;
			procat := NIL;
		     ELSE
			mode := typeAt; skip := FALSE;
                     END;
                  END;
               END;
            ELSE (* sy = field *)
               Assert(sy = MCBase.field);
	       attype := NIL; atip := NIL;
	       mode := fieldAt;
	       flevel := val;
	       WithType(flevel, attype);
            END;
         END;
         IF skip THEN
            GetSymbol;
         END;
      END PreDesignator;

   BEGIN (* ParseDesignator *)
      PreDesignator(at);
      WHILE (sy = lbrack) OR (sy = arrow) OR (sy = period) DO
	 dtype := at^.attype;
	 NewAttribute(dat);
	 WITH dat^ DO
	    link := NIL;
	    IF sy = lbrack THEN
	       mode := indexAt; desat := at;
	       Assert(dtype^.form = arrays);
	       attype := dtype^.elp;
	       GetSymbol;
	       ParseExpression(indexat);
	       Assert(sy = rbrack);
	       GetSymbol; (* rbrack *)
	    ELSIF sy = arrow THEN (* indirection via a pointer *)
	       GetSymbol;
	       mode := refAt; desat := at;
	       IF dtype = addrptr THEN
		  attype := wordptr;
	       ELSE
		  Assert(dtype^.form = pointers);
		  attype := dtype^.elemp;
	       END;
	    ELSE (* record field *)
	       GetSymbol; (* period *)
	       Assert((sy = namesy) & (nptr # NIL) & (nptr^.klass = fields));
	       mode := selectAt; desat := at;
	       Assert(dtype^.form = records);
	       field := nptr; attype := nptr^.idtyp;
	       GetSymbol;
	    END;
	 END;
	 at := dat;
      END;
   END ParseDesignator;

   PROCEDURE ProcFuncCall(VAR at: Attribute; pat: Attribute);
      VAR
	 returnType: Type;
	 head, tail: Attribute;
	 param: Attribute;
	 standardFunction: BOOLEAN;
	 sfname: Stfuncs;
   BEGIN
      standardFunction := FALSE;
      IF pat^.attype # NIL THEN
	 WITH pat^.attype^ DO
	    Assert(form = proctypes);
	    IF rkind = funcs THEN
	       returnType := funcp;
	    ELSE
	       returnType := NIL;
	    END;
	 END;
      ELSE
	 Assert(pat^.atip # NIL);
	 WITH pat^.atip^ DO
	    Assert(klass IN Idset{mods, pures, funcs});
	    IF klass = mods THEN
	       returnType := NIL;
	    ELSE
	       Assert(isstandard);
	       IF klass = pures THEN
		  returnType := NIL;
	       ELSE
		  standardFunction := TRUE;
		  sfname := fname;
	       END;
	    END;
	 END;
      END;

      head := NIL; tail := NIL;
      WHILE sy # rparent DO
	 ParseExpression(param);
	 param^.link := NIL;
	 IF head = NIL THEN
	    head := param;
	 ELSE
	    tail^.link := param;
	 END;
	 tail := param;
	 IF sy = comma THEN
	    GetSymbol; (*comma*)
	 END;
      END;
      GetSymbol; (*rparent*)

      NewAttribute(at);
      WITH at^ DO
	 link := NIL; mode := callAt;
	 firstparam := head; procat := pat;
	 IF standardFunction THEN
	    CASE sfname OF
	    | absf:  returnType := head^.attype;
	    | adrf:  returnType := addrptr;
	    | argcf: returnType := cardptr;
	    | capf:  returnType := charptr;
	    | chrf:  returnType := charptr;
	    | fltf:  returnType := realptr;
	    | higf:  returnType := cardptr;
	    | minf:  returnType := head^.attype;
	    | maxf:  returnType := head^.attype;
	    | oddf:  returnType := boolptr;
	    | ordf:  returnType := cardptr;
	    | sizf:  returnType := cardptr;
	    | sqrf:  returnType := realptr;
	    | trcf:  returnType := intptr;
	    | tszf:  returnType := cardptr;
	    | uxf:   returnType := boolptr;
	    | uxff:  returnType := boolptr;
	    | uxsf:  returnType := boolptr;
	    | valf:  returnType := head^.attype;
	    ELSE
	       Assert(FALSE);
	    END;
	 END;
	 attype := returnType;
      END;
   END ProcFuncCall;

   PROCEDURE Factor(VAR at: Attribute);
      VAR
	 at1: Attribute;

      PROCEDURE SetConstructor(VAR at: Attribute; settype: Type);
         VAR
	    at1, at2: Attribute;

	 PROCEDURE Element(VAR at: Attribute);
	    VAR
	       at1, at2: Attribute;
	 BEGIN
	    ParseExpression(at1);
	    IF sy = range THEN
	       GetSymbol; (* range *)
	       ParseExpression(at2);
	       Binary(at, settype, at1, at2, range);
	    ELSE
	       at := at1;
	    END;
	 END Element;

      BEGIN
         GetSymbol; (* lconbr *)
	 IF sy # rconbr THEN
	    Element(at1);
	    WHILE sy # rconbr DO
	       Element(at2);
	       Binary(at1, settype, at1, at2, comma);
	    END;
	    Binary(at, settype, NIL, at1, comma);
	 ELSE
	    (* supported but not expected because the 3rd pass
	       is expected to do the following
	    *)
	    IF settype^.form = bigsets THEN
	       NewAttribute(at);
	       WITH at^ DO
		  attype := settype; atip := NIL; mode := constAt;
		  InitConstSet(cval, attype);
		  TermConstSet(cval);
	       END;
	    ELSE
	       Assert(settype^.form = sets);
	       CreateConstAt(0, at); at^.attype := settype;
	    END;
	 END;
         GetSymbol; (* rconbr *)
      END SetConstructor;

   BEGIN (* Factor *)
      IF sy = lparent THEN
         GetSymbol;
	 ParseExpression(at);
	 GetSymbol; (* rparent *)
      ELSIF sy = notsy THEN
	 (* at1^.attype = boolptr *)
	 GetSymbol;
	 Factor(at1);
	 Unary(at, boolptr, at1, notsy);
      ELSIF (sy = namesy) OR (sy = field) THEN
         ParseDesignator(at1);
         IF sy = lparent THEN (* function call *)
            GetSymbol;
	    ProcFuncCall(at, at1);
         ELSIF sy = lconbr THEN
	    Assert(at1^.mode = typeAt);
	    SetConstructor(at, at1^.attype);
	    ReturnAt(at1);
	 ELSE
	    at := at1;
         END;
      ELSIF sy = lconbr THEN
         SetConstructor(at, bitsetptr);
      ELSE (* Constant *)
         Assert(sy = anycon);
	 NewAttribute(at);
         WITH at^ DO
	    mode := constAt;
	    IF IsReal(cstPtr) THEN
	       attype := realptr;
	    ELSE
	       attype := BaseType(cstPtr);
	    END;
	    atip := NIL;
	    cval := Constval(cString);
         END;
         GetSymbol;
      END;
   END Factor;

   PROCEDURE Term(VAR at: Attribute);
      VAR
         at1, at2: Attribute;
	 opsy: Symbol;
   BEGIN (* Term *)
      Factor(at1);
      WHILE (sy >= andsy) & (sy <= modsy) DO
	 opsy := sy;
	 GetSymbol;
	 Factor(at2);
	 Binary(at1, ResultType(at1, at2), at1, at2, opsy);
      END;
      at := at1;
   END Term;

   PROCEDURE SimpleExpression(VAR at: Attribute);
      VAR
         opsy: Symbol;
         at1, at2: Attribute;
         negb: BOOLEAN; (* a negation has to take place *)
   BEGIN
      negb := sy = minus;
      IF negb THEN
	 GetSymbol;
      END;
      Term(at1);
      IF negb THEN
	 Unary(at1, at1^.attype, at1, minus);
      END;
      WHILE (sy >= plus) & (sy <= orsy) DO
	 opsy := sy;
         GetSymbol;
         Term(at2);
	 Binary(at1, ResultType(at1, at2), at1, at2, opsy);
      END;
      at := at1;
   END SimpleExpression;

   PROCEDURE ParseExpression(VAR at: Attribute);
      VAR
         opsy : Symbol;
         at1, at2: Attribute; (* descriptor of second operand *)
   BEGIN (* ParseExpression *)
      SimpleExpression(at1);
      IF (sy >= eql) & (sy <= insy) THEN (* relational operator ? *)
         opsy := sy;
         GetSymbol;
         SimpleExpression(at2);
	 Binary(at1, boolptr, at1, at2, opsy);
      END;
      at := at1;
   END ParseExpression;

   PROCEDURE GetTestType(sy: Symbol; VAR testtype: TestType);
   BEGIN
      CASE sy OF
      | lss: testtype := lt;
      | leq: testtype := le;
      | eql: testtype := eq;
      | neq: testtype := ne;
      | geq: testtype := ge;
      | grt: testtype := gt;
      END;
   END GetTestType;

   PROCEDURE LogOp(VAR at: Attribute; VAR trueLab, falseLab: Label);
      (* at^.mode is unaryAt or binaryAt and
	     opsy is one of orsy, andsy, negsy

	 this procedure allows to avoid assembler pseudo-ops
	 which declares a label to be equal to another one;
	 this is necessary for some buggy assemblers

	 post conditions:
	     trueLab and falseLab remain unchanged if ok is set
	     at^.mode = condMode
		 tlabel = trueLab
		 flabel = falseLab
	 remark:
	     both labels are possibly used
      *)
      TYPE
	 SymSet = SET OF Symbol;
      CONST
	 LogOps = SymSet{orsy, andsy, notsy};
      VAR
	 op1, op2: Attribute;
	 operator: Symbol;
	 label: Label; (* label between evaluation of op1 and op2 *)

      PROCEDURE LogAt(at: Attribute) : BOOLEAN;
      BEGIN
	 WITH at^ DO
	    RETURN ((mode = unaryAt) OR (mode = binaryAt)) &
		   (opsy IN LogOps)
	 END;
      END LogAt;

   BEGIN (* LogOp *)
      operator := at^.opsy;
      IF operator = notsy THEN
	 op1 := at^.rightop;
	 IF LogAt(op1) THEN
	    LogOp(op1, falseLab, trueLab);
	 ELSE
	    GenLogExpr(op1, falseLab, trueLab);
	    LoadCond(op1);
	 END;
	 InvertTest(op1^.test);
	 at := op1;
      ELSE
	 WITH at^ DO
	    op1 := leftop;
	    op2 := rightop;
	 END;
	 IF (operator = andsy) & ~falseLab.ok THEN
	    GetLabel(falseLab);
	 ELSIF (operator = orsy) & ~trueLab.ok THEN
	    GetLabel(trueLab);
	 END;
	 label.ok := FALSE;
	 IF LogAt(op1) THEN
	    IF operator = andsy THEN
	       LogOp(op1, label, falseLab);
	    ELSE
	       LogOp(op1, trueLab, label);
	    END;
	 ELSE
	    IF operator = andsy THEN
	       GenLogExpr(op1, label, falseLab);
	    ELSE
	       GenLogExpr(op1, trueLab, label);
	    END;
	    LoadCond(op1);
	 END;
	 WITH op1^ DO
	    IF operator = andsy THEN
	       InvertTest(test);
	       GenTest(test, atype, falseLab); Emit(NOP, "");
	    ELSE
	       GenTest(test, atype, trueLab); Emit(NOP, "");
	    END;
	    IF label.ok THEN
	       EmitLabel(label);
	    END;
	 END;
	 IF LogAt(op2) THEN
	    LogOp(op2, trueLab, falseLab);
	 ELSE
	    GenLogExpr(op2, trueLab, falseLab); LoadCond(op2);
	 END;
	 at := op2;
      END;
      WITH at^ DO
	 (* mode = condMode *)
	 tlabel := trueLab;
	 flabel := falseLab;
      END;
   END LogOp;

   PROCEDURE GenLogExpr(VAR at: Attribute; VAR trueLab, falseLab: Label);
      (* like GenExpr; result is in condMode;
	 if tlabel or flabel are set they equal trueLab and falseLab
      *)
      VAR
	 testtype: TestType;
	 resultType: Type;

      PROCEDURE JoinLabels(VAR extLabel, intLabel: Label);
      BEGIN
	 IF intLabel.ok THEN
	    IF extLabel.ok THEN
	       StrEmit2("%:D%l%:=%l", intLabel, extLabel);
	       intLabel := extLabel;
	    ELSE
	       extLabel := intLabel;
	    END;
	 END;
      END JoinLabels;

   BEGIN
      WITH at^ DO
	 resultType := attype;
	 IF ((mode = unaryAt) OR (mode = binaryAt)) &
	    (opsy IN SymSet{orsy, andsy, notsy}) THEN
	    LogOp(at, trueLab, falseLab);
	 ELSE
	    GenExpr(at);
	    LoadCond(at);
	    WITH at^ DO
	       Assert(mode = condMode);
	       JoinLabels(trueLab, tlabel);
	       JoinLabels(falseLab, flabel);
	    END;
	 END;
      END;
      at^.attype := resultType;
   END GenLogExpr;

   PROCEDURE GenCall(VAR at: Attribute);
      (* in case of functions the function value attribute is
	 returned in `at'
	 else `at' is NIL
      *)
      VAR
	 ptype: Type;
	 word: CARDINAL;
	 numberOfRegs: CARDINAL;

      MODULE StackAllocations;

	 (* collect stack reservations:
	       allocated during parameter loading
	       deallocated after procedure/function call
	 *)

	 FROM MCBase IMPORT Offset, Size;
	 FROM MCP4Stack IMPORT StackFree;
	 FROM Memory IMPORT ALLOCATE, DEALLOCATE;

	 EXPORT StackRes, ReleaseAll;

	 TYPE
	    List = POINTER TO Res;
	    Res =
	       RECORD
		  offset: Offset;
		  size: Size;
		  link: List;
	       END;
	 VAR
	    list: List;

	 PROCEDURE StackRes(offset: Offset; size: Size);
	    VAR new: List;
	 BEGIN
	    NEW(new);
	    new^.offset := offset;
	    new^.size := size;
	    new^.link := list;
	    list := new;
	 END StackRes;

	 PROCEDURE ReleaseAll;
	    VAR old: List;
	 BEGIN
	    WHILE list # NIL DO
	       StackFree(list^.offset, list^.size);
	       old := list;
	       list := list^.link;
	       DISPOSE(old);
	    END;
	 END ReleaseAll;

      BEGIN
	 list := NIL;
      END StackAllocations;

      PROCEDURE Standard(params: Attribute);

	 TYPE
	    ShiftKind = (arithShift, logShift, rotate);
	 VAR
	    param: Attribute;
	    atype: Type; (* type of argument *)
	    newReg: Reg;

	 PROCEDURE NextParam;
	 BEGIN
	    Assert(params # NIL);
	    param := params;
	    IF IsStringConst(param) THEN
	       ConvertCharConst(param);
	    END;
	    atype := param^.attype;
	    params := params^.link;
	 END NextParam;

	 PROCEDURE Next;
	 BEGIN
	    NextParam;
	    GenExpr(param);
	 END Next;

	 PROCEDURE AbsAt(param: Attribute);
	    (* param is in regMode *)
	    VAR
	       newReg: Reg;
	 BEGIN
	    (* code sequence from GNU super optimizer *)
	    Assert(param^.mode = regMode);
	    Emit3(SRA, "%a,%c,%r", param, 31, tmpreg);
	    IF Reserved(param^.reg) THEN
	       GetReg(newReg);
	       Emit3(ADD, "%r,%a,%r", tmpreg, param, newReg);
	       param^.reg := newReg;
	    ELSE
	       Emit3(ADD, "%r,%a,%a", tmpreg, param, param);
	    END;
	    Emit3(XOR, "%a,%r,%a", param, tmpreg, param);
	 END AbsAt;

	 PROCEDURE High;
	    (* param is in varAt *)
	 BEGIN
	    NextParam;
	    Assert(param^.attype^.dyn);
	       (* otherwise it should have been folded *)
	    DynArray(param);
	    WITH param^ DO
	       IF mode = regMode THEN
		  reg := VAL(Reg, ORD(reg) + 1);
	       ELSE
		  OffsetAt(param, oneword);
	       END;
	    END;
	    at := param;
	 END High;

	 PROCEDURE Val;
	    VAR
	       dtype,			(* target type *)
	       stype: Type;		(* operand type *)
	 BEGIN
	    NextParam; dtype := param^.attype;
	    Next; at := param; stype := at^.attype;
	    Load(at);
	    IF arithmeticRangeCheck THEN
	       CheckAgainst(at, dtype);
	    END;
	 END Val;

	 PROCEDURE Store(reg: Reg; VAR at: Attribute);
	    (* used by UnixCall & UnixFork *)
	 BEGIN
	    Address(at);
	    IF at^.mode = regMode THEN
	       Emit3(ORop, "%r,%r,%a", reg, g0, at);
	    ELSE
	       Emit3(STORE, "%r,%a", at^.attype, reg, at);
	    END;
	    ReturnAt(at);
	 END Store;

	 PROCEDURE UnixCall;
	    VAR
	       sysCall: INTEGER;
	       d0at, d1at: Attribute;
	       reg: Reg;
	 BEGIN
	    Next;
	    Assert(param^.mode = simm13Mode); sysCall := param^.simm13val;
	    StrEmit1("%* SYSTEM.UNIXCALL call=%c", sysCall);
	    d0at := params; params := params^.link;
	    d1at := params; params := params^.link;
	    reg := o0;
	    WHILE params # NIL DO
	       NextParam; GenExpr(param);
	       IF reg >= o6 THEN
		  Error(401); reg := o0;
	       END;
	       LoadReg(param, reg); reg := VAL(Reg, ORD(reg) + 1);
	    END;
	    Assert(g1 = tmpreg); (* otherwise we have to save it... *)
	    LoadConst(g1, sysCall);
	    Emit1(TA, "%c", 8); ccat := at;
	    Store(o0, d0at); Store(o1, d1at); Assert(ccat = at);
	    WITH at^ DO
	       mode := condMode;
	       attype := boolptr;
	       test := ge; atype := cardAT;
	       tlabel.ok := FALSE; flabel.ok := FALSE;
	    END;
	 END UnixCall;

	 PROCEDURE UnixFork;
	    VAR
	       endifLab: Label;
	 BEGIN
	    StrEmit("%* SYSTEM.UNIXFORK");
	    (* clear flags parameter of forkx *)
	    LoadConst(o0, 0);
	    LoadConst(o1, 0);
	    LoadConst(g1, forksys); (* extended version of Solaris 11 *)
	    Emit1(TA, "%c", 8); ccat := at;
	    WITH at^ DO
	       attype := boolptr;
	       mode := regMode;
	       GetReg(reg);
	       Emit3(SUBXcc, "%r,%i,%r", g0, -1, reg);
	    END;
	    GetLabel(endifLab); Emit1(BE, "%l", endifLab);
	    Emit3(ORop, "%r,%r,%r", o0, g0, tmpreg);
	    Emit3(SUBcc, "%r,%r,%r", o1, g0, g0);
	    Emit1(BNE, "%A%l", endifLab);
	    Emit3(ORop, "%r,%r,%r", g0, g0, tmpreg);
	    EmitLabel(endifLab);
	    Next; Store(tmpreg, param);
	    ccat := NIL;
	 END UnixFork;

	 PROCEDURE UnixSignal;
	 BEGIN
	    StrEmit("%* SYSTEM.UNIXSIGNAL");
	    Next; LoadReg(param, o0); ReturnAt(param);
	    Next; LoadReg(param, o1); ReturnAt(param);
	    Next; LoadAddrReg(param, o2); ReturnAt(param);
	    Next; LoadAddrReg(param, o3); ReturnAt(param);
	    Emit2(CALL, "%_%s,%c", ".signal", 4);
	    Emit(NOP, "");
	    WITH at^ DO
	       mode := regMode;
	       GetReg(reg);
	       Emit3(ORcc, "%r,%r,%r", o0, g0, reg);
	    END;
	    ccat := at;
	 END UnixSignal;

	 PROCEDURE NewProcess;
	 BEGIN
	    StrEmit("%* SYSTEM.NEWPROCESS");
	    Next; LoadReg(param, o0); ReturnAt(param);
	    Next; LoadReg(param, o1); ReturnAt(param);
	    NextParam;
	    IF (param^.mode = constAt) & (param^.cval.value < 8192) THEN
	       Error(404);
	    END;
	    GenExpr(param);
	    LoadReg(param, o2); ReturnAt(param);
	    Emit2(CALL, "%_%s,%c", ".newprocess", 3);
	    Emit(NOP, "");
	    Next; Address(param);
	    IF param^.mode = regMode THEN
	       Emit3(ORop, "%r,%r,%r", o0, g0, param^.reg);
	    ELSE
	       Emit2(ST, "%r,%a", o0, param);
	    END;
	    ReturnAt(param);
	    at := NIL; ccat := NIL;
	 END NewProcess;

	 PROCEDURE Transfer;
	 BEGIN
	    StrEmit("%* SYSTEM.TRANSFER");
	    Next; LoadAddrReg(param, o0); ReturnAt(param);
	    Next; LoadAddrReg(param, o1); ReturnAt(param);
	    Emit2(CALL, "%_%s,%c", ".transfer", 2);
	    Emit(NOP, "");
	    at := NIL; ccat := NIL;
	 END Transfer;

	 PROCEDURE IncDec(op: Mnemonic; VAR at: Attribute);
	    VAR
	       incdec: Attribute;
	       origType: Type;
	       tmpat: Attribute;
	 BEGIN
	    IF params # NIL THEN
	       Next; incdec := param;
	       Load(incdec);
	    ELSE
	       incdec := NIL;
	    END;
	    Address(at);
	    WITH at^ DO
	       origType := attype;
	       attype := ResultType(at, at);
	       IF mode = regMode THEN
		  IF incdec = NIL THEN
		     Emit3(op, "%r,%c,%r", reg, 1, reg);
		  ELSE
		     Emit3(op, "%r,%a,%r", reg, incdec, reg);
		  END;
		  IF arithmeticRangeCheck THEN
		     CheckAgainst(at, origType);
		  END;
	       ELSE
		  Emit3(LOAD, "%a,%r", origType, at, tmpreg);
		  IF incdec = NIL THEN
		     Emit3(op, "%r,%c,%r", tmpreg, 1, tmpreg);
		  ELSE
		     Emit3(op, "%r,%a,%r", tmpreg, incdec, tmpreg);
		  END;
		  IF arithmeticRangeCheck THEN
		     NewAttribute(tmpat);
		     WITH tmpat^ DO
			attype := at^.attype;
			mode := regMode;
			reg := tmpreg;
		     END;
		     CheckAgainst(tmpat, origType);
		     ReturnAt(tmpat);
		  END;
		  Emit3(STORE, "%r,%a", origType, tmpreg, at);
	       END;
	    END;
	    IF incdec # NIL THEN
	       ReturnAt(incdec);
	    END;
	    ReturnAt(at);
	 END IncDec;

	 PROCEDURE InclExcl(op: Mnemonic; VAR at, param: Attribute);
	    VAR
	       accReg: Reg;
	 BEGIN
	    IF at^.attype^.form = bigsets THEN
	       AccessMemberOfBigSet(op, at, param);
	    ELSE
	       Load(param);
	       IF arithmeticRangeCheck THEN
		  Assert(at^.attype^.form = sets);
		  CheckAgainst(param, at^.attype^.basep);
	       END;
	       Emit2(SETHI, "%H(%i),%r", MIN(INTEGER), tmpreg);
	       Emit3(SRL, "%r,%a,%r", tmpreg, param, tmpreg);
	       ReturnAt(param);
	       Address(at);
	       WITH at^ DO
		  IF mode = regMode THEN
		     Emit3(op, "%r,%r,%r", reg, tmpreg, reg);
		  ELSE
		     GetReg(accReg);
		     Emit3(LOAD, "%a,%r", attype, at, accReg);
		     Emit3(op, "%r,%r,%r", accReg, tmpreg, accReg);
		     Emit3(STORE, "%r,%a", attype, accReg, at);
		     FreeReg(accReg);
		  END;
	       END;
	    END;
	    ReturnAt(at);
	 END InclExcl;

	 PROCEDURE Float;
	    VAR
	       floatReg: Reg;
	       stackOffset: Offset;
	       low, high: Offset;
	       indexReg: Reg;
	 BEGIN
	    Next; OntoStack(param);
	    Assert(realptr^.size = doubleword);
	    Assert(IsReal(at^.attype) & (at^.attype^.size = doubleword));
	    GetFloatReg(floatReg, doubleword);
	    Emit2(LDF, "%a,%r", param, floatReg); ReturnAt(param);
	    Emit2(FiTOd, "%r,%r", floatReg, floatReg);
	    WITH at^ DO
	       mode := floatRegMode; reg := floatReg;
	    END;
	 END Float;

	 PROCEDURE Trunc;
	    VAR
	       floatReg: Reg;
	       shortrealptr: Type;
	 BEGIN
	    Next; Load(param);
	    GetFloatReg(floatReg, oneword);
	    WITH param^ DO
	       CASE attype^.size OF
	       |  4: Emit2(FsTOi, "%r,%r", reg, floatReg);
	       |  8: Emit2(FdTOi, "%r,%r", reg, floatReg);
	       | 16: Emit2(FqTOi, "%r,%r", reg, floatReg);
	       END;
	       ReleaseAt(param);
	       NEW(shortrealptr); shortrealptr^ := realptr^;
	       shortrealptr^.size := oneword;
	       mode := floatRegMode; reg := floatReg;
	       attype := shortrealptr;
	       OntoStack(param);
	       attype := intptr;
	    END;
	    at := param;
	 END Trunc;

	 PROCEDURE ArgValue;
	    VAR
	       strat, indexat: Attribute; (* arguments of ARGV *)
	       highat: Attribute;         (* HIGH(desat) *)
	       argvReg: Reg;                (* -> argv[index] *)
	       indexReg: Reg;
	       newReg: Reg;
	       loopLabel, condLabel, endLabel: Label;
	 BEGIN
	    StrEmit("%* ARGV");
	    NextParam; strat := param;
	    WITH strat^ DO
	       Assert(attype^.form = arrays);
	       IF attype^.dyn THEN
		  Assert(mode = varAt);
		  NewAttribute(highat); highat^ := strat^;
		  DynArrayHigh(highat); highat^.attype := cardptr;
		  Load(highat);
	       ELSE
		  WITH attype^.ixp^ DO
		     CreateConstAt(max - min, highat);
		  END;
	       END;
	    END;
	    GenExpr(strat); LoadAddr(strat);

	    Next; indexat := param; Load(indexat);
	    IF Reserved(indexat^.reg) THEN
	       GetReg(newReg); Emit3(SLL, "%a,%c,%r", indexat, 2, newReg);
	       indexat^.reg := newReg;
	    ELSE
	       Emit3(SLL, "%a,%c,%a", indexat, 2, indexat);
	    END;

	    GetReg(argvReg);
	    Emit2(SETHI, "%H(%_%s),%r", ".argv", argvReg);
	    Emit3(LD, "%[r,%L(%_%s)],%r", argvReg, ".argv", argvReg);
	    Emit3(LD, "%[r,r],%r", argvReg, indexat^.reg, argvReg);
	    ReturnAt(indexat);

	    GetReg(indexReg);
	    Emit3(ORop, "%r,%r,%r", g0, g0, indexReg);

	    GetLabel(loopLabel); EmitLabel(loopLabel);
	    Emit3(LDUB, "%[r,r],%r", argvReg, indexReg, tmpreg);
	    Emit3(STB, "%r,%[r,r]", tmpreg, strat^.reg, indexReg);
	    Emit3(SUBcc, "%r,%r,%r", tmpreg, g0, g0);
	    GetLabel(endLabel); GenTest(eq, cardAT, endLabel);
	    Emit3(SUBcc, "%r,%r,%r", indexReg, highat^.reg, g0); (* delay s. *)
	    GenTest(lt, cardAT, loopLabel);
	    Emit3(ADD, "%r,%c,%r", indexReg, 1, indexReg); (* delay slot *)
	    EmitLabel(endLabel);

	    ReturnAt(strat); ReturnAt(highat);
	    FreeReg(indexReg); FreeReg(argvReg);
	    at := NIL;
	 END ArgValue;

      BEGIN (* Standard *)
	 WITH at^ DO
	    Assert(procat^.atip^.klass IN Idset{pures, funcs});
	    Assert(procat^.atip^.isstandard);
	 END;
	 IF at^.procat^.atip^.klass = funcs THEN (* standard functions *)
	    CASE at^.procat^.atip^.fname OF
	    | absf:     Next; Load(param);
			IF (atype = realptr) OR (atype = longrealptr) THEN
			   Emit2(FABSs, "%a,%a", param, param);
			ELSE
			   AbsAt(param);
			END;
			at := param;
	    | adrf:     Next; LoadAddr(param); at := param;
			at^.mode := regMode;
	    | argcf:    WITH at^ DO
			   mode := regMode;
			   GetReg(reg);
			   Emit2(SETHI, "%H(%_%s),%r", ".argc", reg);
			   Emit3(LD, "%[r,%L(%_%s)],%r", reg, ".argc", reg);
			END;
	    | capf:     Next; Load(param);
			IF Reserved(param^.reg) THEN
			   GetReg(newReg);
			   Emit3(ANDop, "%a,%c,%r", param, 137B, newReg);
			   param^.reg := newReg;
			ELSE
			   Emit3(ANDop, "%a,%c,%a", param, 137B, param);
			END;
			at := param;
	    | chrf:     Next; Load(param);
			IF arithmeticRangeCheck THEN
			   CheckAgainst(param, charptr);
			END;
			at := param;
	    | fltf:     Float;
	    | higf:     High; at := param;
	    | maxf:     Assert(FALSE); (* should be folded *)
	    | minf:     Assert(FALSE); (* should be folded *)
	    | oddf:     Next; Load(param);
			Emit3(ANDcc, "%a,%c,%r", param, 1, g0);
			ccat := param;
			LoadCond(param); at := param;
	    | ordf:     Next; Load(param);
			IF arithmeticRangeCheck THEN
			   CheckAgainst(param, cardptr);
			END;
			at := param;
	    | sizf:     NextParam;
	                Assert(IsDyn(param^.attype));
			DynArraySize(param); at := param;
	    | trcf:     Trunc;
	    | valf:     Val;

	    (* module SYSTEM *)
	    | sqrf:     Next; Load(param);
			Assert(param^.attype = realptr);
			Assert(~Reserved(param^.reg));
	                Emit2(FSQRTd, "%r,%r", param^.reg, param^.reg);
			at := param;
	    | tszf:     Assert(FALSE); (* should be folded *)
	    | uxf:      UnixCall;
	    | uxff:     UnixFork;
	    | uxsf:     UnixSignal;
	    END;
	 ELSE (* standard procedures *)
	    CASE at^.procat^.atip^.pname OF
	    | argvp:    ArgValue;
	    | decp:     Next; at := param; IncDec(SUB, at);
	    | disp:     Assert(FALSE); (* substituted to DISPOSE *)
	    | exlp:     Next; at := param; Next; InclExcl(ANDN, at, param);
	    | halp:     LoadPosParams;
			Emit1(CALL, "%_%s", ".halt"); Emit(NOP, "");
	    | incp:     Next; at := param; IncDec(ADD, at);
	    | inlp:     Next; at := param; Next; InclExcl(ORop, at, param);
	    | newp:     Assert(FALSE); (* substituted to ALLOCATE *)

	    (* SYSTEM module *)
	    | nprp:     NewProcess;
	    | trsp:     Transfer;
	    END;
	 END;
      END Standard;

      PROCEDURE LoadParams(params: Attribute; plist: Ident);

	 VAR
	    firstparam: Attribute;
	    pident: Ident;
	    param: Attribute;
	    ptype: Type; (* type of parameter *)
	    word: Size;
	    nestedCalls: BOOLEAN;
	    saveParams: BOOLEAN;
	    head, tail: Attribute;

	 PROCEDURE GenParam(VAR at: Attribute);
	 BEGIN
	    IF ~nestedCalls OR
		  ~(at^.mode IN AtModeSet{varAt, constAt, procAt}) THEN
	       GenExpr(at);
	    END;
	 END GenParam;

	 PROCEDURE KeepStackReservation(at: Attribute);
	 BEGIN
	    WITH at^ DO
	       IF (mode IN AtModeSet{addrMode, indexMode}) & tmp THEN
		  StrEmit1("%* KeepStackReservation: offset = %i", stackoffset);
		  StackRes(stackoffset, attype^.size);
		  tmp := FALSE;
	       END;
	    END;
	 END KeepStackReservation;

	 PROCEDURE StoreParam(VAR at: Attribute; param: Ident);
	    VAR
	       paramReg: Reg;
	       low, high: Offset;
	       useIndexReg: BOOLEAN;
	       paramType: Type;
	 BEGIN (* StoreParam *)
	    IF saveParams THEN
	       IF ~(at^.mode IN AtModeSet{varAt, constAt, procAt}) THEN
		  IF param^.vkind = valparam THEN
		     Load(at); at^.isaddr := FALSE;
		  ELSE
		     LoadAddr(at); KeepStackReservation(at);
		     at^.mode := regMode; at^.isaddr := TRUE;
		     at^.attype := addrptr;
		  END;
		  OntoStack(at);
	       ELSE
		  at^.isaddr := FALSE;
	       END;
	    ELSE
	       GenExpr(at);
	       WITH param^ DO
		  IF (vkind = valparam) & arithmeticRangeCheck THEN
		     CheckAgainst(at, param^.idtyp);
		  END;
		  IF inReg THEN
		     paramReg := VAL(Reg, preg + ORD(o0));
		     IF (vkind = valparam) OR nestedCalls & at^.isaddr THEN
			LoadReg(at, paramReg);
		     ELSE
			LoadAddrReg(at, paramReg);
			KeepStackReservation(at);
		     END;
		     ParamReg(paramReg);
		  ELSE
		     IF (vkind = valparam) OR
			   (at^.mode IN AtModeSet{addrMode, indexMode}) &
			   nestedCalls & at^.isaddr THEN
			Load(at);
		     ELSE
			LoadAddr(at);
			KeepStackReservation(at);
			at^.mode := regMode;
		     END;
		     IF idtyp^.size <= doubleword THEN
			LowHigh(vaddr, low, high);
		     ELSE
			LowHigh(vaddr + doubleword, low, high);
		     END;
		     useIndexReg := high # 0;
		     IF useIndexReg THEN
			LoadConst(tmpreg, vaddr);
		     END;
		     IF vkind = valparam THEN
			paramType := idtyp;
		     ELSE
			paramType := addrptr;
		     END;
		     IF at^.mode = regMode THEN
			IF useIndexReg THEN
			   Emit4(STORE, "%a,%[r,r]", paramType,
			      at, top, tmpreg);
			ELSE
			   Emit4(STORE, "%a,%[r,d]", paramType,
			      at, top, vaddr);
			END;
		     ELSE
			IF useIndexReg THEN
			   CASE idtyp^.size OF
			   |  4: Emit3(STF, "%a,%[r,r]", at, top, tmpreg);
			   |  8: Emit3(STDF, "%a,%[r,r]", at, top, tmpreg);
			   | 16: Emit3(STDF, "%a,%[r,r]", at, top, tmpreg);
				 Emit3(ADD, "%r,%c,%r",
				    tmpreg, oneword, tmpreg);
			         Emit3(STDF, "%r,%[r,r]",
				    VAL(Reg, ORD(at^.reg) + 2), 
				    top, tmpreg);
			   END;
			ELSE
			   CASE idtyp^.size OF
			   |  4: Emit3(STF, "%a,%[r,d]", at, top, vaddr);
			   |  8: Emit3(STDF, "%a,%[r,d]", at, top, vaddr);
			   | 16: Emit3(STDF, "%a,%[r,d]", at, top, vaddr);
			         Emit3(STDF, "%r,%[r,d]",
				    VAL(Reg, ORD(at^.reg) + 2), 
				    top, vaddr + doubleword);
			   END;
			END;
		     END;
		  END;
	       END; (* WITH param^ DO *)
	    END;
	 END StoreParam;

	 PROCEDURE NestedCalls(params: Attribute) : BOOLEAN;
	    (* return TRUE if there are any nested calls inside
	       the parameter list
	    *)

	    PROCEDURE AnyCalls(at: Attribute) : BOOLEAN;
	    BEGIN
	       IF at = NIL THEN RETURN FALSE END;
	       WITH at^ DO
		  CASE mode OF
		  | callAt:   IF (procat # NIL) &
				    ~procat^.atip^.isstandard THEN
				 RETURN TRUE
			      END;
			      RETURN NestedCalls(firstparam)
		  | unaryAt:  RETURN AnyCalls(rightop)
		  | binaryAt: RETURN AnyCalls(leftop) OR AnyCalls(rightop)
		  | refAt:    RETURN AnyCalls(desat)
		  | selectAt: RETURN AnyCalls(desat)
		  | indexAt:  RETURN AnyCalls(desat) OR AnyCalls(indexat)
		  ELSE
		     RETURN FALSE
		  END;
	       END;
	    END AnyCalls;

	 BEGIN (* NestedCalls *)
	    WHILE params # NIL DO
	       IF AnyCalls(params) THEN RETURN TRUE END;
	       params := params^.link;
	    END;
	    RETURN FALSE
	 END NestedCalls;

	 PROCEDURE LoadDynParam(VAR at: Attribute; param: Ident; ptype: Type);
	    (* push address of parameter AND dope vector
	       things has to be pushed in reverse order
	       (because stackdirection = backwardDir)
	    *)
	    VAR
	       actdyn: BOOLEAN; (* actual parameter is a dynamic array *)
	       sizeAt: Attribute;
	       etype: Type; (* element type of actual parameter *)
	       lengthInWords: CARDINAL;

	       (* where to store the parameter *)
	       paramInReg: BOOLEAN;
	       poffset, hoffset: Offset;
	       areg, hreg: Reg;

	 BEGIN (* LoadDynParam *)
	    WITH at^.attype^ DO
	       actdyn := (form = arrays) & dyn;
	       IF form = arrays THEN
		  etype := elp;
	       END;
	    END;
	    WITH param^ DO
	       paramInReg := inReg;
	       IF paramInReg THEN
		  areg := VAL(Reg, preg + ORD(o0)); ParamReg(areg);
		  hreg := VAL(Reg, preg + 1 + ORD(o0)); ParamReg(hreg);
	       ELSE
		  poffset := vaddr; hoffset := poffset + oneword;
	       END;
	    END;
	    WITH ptype^ DO
	       IF elp = byteptr THEN
		  IF actdyn THEN
		     Assert(at^.mode = varAt);
		     IF etype^.size = onebyte THEN
			DynArray(at);
			OffsetAt(at, oneword);
			IF paramInReg THEN
			   IF at^.mode = regMode THEN
			      Emit3(ORop, "%a,%r,%r", at, g0, hreg);
			   ELSE
			      Emit2(LD, "%a,%r", at, hreg);
			   END;
			ELSE
			   IF at^.mode = regMode THEN
			      Emit3(ST, "%a,%[r,d]", at, top, hoffset);
			   ELSE
			      Emit2(LD, "%a,%r", at, tmpreg);
			      Emit3(ST, "%r,%[r,d]", tmpreg, top, hoffset);
			   END;
			END;
			OffsetAt(at, -oneword);
			DereferenceAt(at);
		     ELSE
			NewAttribute(sizeAt); sizeAt^ := at^;
			DynArraySize(sizeAt);
			IF paramInReg THEN
			   Emit3(SUB, "%a,%c,%r", sizeAt, 1, hreg);
			ELSE
			   Emit3(SUB, "%a,%c,%a", sizeAt, 1, sizeAt);
			   Emit3(ST, "%a,%[r,d]", sizeAt, top, hoffset);
			END;
			ReturnAt(sizeAt);
			Address(at);
		     END;
		     IF paramInReg THEN
			LoadAddrReg(at, areg);
		     ELSE
			LoadAddr(at);
			Emit3(ST, "%r,%[r,d]", at^.reg, top, poffset);
		     END;
		     KeepStackReservation(at);
		  ELSE
		     (* formal parameter: element type is byteptr
		        actual parameter: is not an dynamic array
		     *)
		     WITH at^.attype^ DO
			IF (form = arrays) & (elp^.size = onebyte) THEN
			   IF paramInReg THEN
			      LoadConst(hreg, ixp^.max - ixp^.min);
			   ELSE
			      LoadConst(tmpreg, ixp^.max - ixp^.min);
			      Emit3(ST, "%r,%[r,d]", tmpreg, top, hoffset);
			   END;
			ELSE
			   IF paramInReg THEN
			      LoadConst(hreg, size - 1);
			   ELSE
			      LoadConst(tmpreg, size - 1);
			      Emit3(ST, "%r,%[r,d]", tmpreg, top, hoffset);
			   END;
			END;
		     END;
		     GenExpr(at);
		     IF at^.attype^.size < oneword THEN
			IF param^.vkind = varparam THEN
			   Error(403); (* alignment problems *)
			ELSE
			   Load(at); (* will be pushed onto stack *)
			END;
		     END;
		     IF ~(at^.mode IN AtModeSet{addrMode, indexMode}) THEN
			OntoStack(at);
			WITH at^ DO
			   StackRes(stackoffset, attype^.size);
			   tmp := FALSE;
			END;
		     END;
		     IF paramInReg THEN
			LoadAddrReg(at, areg);
		     ELSE
			LoadAddr(at);
			Emit3(ST, "%r,%[r,d]", at^.reg, top, poffset);
		     END;
		     KeepStackReservation(at);
		  END;
	       ELSIF elp = wordptr THEN
		  IF actdyn THEN
		     Assert(at^.mode = varAt);
		     NewAttribute(sizeAt); sizeAt^ := at^;
		     DynArraySize(sizeAt);
		     (* align it to a 4-byte boundary, if necessary *)
		     IF etype^.size < oneword THEN
			IF Reserved(sizeAt^.reg) THEN
			   LoadReg(sizeAt, tmpreg);
			END;
			Emit3(ADD, "%a,%c,%a", sizeAt, 3, sizeAt);
			Emit3(ANDN, "%a,%c,%a", sizeAt, 3, sizeAt);
		     END;
		     Assert(sizeAt^.mode = regMode);
		     ConstDivReg(cardAT, sizeAt^.reg, oneword);
		     IF paramInReg THEN
			Emit3(SUB, "%a,%c,%r", sizeAt, 1, hreg);
		     ELSE
			Emit3(SUB, "%a,%c,%a", sizeAt, 1, sizeAt);
			Emit3(ST, "%a,%[r,d]", sizeAt, top, hoffset);
		     END;
		     ReturnAt(sizeAt);
		     Address(at);
		     IF paramInReg THEN
			LoadAddrReg(at, areg);
		     ELSE
			LoadAddr(at);
			Emit3(ST, "%r,%[r,d]", at^.reg, top, poffset);
		     END;
		     KeepStackReservation(at);
		  ELSE
		     (* formal parameter: element type is wordptr
		        actual parameter: is not an dynamic array
		     *)
		     WITH at^.attype^ DO
			lengthInWords := (size + 3) DIV oneword;
			IF paramInReg THEN
			   LoadConst(hreg, lengthInWords - 1);
			ELSE
			   LoadConst(tmpreg, lengthInWords - 1);
			   Emit3(ST, "%r,%[r,d]", tmpreg, top, hoffset);
			END;
		     END;
		     GenExpr(at);
		     IF ~(at^.mode IN AtModeSet{addrMode, indexMode}) THEN
			OntoStack(at);
			WITH at^ DO
			   StackRes(stackoffset, attype^.size);
			   tmp := FALSE;
			END;
		     END;
		     IF paramInReg THEN
			LoadAddrReg(at, areg);
		     ELSE
			LoadAddr(at);
			Emit3(ST, "%r,%[r,d]", at^.reg, top, poffset);
		     END;
		     KeepStackReservation(at);
		  END;
	       ELSE
		  (* formal parameter: element is neither byteptr nor wordptr *)
		  IF actdyn THEN
		     Assert(at^.mode = varAt);
		     DynArray(at);
		     OffsetAt(at, oneword);
		     IF paramInReg THEN
			IF at^.mode = regMode THEN
			   Emit3(ORop, "%a,%r,%r", at, g0, hreg);
			ELSE
			   Emit2(LD, "%a,%r", at, hreg);
			END;
		     ELSE
			IF at^.mode = regMode THEN
			   Emit3(ST, "%a,%[r,d]", at, top, hoffset);
			ELSE
			   Emit2(LD, "%a,%r", at, tmpreg);
			   Emit3(ST, "%r,%[r,d]", tmpreg, top, hoffset);
			END;
		     END;
		     OffsetAt(at, -oneword);
		     DereferenceAt(at);
		  ELSE
		     GenExpr(at);
		     WITH at^.attype^ DO
			Assert(form = arrays);
			StrEmit2("%* max = %c, min = %c", ixp^.max, ixp^.min);
			IF paramInReg THEN
			   LoadConst(hreg, Offset(ixp^.max - ixp^.min));
			ELSE
			   LoadConst(tmpreg, Offset(ixp^.max - ixp^.min));
			   Emit3(ST, "%r,%[r,d]", tmpreg, top, hoffset);
			END;
		     END;
		  END;
		  IF paramInReg THEN
		     LoadAddrReg(at, areg);
		  ELSE
		     LoadAddr(at);
		     Emit3(ST, "%r,%[r,d]", at^.reg, top, poffset);
		  END;
		  KeepStackReservation(at);
	       END;
	    END;
	 END LoadDynParam;

      BEGIN (* LoadParams *)
	 nestedCalls := NestedCalls(params);
	 saveParams := nestedCalls;
	 firstparam := params; pident := plist;
	 head := NIL; tail := NIL;
	 WHILE params # NIL DO
	    Assert(pident # NIL);
	    param := params; params := params^.link;
	    ptype := pident^.idtyp;
	    WITH pident^ DO
	       IF IsDyn(idtyp) THEN
		  IF saveParams THEN
		     IF ~(param^.mode IN AtModeSet{constAt, varAt, procAt}) THEN
			GenParam(param); OntoStack(param);
		     END;
		  ELSE
		     LoadDynParam(param, pident, idtyp);
		  END;
	       ELSIF vkind = valparam THEN
		  IF idtyp^.size > 0 THEN
		     IF (idtyp^.form # arrays) & IsStringConst(param) THEN
			ConvertCharConst(param);
		     END;
		     GenParam(param);
		     Convert(param, idtyp); ptype := param^.attype;
		     StoreParam(param, pident);
		  END;
	       ELSE (* varparam or copyparam *)
		  GenParam(param);
		  StoreParam(param, pident);
	       END;
	    END;
	    pident := pident^.vlink;
	    IF saveParams THEN
	       param^.link := NIL;
	       IF head = NIL THEN
		  head := param;
	       ELSE
		  tail^.link := param;
	       END;
	       tail := param;
	    ELSE
	       ReturnAt(param);
	    END;
	 END;
	 (* 2nd pass in case of nested calls *)
	 IF nestedCalls THEN
	    saveParams := FALSE;
	    params := head; pident := plist;
	    WHILE params # NIL DO
	       param := params; params := params^.link;
	       ptype := pident^.idtyp;
	       WITH pident^ DO
		  IF IsDyn(idtyp) THEN
		     LoadDynParam(param, pident, idtyp);
		  ELSE
		     StoreParam(param, pident);
		  END;
		  ReturnAt(param);
	       END;
	       pident := pident^.vlink;
	    END;
	 END;
	 FreeParamRegs;
      END LoadParams;

      PROCEDURE StaticLink(plevel: CARDINAL);
	 VAR
	    leveldiff: CARDINAL;
      BEGIN
	 StrEmit2("%*  static link (level = %c, plevel = %c)", level, plevel);
	 IF level >= plevel THEN
	    leveldiff := level - plevel;
	    Emit3(LD, "%[r,d],%r", base, procmarkspace, tmpreg);
	    WHILE leveldiff > 0 DO
	       Emit3(LD, "%[r,d],%r", tmpreg, procmarkspace, tmpreg);
	       DEC(leveldiff);
	    END;
	    Emit3(ST, "%r,%[r,d]", tmpreg, top, procmarkspace);
	 ELSE
	    Assert(level+1 = plevel);
	    Emit3(ST, "%r,%[r,d]", base, top, procmarkspace);
	 END;
      END StaticLink;

      VAR
	 isFunction: BOOLEAN;
	 idclass: Idclass;

   BEGIN (* GenCall *)
      WITH at^ DO
	 Assert(mode = callAt);
	 IF procat = NIL THEN
	    isFunction := TRUE;
	    (* type transfer function *)
	    IF attype^.size # firstparam^.attype^.size THEN
	       Error(402);
	    ELSE
	       GenExpr(firstparam);
	       IF attype^.size <= oneword THEN
		  Load(firstparam);
	       ELSE
		  LoadAddr(firstparam);
	       END;
	    END;
	    at := firstparam;
	 ELSIF (procat^.atip # NIL) &
	       (procat^.atip^.klass IN Idset{pures, funcs}) &
	       procat^.atip^.isstandard THEN
	    (* standard procedure or function *)
	    isFunction := procat^.atip^.klass = funcs;
	    ptype := NIL;
	    Standard(firstparam);
	 ELSE
	    ptype := procat^.attype;
	    IF (procat^.atip # NIL) & (procat^.atip^.klass = mods) THEN
	       Assert(firstparam = NIL);
	       Assert(procat^.mode = procAt);
	       idclass := mods;
	    ELSE
	       Assert(ptype # NIL);
	       WITH ptype^ DO
		  Assert(form = proctypes);
		  idclass := rkind;
	       END;
	       LoadParams(firstparam, ptype^.fstparam);
	    END;
	    IF procat^.mode = procAt THEN
	       WITH procat^.atip^ DO
		  IF plev > 1 THEN
		     StaticLink(plev);
		  END;
		  SaveRegs;
		  IF procat^.atip^.klass = mods THEN
		     Emit2(CALL, "%n,%c", procat^.atip, 0);
		  ELSE
		     Emit2(CALL, "%n,%c",
			procat^.atip, procat^.attype^.parregs);
		  END;
		  Emit(NOP, "");
		  RestoreRegs;
	       END;
	    ELSE
	       GenExpr(procat);
	       LoadReg(procat, tmpreg);
	       numberOfRegs := procat^.attype^.parregs;
	       ReturnAt(procat);
	       SaveRegs;
	       Emit2(CALL, "%r,%c", tmpreg, numberOfRegs);
	       Emit(NOP, "");
	       RestoreRegs;
	    END;
	    IF idclass = funcs THEN
	       Assert(ptype # NIL);
	       WITH ptype^ DO
		  isFunction := TRUE;
		  IF IsReal(funcp) THEN
		     mode := floatRegMode;
		     GetFloatReg(reg, funcp^.size);
		     FOR word := 0 TO funcp^.size DIV oneword - 1 DO
			Emit2(FMOVs, "%r,%r",
			   VAL(Reg, ORD(fprval) + word),
			   VAL(Reg, ORD(reg) + word));
		     END;
		  ELSE
		     mode := regMode;
		     GetReg(reg);
		     IF funcp = boolptr THEN
			Emit3(ORcc, "%r,%r,%r", o0, g0, reg); ccat := at;
		     ELSE
			Emit3(ORop, "%r,%r,%r", o0, g0, reg);
		     END;
		  END;
	       END;
	    ELSE
	       isFunction := FALSE;
	    END;
	 END;
      END;
      ReleaseAll; (* stack reservations *)
      IF ~isFunction THEN
	 at := NIL;
      END;
   END GenCall;

   PROCEDURE SwapOps(VAR leftop, rightop: Attribute);
      VAR
	 tmp: Attribute;
   BEGIN
      tmp := leftop; leftop := rightop; rightop := tmp;
   END SwapOps;

   PROCEDURE GenBigSetOp(opsymbol: Symbol; leftop, rightop: Attribute;
                         VAR at: Attribute);
      (* generate a single operation for big sets;
	 leftop and rightop are cleaned up afterwards
      *)
      VAR
	 size: Size;
	 cntReg: Reg;
	 loopLabel, endLabel: Label;
	 setType: Type;
	 leftReg, rightReg: Reg;
	 boolOp: BOOLEAN;

      PROCEDURE InOp(leftop, rightop: Attribute; VAR at: Attribute);
      BEGIN
	 AccessMemberOfBigSet(ANDcc, rightop, leftop);
	 ReturnAt(rightop);
	 NewAttribute(at);
	 WITH at^ DO
	    attype := boolptr;
	    mode := condMode;
	    test := ne;
	    atype := intAT;
	    tlabel.ok := FALSE; flabel.ok := FALSE;
	    ccat := at;
	 END;
      END InOp;

   BEGIN (* GenBigSetOp *)
      IF opsymbol = insy THEN
	 (* all other operations need a loop,
	    so we are sorting this out here
	 *)
	 InOp(leftop, rightop, at); RETURN
      END;

      setType := leftop^.attype;
      Assert(setType = rightop^.attype);
      Assert(setType^.form = bigsets);
      size := setType^.size;
      Assert(size MOD oneword = 0);

      LoadAddr(leftop); LoadAddr(rightop);
      IF opsymbol IN SymSet{eql..leq} THEN
	 IF opsymbol = geq THEN
	    SwapOps(leftop, rightop); opsymbol := leq;
	 END;
	 boolOp := TRUE;
      ELSE
	 (* where to deposit the result? *)
	 IF leftop^.tmp THEN
	    at := leftop;
	 ELSIF rightop^.tmp THEN
	    at := rightop;
	 ELSE
	    AllocOnStack(setType, at); LoadAddr(at);
	 END;
	 boolOp := FALSE;
      END;
      leftReg := tmpreg; GetReg(rightReg);
      GetReg(cntReg); LoadConst(cntReg, size - oneword);

      GetLabel(loopLabel); EmitLabel(loopLabel);
      Emit3(LD, "%[r,r],%r", leftop^.reg, cntReg, leftReg);
      Emit3(LD, "%[r,r],%r", rightop^.reg, cntReg, rightReg);

      IF boolOp THEN
	 GetLabel(endLabel);
	 CASE opsymbol OF
	 | eql, neq:
	       Emit3(SUBcc, "%r,%r,%r", leftReg, rightReg, g0);
	 | leq: (* geq is converted to leq, see above *)
	       Emit3(ANDNcc, "%r,%r,%r", leftReg, rightReg, g0);
	 END;
	 GenTest(ne, cardAT, endLabel); Emit(NOP, "");
	 Emit3(SUBcc, "%r,%c,%r", cntReg, oneword, cntReg);
	 GenTest(ge, intAT, loopLabel); Emit(NOP, "");
	 NewAttribute(at);
	 WITH at^ DO
	    attype := boolptr;
	    mode := condMode;
	    IF opsymbol = neq THEN
	       test := never; tlabel := endLabel; flabel.ok := FALSE;
	    ELSE
	       test := always; flabel := endLabel; tlabel.ok := FALSE;
	    END;
	    atype := intAT;
	    ccat := NIL;
	 END;
      ELSE
	 CASE opsymbol OF
	 | plus:  Emit3(ORop, "%r,%r,%r", leftReg, rightReg, leftReg);
	 | minus: Emit3(ANDN, "%r,%r,%r", leftReg, rightReg, leftReg);
	 | times: Emit3(ANDop, "%r,%r,%r", leftReg, rightReg, leftReg);
	 | slash: Emit3(XOR, "%r,%r,%r", leftReg, rightReg, leftReg);
	 END;
	 Emit3(ST, "%r,%[r,r]", leftReg, at^.reg, cntReg);
	 Emit3(SUBcc, "%r,%c,%r", cntReg, oneword, cntReg);
	 GenTest(ge, intAT, loopLabel); Emit(NOP, "");
      END;
      FreeReg(cntReg); FreeReg(rightReg);
      IF leftop # at THEN
	 ReturnAt(leftop);
      END;
      IF rightop # at THEN
	 ReturnAt(rightop);
      END;
   END GenBigSetOp;

   PROCEDURE AccessMemberOfBigSet(op: Mnemonic; set, member: Attribute);
      (* gen code for member access of big sets;
         op is typically ORop (INCL), ANDN (EXCL) or ANDcc (IN);
	 member is cleaned up afterwards
      *)
      VAR
	 offset: Offset;
	 offsetAt: Attribute;
	 newReg, indexReg, maskReg: Reg;
   BEGIN
      Load(member);
      IF controlRangeCheck THEN
	 Assert(set^.attype^.form = bigsets);
	 CheckAgainst(member, set^.attype^.basep);
      END;
      IF Reserved(member^.reg) THEN
	 GetReg(newReg); Emit3(ORop, "%r,%r,%r", member^.reg, g0, newReg);
	 member^.reg := newReg;
      END;
      offset := set^.attype^.offset;
      IF offset # 0 THEN
	 CreateConstAt(offset, offsetAt);
	 Emit3(SUB, "%a,%a,%a", member, offsetAt, member);
	 ReturnAt(offsetAt);
      END;
      LoadAddr(set); GetReg(indexReg);
      Emit3(ORop, "%a,%r,%r", member, g0, indexReg);
      ConstDivReg(cardAT, indexReg, BitsPerWord);
      ConstMulReg(cardAT, indexReg, oneword);
      ConstModReg(cardAT, member^.reg, BitsPerWord);
      Emit3(LD, "%[r,r],%r", set^.reg, indexReg, tmpreg);
      GetReg(maskReg); Emit2(SETHI, "%H(%i),%r", MIN(INTEGER), maskReg);
      Emit3(SRL, "%r,%a,%r", maskReg, member, maskReg);
      Emit3(op, "%r,%r,%r", tmpreg, maskReg, tmpreg); FreeReg(maskReg);
      IF op = ANDcc THEN
	 ccat := set;
      ELSE
	 (* store result back *)
	 Emit3(ST, "%r,%[r,r]", tmpreg, set^.reg, indexReg);
      END;
      FreeReg(indexReg); ReturnAt(member);
   END AccessMemberOfBigSet;

   PROCEDURE BigSetConstructor(VAR at: Attribute; settype: Type);

      VAR
	 spat: Attribute;
	 setconstAt, setconstAccessAt: Attribute;
	 toReg: Reg; (* used for copying: MoveBytes *)

      PROCEDURE CommaList(VAR at: Attribute);

	 PROCEDURE Element(at: Attribute);
	    VAR
	       r: Reg;
	 BEGIN
	    WITH at^ DO
	       IF mode = constAt THEN
		  ConstSetElement(setconstAt^.cval, cval.value, cval.value);
		  ReturnAt(at);
	       ELSE
		  GenExpr(at);
		  AccessMemberOfBigSet(ORop, spat, at);
	       END;
	    END;
	 END Element;

      BEGIN (* CommaList *)
	 WHILE (at^.mode = binaryAt) & (at^.opsy = comma) DO
	    WITH at^ DO
	       WITH rightop^ DO
		  IF (mode = binaryAt) & (opsy = range) THEN
		     Range(at^.rightop);
		  ELSE
		     Element(at^.rightop);
		  END;
	       END;
	    END;
	    at := at^.leftop;
	 END;
	 Element(at);
      END CommaList;

      PROCEDURE Range(at: Attribute);
	 (* following algorithm is used:

	    IF leftop > rightop THEN
	       DEC(leftop, offset); DEC(rightop, offset);
	       indexReg := leftop DIV 32 * oneword;
	       maskReg := MIN(INTEGER) >> rightop MOD 32;
	       setReg := set[indexReg];
	       LOOP
		  setReg := setReg ORop maskReg;
		  INC(leftop);
		  IF leftop > rightop THEN EXIT END;
		  maskReg >>= 1;
		  IF maskReg = 0 THEN
		     set[indexReg] := setReg;
		     INC(indexReg, oneword);
		     setReg := set[indexReg];
		     maskReg := MIN(INTEGER);
		  END;
	       END;
	       set[indexReg] := setReg;
	    END;

	    optimizations are possible: we could, for example,
	    process larger ranges by storing -1s wordwise
	    with some special handling at the begin and the end
	    of the loop; this overhead, however, is less
	    efficient for small ranges
	 *)
	 VAR
	    indexReg: Reg;
	    maskReg: Reg;
	    setReg: Reg;
	    offset: Offset; offsetAt: Attribute;
	    loopLabel, endOfLoopLabel, endifLabel, endLabel: Label;

	 PROCEDURE LoadOp(at: Attribute);
	    VAR
	       newReg: Reg;
	 BEGIN
	    WITH at^ DO
	       IF mode = regMode THEN
		  IF Reserved(reg) THEN
		     GetReg(newReg);
		     Emit3(ORop, "%r,%r,%r", reg, g0, newReg);
		     reg := newReg;
		  END;
	       ELSE
		  Load(at);
		  Assert((mode = regMode) & ~Reserved(reg));
	       END;
	    END;
	 END LoadOp;

      BEGIN (* Range *)
	 WITH at^ DO
	    IF (leftop^.mode = constAt) &
		  (rightop^.mode = constAt) THEN
	       ConstSetElement(setconstAt^.cval,
		  leftop^.cval.value, rightop^.cval.value);
	       RETURN
	    END;
	    (* lots of optimizations are possible if one of
	       the operands is constant...
	    *)
	    GenExpr(leftop); LoadOp(leftop);
	    GenExpr(rightop); LoadOp(rightop);

	    IF controlRangeCheck THEN
	       Assert(attype^.form = bigsets);
	       CheckAgainst(leftop, attype^.basep);
	       CheckAgainst(rightop, attype^.basep);
	    END;

	    (* IF leftop >= rightop THEN ... *)
	    GetLabel(endLabel);
	    Emit3(SUBcc, "%a,%a,%r", leftop, rightop, g0);
	    GenTest(gt, cardAT, endLabel);

	    (* DEC(leftop, offset); DEC(rightop, offset); *)
	    offset := settype^.offset;
	    IF offset # 0 THEN
	       CreateConstAt(offset, offsetAt);
	       Emit3(SUB, "%a,%a,%a", leftop, offsetAt, leftop);
	       Emit3(SUB, "%a,%a,%a", rightop, offsetAt, rightop);
	       ReturnAt(offsetAt);
	    END;

	    (* indexReg := leftop DIV 32 * oneword *)
	    GetReg(indexReg);
	    Emit3(ORop, "%a,%r,%r", leftop, g0, indexReg);
	    ConstDivReg(cardAT, indexReg, BitsPerWord);
	    ConstMulReg(cardAT, indexReg, oneword);

	    (* maskReg := MIN(INTEGER) >> leftop MOD 32 *)
	    GetReg(maskReg);
	    Emit2(SETHI, "%H(%i),%r", MIN(INTEGER), maskReg);
	    Emit3(ANDop, "%r,%i,%r", leftop^.reg, BitsPerWord-1, tmpreg);
	    Emit3(SRL, "%r,%r,%r", maskReg, tmpreg, maskReg);

	    (* setReg := set[indexReg]; *)
	    GetReg(setReg);
	    Emit3(LD, "%[r,r],%r", spat^.reg, indexReg, setReg);

	    (* LOOP ... *)
	    GetLabel(loopLabel); EmitLabel(loopLabel);

	    (* setReg := setReg ORop maskReg; *)
	    Emit3(ORop, "%r,%r,%r", setReg, maskReg, setReg);

	    (* INC(leftop) *)
	    Emit3(ADD, "%r,%c,%r", leftop^.reg, 1, leftop^.reg);

	    (* IF leftop > rightop THEN EXIT END *)
	    Emit3(SUBcc, "%r,%r,%r", leftop^.reg, rightop^.reg, g0);
	    GetLabel(endOfLoopLabel);
	    GenTest(gt, cardAT, endOfLoopLabel); Emit(NOP, "");

	    (* maskReg >>= 1; *)
	    Emit3(SRL, "%r,%c,%r", maskReg, 1, maskReg);

	    (* IF maskReg = 0 THEN ... *)
	    Emit3(ORcc, "%r,%r,%r", maskReg, g0, g0);
	    GenTest(ne, cardAT, loopLabel); Emit(NOP, "");
	    Emit3(ST, "%r,%[r,r]", setReg, spat^.reg, indexReg);
	    Emit3(ADD, "%r,%c,%r", indexReg, oneword, indexReg);
	    Emit3(LD, "%[r,r],%r", spat^.reg, indexReg, setReg);
	    Emit1(BA, "%l", loopLabel);
	    Emit2(SETHI, "%H(%i),%r", MIN(INTEGER), maskReg); (* delay slot *)

	    EmitLabel(endOfLoopLabel);
	    Emit3(ST, "%r,%[r,r]", setReg, spat^.reg, indexReg);
	    EmitLabel(endLabel);

	    ReturnAt(leftop); ReturnAt(rightop);
	    FreeReg(indexReg); FreeReg(maskReg); FreeReg(setReg);
	    ccat := NIL;
	 END;
      END Range;

   BEGIN (* BigSetConstructor *)
      Assert(settype^.form = bigsets);
      (* collect all constant parts into this set;
         if we do not have any constant parts we get at
	 least an empty set which is used for initialization;
	 there are optimizations possible in cases where
	 we do not have ranges and no constant parts
      *)
      NewAttribute(setconstAt);
      WITH setconstAt^ DO
	 attype := settype; atip := NIL; mode := constAt;
	 InitConstSet(cval, attype);
      END;
      NewAttribute(setconstAccessAt); setconstAccessAt^ := setconstAt^;
      Address(setconstAccessAt); LoadAddr(setconstAccessAt);

      (* allocate a temporary area on the stack which
	 is sufficient to hold our set;
	 our first action is the initialization of this
	 area with the constant parts
      *)
      AllocOnStack(settype, spat);
      LoadAddr(spat);
      GetReg(toReg); Emit3(ORop, "%r,%r,%r", spat^.reg, g0, toReg);
      MoveBytes(setconstAccessAt^.reg, toReg, settype^.size);
      (* MoveBytes releases setconstAccessAt^.reg and toReg *)
      RestrictedReturnAt(setconstAccessAt, setconstAccessAt^.reg);

      WITH at^ DO
	 IF (mode = binaryAt) & (opsy IN SymSet{comma, range}) THEN
	    IF opsy = comma THEN
	       CommaList(at);
	    ELSE
	       Range(at);
	    END;
	 ELSE
	    CommaList(at);
	 END;
      END;
      TermConstSet(setconstAt^.cval);
      at := spat;
   END BigSetConstructor;

   PROCEDURE GenExpr(VAR at: Attribute);
      (* `at' is an expression tree;
	 the result attribute is returned in `at'

	 take care of condition codes:
	 if any attribute is returned with condMode
	 then the condition codes are set and must not
	 be destroyed by following instructions
      *)
      VAR
	 resultType: Type;

      PROCEDURE NewReg(at: Attribute; VAR srcReg: Reg);
      BEGIN
	 WITH at^ DO
	    Assert(mode = regMode);
	    srcReg := reg;
	    IF Reserved(reg) THEN
	       GetReg(reg);
	    END;
	 END;
      END NewReg;

      PROCEDURE Unary(VAR at: Attribute);
	 (* minus:      numerics
	    plus:       numerics
	    notsy:      booleans
	 *)
	 VAR
	    arith: ArithmeticType;
	    freg: Reg;
	    label1, label2: Label;
	    srcReg: Reg;
      BEGIN
	 arith := ArithType(at^.attype);
	 WITH at^ DO
	    IF opsy = notsy THEN
	       label1.ok := FALSE; label2.ok := FALSE;
	       LogOp(at, label1, label2);
	    ELSE
	       GenExpr(rightop);
	       CASE opsy OF
	       | minus: CASE arith OF
			| intAT:       Load(rightop); NewReg(rightop, srcReg);
				       Emit3(SUB, "%r,%r,%a", g0,
					  srcReg, rightop);
			| float32AT..float128AT:
				       Load(rightop);
				       Emit2(FNEGs, "%a,%a", rightop, rightop);
			ELSE
			   Assert(FALSE);
			END;
	       | plus:
	       ELSE
		  Assert(FALSE);
	       END;
	       at := at^.rightop;
	    END;
	 END;
      END Unary;

      PROCEDURE Binary(VAR at: Attribute);
	 (* op may be: insy, divsy, modsy, orsy, andsy,
	               plus, minus, times, slash,
	               eql, neq, lst, grt, leq, geq
	    incl. set construction: comma, range
	 *)
	 VAR
	    arith: ArithmeticType;
	    float: BOOLEAN;
	    label1, label2: Label;
	    helpat: Attribute;
	    testtype: TestType;
	    released: BOOLEAN;
	    dtype: Type;
	    srcReg: Reg;
	    leftOp, rightOp: Attribute;

	 PROCEDURE SetConstructor(VAR at: Attribute; setType: Type);

	    PROCEDURE CommaList(VAR at: Attribute);
	       (* at^.opsy = comma not required;
		  returns `at' as constAt or in regMode
	       *)
	       VAR
		  setpattern: BITSET;
		  setReg: Reg;

	       PROCEDURE Element(at: Attribute);
	       BEGIN
		  WITH at^ DO
		     IF setReg = noReg THEN
			GetReg(setReg);
			Emit3(ORop, "%r,%r,%r", g0, g0, setReg);
		     END;
		     IF mode = constAt THEN
			INCL(setpattern, cval.value);
		     ELSE
			GenExpr(at); Load(at);
			IF arithmeticRangeCheck THEN
			   CheckAgainst(at, setType^.basep);
			END;
			Emit2(SETHI, "%H(%i),%r", MIN(INTEGER), tmpreg);
			Emit3(SRL, "%r,%a,%r", tmpreg, at, tmpreg);
			Emit3(ORop, "%r,%r,%r", tmpreg, setReg, setReg);
			ReleaseAt(at);
		     END;
		  END;
	       END Element;

	    BEGIN (* CommaList *)
	       setpattern := {};
	       setReg := noReg;
	       WHILE (at^.mode = binaryAt) & (at^.opsy = comma) DO
		  WITH at^ DO
		     WITH rightop^ DO
			IF (mode = binaryAt) & (opsy = range) THEN
			   Range(at^.rightop, setReg);
			ELSE
			   Element(at^.rightop);
			END;
		     END;
		  END;
		  at := at^.leftop;
	       END;
	       Element(at);
	       WITH at^ DO
		  attype := bitsetptr;
		  IF setReg = noReg THEN
		     mode := constAt;
		     cval.value := CARDINAL(setpattern);
		  ELSE
		     mode := regMode; reg := setReg;
		     IF setpattern # {} THEN
			LoadConst(tmpreg, INTEGER(setpattern));
			Emit3(ORop, "%r,%r,%r", reg, tmpreg, reg);
		     END;
		  END;
	       END;
	    END CommaList;

	    PROCEDURE Range(VAR at: Attribute; destReg: Reg);
	       (* at^.opsy = range;
		  return at in regMode
	       *)
	       VAR
		  tmpreg2: Reg;
		  rightReg: Reg;
	    BEGIN
	       WITH at^ DO
		  IF leftop^.mode # constAt THEN
		     GenExpr(leftop); Load(leftop);
		     IF arithmeticRangeCheck THEN
			CheckAgainst(leftop, setType^.basep);
		     END;
		  ELSE
		     Address(leftop);
		  END;
		  IF rightop^.mode # constAt THEN
		     GenExpr(rightop); Load(rightop);
		     IF arithmeticRangeCheck THEN
			CheckAgainst(rightop, setType^.basep);
		     END;
		  ELSE
		     Address(rightop);
		  END;
		  GetReg(tmpreg2);
		  Emit3(ORop, "%r,%i,%r", g0, -1, tmpreg);
		  Emit3(SLL, "%r,%a,%r", tmpreg, leftop, tmpreg);
		  IF rightop^.mode # regMode THEN
		     GetReg(rightReg);
		     Emit3(SUB, "%r,%a,%r", g0, rightop, rightReg);
		     rightop^.mode := regMode; rightop^.reg := rightReg;
		  ELSE
		     Emit3(SUB, "%r,%a,%a", g0, rightop, rightop);
		  END;
		  Emit3(ADD, "%a,%c,%a", rightop, 31, rightop);
		  Emit3(ADD, "%a,%a,%r", rightop, leftop, tmpreg2);
		  Emit3(SRL, "%r,%r,%r", tmpreg, tmpreg2, tmpreg);
		  FreeReg(tmpreg2);
		  IF destReg = noReg THEN
		     GetReg(destReg);
		     Emit3(SLL, "%r,%a,%r", tmpreg, rightop, destReg);
		  ELSE
		     Emit3(SLL, "%r,%a,%r", tmpreg, rightop, tmpreg);
		     Emit3(ORop, "%r,%r,%r", destReg, tmpreg, destReg);
		  END;
		  ReturnAt(leftop); ReturnAt(rightop);
		  mode := regMode;
		  reg := destReg;
	       END;
	    END Range;

	 BEGIN (* SetConstructor *)
	    Assert(setType^.form = sets);
	    WITH at^ DO
	       IF (mode = binaryAt) & (opsy IN SymSet{comma, range}) THEN
		  IF opsy = comma THEN
		     CommaList(at);
		  ELSE
		     Range(at, noReg);
		  END;
	       ELSE
		  CommaList(at);
	       END;
	    END;
	 END SetConstructor;

      BEGIN (* Binary *)
	 IF at^.opsy IN SymSet{orsy, andsy} THEN
	    label1.ok := FALSE; label2.ok := FALSE;
	    LogOp(at, label1, label2);
	 ELSIF at^.opsy IN SymSet{comma, range} THEN
	    WITH at^ DO
	       IF (opsy = comma) & (leftop = NIL) THEN
		  IF attype^.form = bigsets THEN
		     BigSetConstructor(rightop, attype);
		  ELSE
		     SetConstructor(rightop, attype);
		  END;
		  at := at^.rightop;
	       ELSE
		  IF attype^.form = bigsets THEN
		     BigSetConstructor(at, attype);
		  ELSE
		     SetConstructor(at, attype);
		  END;
	       END;
	    END;
	 ELSIF at^.rightop^.attype^.form = bigsets THEN
	    WITH at^ DO
	       IF opsy = insy THEN
		  IF IsStringConst(leftop) THEN
		     ConvertCharConst(leftop);
		  END;
	       END;
	       GenExpr(leftop); GenExpr(rightop);
	       GenBigSetOp(opsy, leftop, rightop, at);
	    END;
	 ELSIF at^.opsy = insy THEN
	    WITH at^ DO
	       IF IsStringConst(leftop) THEN
		  ConvertCharConst(leftop);
	       END;
	       GenExpr(leftop);
	       IF leftop^.mode # simm13Mode THEN
		  Load(leftop);
		  IF arithmeticRangeCheck THEN
		     Assert(rightop^.attype^.form = sets);
		     CheckAgainst(leftop, rightop^.attype^.basep);
		  END;
	       END;
	       GenExpr(rightop);
	       IF rightop^.mode # simm13Mode THEN
		  Load(rightop);
	       END;
	       Emit2(SETHI, "%H(%i),%r", MIN(INTEGER), tmpreg);
	       Emit3(SRL, "%r,%a,%r", tmpreg, leftop, tmpreg);
	       Emit3(ANDcc, "%r,%a,%r", tmpreg, rightop, g0);
	       ReturnAt(leftop); ReturnAt(rightop);
	       mode := condMode;
	       test := ne;
	       atype := intAT;
	       tlabel.ok := FALSE;
	       flabel.ok := FALSE;
	       ccat := at;
	    END;
	 ELSIF ((at^.opsy = leq) OR (at^.opsy = geq)) &
	       (at^.leftop^.attype^.form = sets) THEN
	    WITH at^ DO
	       GenExpr(leftop); GenExpr(rightop);
	       IF opsy = geq THEN
		  SwapOps(leftop, rightop);
	       END;
	       (* code for leftop <= rightop (improper set inclusion) *)
	       Load(leftop);
	       IF rightop^.mode # simm13Mode THEN
		  Load(rightop);
	       END;
	       Emit3(ANDNcc, "%a,%a,%r", leftop, rightop, g0);
	       ReturnAt(leftop); ReturnAt(rightop);
	       mode := condMode;
	       test := eq; atype := intAT;
	       tlabel.ok := FALSE; flabel.ok := FALSE;
	       ccat := at;
	    END;
	 ELSE
	    WITH at^ DO
	       IF opsy IN SymSet{eql..leq} THEN
		  dtype := ResultType(leftop, rightop);
	       ELSE
		  dtype := attype;
	       END;
	       IF IsStringConst(leftop) THEN
		  ConvertCharConst(leftop);
	       END;
	       IF IsStringConst(rightop) THEN
		  ConvertCharConst(rightop);
	       END;
	       IF (leftop^.mode # binaryAt) &
		     (rightop^.mode = binaryAt) THEN
		  (* evaluation in reverse order to save registers *)
		  GenExpr(rightop);
		  IF rightop^.mode = condMode THEN
		     Load(rightop);
		  END;
		  GenExpr(leftop);
		  IF leftop^.mode = condMode THEN
		     Load(leftop);
		  END;
	       ELSE
		  GenExpr(leftop);
		  IF leftop^.mode = condMode THEN
		     Load(leftop);
		  END;
		  GenExpr(rightop);
		  IF rightop^.mode = condMode THEN
		     Load(rightop);
		  END;
	       END;
	       arith := ArithType(dtype);
	       float := (dtype = realptr) OR (dtype = longrealptr);
	       IF opsy IN SymSet{eql..leq} THEN
		  GetTestType(opsy, testtype);
		  IF float THEN
		     Load(leftop); Load(rightop);
		     Emit3(FCMP, "%a,%a", dtype, leftop, rightop);
		     Emit(NOP, "");
		     ReturnAt(leftop); ReturnAt(rightop);
		     mode := condMode;
		     test := testtype;
		     atype := arith;
		     tlabel.ok := FALSE;
		     flabel.ok := FALSE;
		     ccat := at;
		  ELSE
		     IF leftop^.mode = simm13Mode THEN
			helpat := rightop; rightop := leftop; leftop := helpat;
			ReverseTest(testtype);
		     END;
		     Load(leftop);
		     IF rightop^.mode # simm13Mode THEN
			Load(rightop);
		     END;
		     leftOp := leftop; rightOp := rightop;
		     mode := cmpMode;
		     test := testtype;
		     atype := arith;
		     reg1 := leftOp^.reg; RestrictedReturnAt(leftOp, reg1);
		     IF rightOp^.mode = simm13Mode THEN
			cmpval := rightOp^.simm13val;
			reg2 := noReg;
			ReturnAt(rightOp);
		     ELSE
			reg2 := rightOp^.reg; cmpval := 0;
			RestrictedReturnAt(rightOp, reg2);
		     END;
		  END;
	       ELSE
		  IF (leftop^.mode = simm13Mode) &
			(opsy IN SymSet{plus, times}) THEN
		     (* commutative operators *)
		     helpat := rightop; rightop := leftop; leftop := helpat;
		  END;
		  Load(leftop);
		  IF float OR (rightop^.mode # simm13Mode) THEN
		     Load(rightop);
		  END;
		  released := FALSE;
		  CASE opsy OF
		  | plus:  IF float THEN
			      Emit4(FADD, "%a,%a,%a",
				 dtype, leftop, rightop, leftop);
			   ELSIF arith = bitAT THEN
			      NewReg(leftop, srcReg);
			      Emit3(ORop, "%r,%a,%a", srcReg, rightop, leftop);
			   ELSE
			      NewReg(leftop, srcReg);
			      Emit3(ADD, "%r,%a,%a", srcReg, rightop, leftop);
			   END;
		  | minus: IF float THEN
			      Emit4(FSUB, "%a,%a,%a",
				 dtype, leftop, rightop, leftop);
			   ELSIF arith = bitAT THEN
			      NewReg(leftop, srcReg);
			      Emit3(ANDN, "%r,%a,%a", srcReg, rightop, leftop);
			   ELSE
			      NewReg(leftop, srcReg);
			      Emit3(SUB, "%r,%a,%a", srcReg, rightop, leftop);
			   END;
		  | times: IF float THEN
			      Emit4(FMUL, "%a,%a,%a",
				 dtype, leftop, rightop, leftop);
			   ELSIF arith = bitAT THEN
			      NewReg(leftop, srcReg);
			      Emit3(ANDop, "%r,%a,%a", srcReg, rightop, leftop);
			   ELSIF rightop^.mode = simm13Mode THEN
			      NewReg(leftop, srcReg);
			      IF srcReg # leftop^.reg THEN
				 Emit3(ORop, "%r,%r,%r", srcReg, g0,
				    leftop^.reg);
			      END;
			      ConstMulReg(arith,
					  leftop^.reg, rightop^.simm13val);
			   ELSE
			      Mult(arith, leftop, rightop);
			      released := TRUE;
			   END;
		  | slash: IF float THEN
			      Emit4(FDIV, "%a,%a,%a",
				 dtype, leftop, rightop, leftop);
			   ELSE (* arith = bitAT *)
			      NewReg(leftop, srcReg);
			      Emit3(XOR, "%r,%a,%a", srcReg, rightop, leftop);
			   END;
		  | divsy: IF rightop^.mode = simm13Mode THEN
			      NewReg(leftop, srcReg);
			      IF srcReg # leftop^.reg THEN
				 Emit3(ORop, "%r,%r,%r", srcReg, g0,
				    leftop^.reg);
			      END;
			      ConstDivReg(arith,
					  leftop^.reg, rightop^.simm13val);
			   ELSE
			      Div(arith, leftop, rightop);
			      released := TRUE;
			   END;
		  | modsy: IF rightop^.mode = simm13Mode THEN
			      NewReg(leftop, srcReg);
			      IF srcReg # leftop^.reg THEN
				 Emit3(ORop, "%r,%r,%r", srcReg, g0,
				    leftop^.reg);
			      END;
			      ConstModReg(arith,
					  leftop^.reg, rightop^.simm13val);
			   ELSE
			      Mod(arith, leftop, rightop);
			      released := TRUE;
			   END;
		  END;
		  IF ~released THEN
		     ReturnAt(rightop);
		  END;
		  at := at^.leftop;
	       END;
	    END;
	 END;
      END Binary;

      PROCEDURE Designator(VAR at: Attribute);
	 (* mode one of refAt, selectAt, indexAt, or fieldAt *)
	 VAR
	    elementSize: Offset;
	    highat: Attribute;
      BEGIN (* Designator *)
	 WITH at^ DO
	    IF mode = fieldAt THEN
	       UseWith(flevel, at);
	    ELSE
	       IF (mode = indexAt) &
		     desat^.attype^.dyn & controlRangeCheck THEN
		  NewAttribute(highat); highat^ := desat^;
		  Assert(highat^.mode = varAt);
		  DynArrayHigh(highat);
		  highat^.attype := cardptr;
	       END;
	       GenExpr(desat);
	       CASE mode OF
	       | refAt:    DereferenceAt(desat);
	       | selectAt: OffsetAt(desat, field^.fldaddr);
	       | indexAt:  WITH desat^.attype^ DO
			      elementSize := elp^.size;
			      IF ~dyn THEN
				 OffsetAt(desat,
				    - INTEGER(ixp^.min) * elementSize);
			      END;
			   END;
			   IF indexat^.mode = constAt THEN
			      OffsetAt(desat,
				 INTEGER(indexat^.cval.value) * elementSize);
			      IF controlRangeCheck & desat^.attype^.dyn THEN
				 Address(indexat); Load(indexat);
				 DynArrayCheck(indexat, highat);
				 ReturnAt(indexat);
			      END;
			   ELSE
			      GenExpr(indexat);
			      Load(indexat);
			      IF controlRangeCheck THEN
				 IF desat^.attype^.dyn THEN
				    DynArrayCheck(indexat, highat);
				 ELSE
				    CheckAgainst(indexat, desat^.attype^.ixp);
				 END;
			      END;
			      IndexAtReg(desat, indexat^.reg, elementSize);
			      (* indexat returned by IndexAtReg *)
			   END;
	       END;
	       at := at^.desat;
	    END;
	 END;
      END Designator;

   BEGIN (* GenExpr *)
      resultType := at^.attype;
      CASE at^.mode OF
      | binaryAt: Binary(at);
      | unaryAt:  Unary(at);
      | refAt,
	selectAt,
	indexAt,
	fieldAt:  Designator(at);
      | callAt:   GenCall(at);
      ELSE
      END;
      IF at # NIL THEN
	 at^.attype := resultType;
	 Address(at);
      END;
   END GenExpr;

   PROCEDURE ParseAndGenDesignator(VAR at: Attribute);
   BEGIN
      ParseDesignator(at);
      GenExpr(at);
   END ParseAndGenDesignator;

   PROCEDURE ParseAndGenExpression(VAR at: Attribute; targetReg: Reg);
      (* targetReg may be noReg *)
   BEGIN
      ParseExpression(at);
      GenExpr(at);
      IF targetReg # noReg THEN
	 LoadReg(at, targetReg);
      END;
   END ParseAndGenExpression;

END MCP4Expr.
