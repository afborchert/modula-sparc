(* Ulm's Modula-2 Compiler    Solaris 2.x/SPARCv8
   -- partially derived from ETH Zurichs Modula-2 Compiler for Lilith --
   Copyright (C) 1983-1996 Universitaet Ulm, SAI, 89069 Ulm, Germany
             (C) 1979-1981 Institut fuer Informatik, ETH Zuerich, Switzerland
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
   $Id: MCP4Stateme.m2,v 0.5 1999/01/25 09:23:38 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Stateme.m2,v $
   Revision 0.5  1999/01/25  09:23:38  borchert
   bug fix: ReturnStatement was releasing intrval if it was allocated but
            not reserved

   Revision 0.4  1998/06/22  19:13:15  borchert
   bug fix: index value of FOR-loop was stored in tmpreg *before*
            evaluation of limit expression which is free to modify tmpreg

   Revision 0.3  1998/05/12  08:33:26  borchert
   bug fix: EmitLabel(endLabel) was invoked with undefined endLabel
            in case of constant REPEAT conditions.

   Revision 0.2  1998/04/23  18:07:00  borchert
   bug fix: Assign did not release stack reservation of expAT
            for objects larger than 4 bytes

   Revision 0.1  1997/02/21  18:40:33  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4Statements; (* AFB 3/96 *)

   (* code generation for statements *)

   FROM MCBase IMPORT Symbol, realptr, Type, Size, oneword, Idclass, Label,
      Offset, charptr, Stptr, Structform, maxint, maxcard, Idset, Kindvar;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Attributes IMPORT Attribute, intrval, fprval, Reg, AtMode,
      GetLabel, tmpreg, NewAttribute, TestType, ArithmeticType;
   FROM MCP4BasicOps IMPORT GenReturn, ReturnAt, Address, Load, MoveBytes,
      OffsetAt, LoadAddr, LoadReg, LoadCond, InvertTest, GenTest,
      LowHigh, LoadConst, OntoStack, ccat, ReverseTest, ConstMulReg,
      RestrictedReturnAt, ConvertCharConst, IsStringConst, LoadPosParams,
      CheckAgainst;
   FROM MCP4CodeSys IMPORT StrEmit, StrEmit1, Emit, Emit1, Emit2, Emit3,
      EmitLabel;
   FROM MCP4Blocks IMPORT blockNptr;
   FROM MCP4Expr IMPORT ParseAndGenDesignator, ParseAndGenExpression,
      ParseDesignator, ParseExpression, GenExpr, GenLogExpr;
   FROM MCP4Global IMPORT Assert, CompilerError, Error;
   FROM MCP4Register IMPORT GetReg, FreeReg, Allocated, Reserved, RegRequest;
   FROM MCP4Scanner IMPORT GetSymbol, sy, Skip, val, arithmeticRangeCheck;
   FROM MCP4Stack IMPORT StackAlloc, StackFree;
   FROM MCP4Types IMPORT BaseType, SizeType, IsReal, Arithmetic, intcarptr,
      TestBaseType;
   FROM Memory IMPORT ALLOCATE, DEALLOCATE;
   IMPORT MCBase, MCP4Attributes, MCP4BasicOps, MCP4CodeSys, MCP4Expr,
      MCP4Global, MCP4Register, MCP4Scanner, MCP4Stack, Memory;

   TYPE
      LoopStmt = POINTER TO LoopStmtRec;
      LoopStmtRec =
	 RECORD
	    endLabel: Label;
	    outerLoop: LoopStmt;
	 END;
   VAR
      loops: LoopStmt; (* points to innermost loop *)

   PROCEDURE Assign(desAT, expAT: Attribute);
      VAR
	 size: Size;
   BEGIN
      Address(desAT); Address(expAT);
      size := SizeType(desAT); (* use desAt in case of variants ! *)
      IF IsReal(expAT^.attype) OR
	    (* in case of type conversions *)
	    (expAT^.mode = floatRegMode) THEN
	 Load(expAT); Assert(expAT^.mode = floatRegMode);
	 Emit3(STORE, "%r,%a", desAT^.attype, expAT^.reg, desAT);
	 IF size = 4 * oneword THEN
	    OffsetAt(desAT, 2 * oneword);
	    Emit3(STORE, "%r,%a", desAT^.attype,
	          VAL(Reg, ORD(expAT^.reg) + 2), desAT);
	 END;
	 ReturnAt(desAT); ReturnAt(expAT);
      ELSIF size > oneword THEN
	 LoadAddr(expAT); LoadAddr(desAT);
	 MoveBytes(expAT^.reg, desAT^.reg, size);
	 RestrictedReturnAt(expAT, expAT^.reg);
	 RestrictedReturnAt(desAT, desAT^.reg);
      ELSE
	 IF arithmeticRangeCheck THEN
	    CheckAgainst(expAT, desAT^.attype);
	 END;
	 IF desAT^.mode = regMode THEN
	    LoadReg(expAT, desAT^.reg);
	 ELSE
	    Load(expAT);
	    Emit3(STORE, "%a,%a", desAT^.attype, expAT, desAT);
	 END;
	 ReturnAt(desAT); ReturnAt(expAT);
      END;
   END Assign;

   PROCEDURE Assignment;
      VAR 
	 at1, at2: Attribute;
   BEGIN 
      ParseAndGenDesignator(at1);
      GetSymbol;
      ParseExpression(at2);
      IF IsStringConst(at2) & (at1^.attype^.form # arrays) THEN
	 ConvertCharConst(at2);
      ELSE
	 GenExpr(at2);
      END;
      Assign(at1, at2);
   END Assignment;

   PROCEDURE ReturnStatement;
      VAR
	 at: Attribute;
	 rtype: Type;
	 allocated: BOOLEAN;
	 done: BOOLEAN;
   BEGIN
      IF sy = lparent THEN
	 WITH blockNptr^ DO
	    Assert(klass = funcs);
	    rtype := idtyp^.funcp;
	 END;
         IF IsReal(rtype) THEN
	    ParseAndGenExpression(at, fprval);
	    IF Allocated(fprval) THEN
	       ReturnAt(at);
	    ELSE
	       RestrictedReturnAt(at, fprval);
	    END;
         ELSE
	    Assert(rtype^.size <= oneword);
	    allocated := Allocated(intrval) OR Reserved(intrval);
	    (* if allocated is TRUE, we are going to scratch an
	       already allocated register; this does not harm us
	       because we are returning but we have to take care
	       that we do not accidently release it
	    *)
	    IF ~allocated THEN
	       RegRequest(intrval, done);
	       Assert(done);
	    END;
	    ParseAndGenExpression(at, intrval);
	    IF allocated THEN
	       (* keep intrval allocated *)
	       RestrictedReturnAt(at, intrval);
	    ELSE
	       ReturnAt(at);
	    END;
         END;
      ELSE
	 Assert(blockNptr^.klass IN Idset{pures, mods});
      END;
      GenReturn;
   END ReturnStatement;

   PROCEDURE IfStatement;
      VAR
         at: Attribute;
         endifLabel, elsifLabel: Label;
   BEGIN
      StrEmit("%* IF statement");
      GetLabel(endifLabel);
      LOOP
         ParseAndGenExpression(at, noReg);
         IF at^.mode = simm13Mode THEN
            IF VAL(BOOLEAN, at^.simm13val) THEN
               StatSequ3(endsy, elsifsy, elsesy);
               IF sy # endsy THEN
                  Skip(endsy, endsy);
               END;
               EXIT
            ELSE
               Skip(elsesy, elsifsy);
               IF sy # elsifsy THEN
                  EXIT
               END;
               GetSymbol;
            END;
         ELSE
	    WITH at^ DO
	       IF mode # condMode THEN
		  LoadCond(at);
	       END;
	       IF ~flabel.ok THEN
		  GetLabel(elsifLabel);
	       ELSE
		  elsifLabel := flabel;
	       END;
	       InvertTest(test);
	       GenTest(test, atype, elsifLabel);
	       Emit(NOP, "");
	       IF tlabel.ok THEN
		  EmitLabel(tlabel);
	       END;
	    END;
            StatSequ3(endsy, elsifsy, elsesy);
	    IF sy # endsy THEN
	       Emit1(BA, "%l", endifLabel);
	       Emit(NOP, "");
	    END;
            EmitLabel(elsifLabel);
            IF sy # elsifsy THEN
               EXIT
            END;
            GetSymbol;
         END;
      END;
      IF sy = elsesy THEN
         GetSymbol;
         StatSequ1(endsy);
      END;
      StrEmit("%* END (* IF *)");
      EmitLabel(endifLabel);
      GetSymbol; (* skip endsy *)
   END IfStatement;

   PROCEDURE WhileStatement;   
      VAR
	 whileCond: Attribute;
	 beginLabel, condLabel, endLabel: Label;
   BEGIN
      ParseExpression(whileCond);

      StrEmit("%* WHILE statement");
      IF whileCond^.mode = constAt THEN
         IF VAL(BOOLEAN, whileCond^.cval.value) THEN
	    GetLabel(beginLabel); EmitLabel(beginLabel);
            StatSequ1(endsy);
	    Emit1(BA, "%l", beginLabel);
	    Emit(NOP, "");
         ELSE
            Skip(endsy, endsy);
         END;
      ELSE
	 GetLabel(condLabel);
	 Emit1(BA, "%l", condLabel);
	 Emit(NOP, "");
	 GetLabel(beginLabel); EmitLabel(beginLabel);
         StatSequ1(endsy);  
	 EmitLabel(condLabel);
	 GetLabel(endLabel);
	 GenLogExpr(whileCond, beginLabel, endLabel);
	 WITH whileCond^ DO
	    GenTest(test, atype, beginLabel);
	    Emit(NOP, "");
	 END;
	 EmitLabel(endLabel);
      END;
      GetSymbol;
      StrEmit("%* END (* WHILE *)");
   END WhileStatement;  

   PROCEDURE RepeatStatement;    
      VAR
	 beginLabel, endLabel: Label;
	 exitCond: Attribute;
   BEGIN
      StrEmit("%* REPEAT");
      GetLabel(beginLabel); EmitLabel(beginLabel);
      StatSequ1(untilsy);
      GetSymbol;  
      StrEmit("%* UNTIL ...");
      ParseExpression(exitCond);
      IF exitCond^.mode = constAt THEN
         IF ~VAL(BOOLEAN, exitCond^.cval.value) THEN
	    Emit1(BA, "%l", beginLabel);
	    Emit(NOP, "");
	 END;
      ELSE
	 GetLabel(endLabel);
	 GenLogExpr(exitCond, endLabel, beginLabel);
	 WITH exitCond^ DO
	    InvertTest(test);
	    GenTest(test, atype, beginLabel);
	    Emit(NOP, "");
	 END;
	 EmitLabel(endLabel);
      END;
      StrEmit("%* (* end of REPEAT loop *)");
   END RepeatStatement;  

   PROCEDURE LoopStatement;   
      VAR
	 beginLabel, endLabel: Label;
	 loopStatement: LoopStmt;
   BEGIN
      GetLabel(beginLabel); GetLabel(endLabel);
      NEW(loopStatement); loopStatement^.endLabel := endLabel;
      loopStatement^.outerLoop := loops; loops := loopStatement;

      StrEmit("%* LOOP");
      EmitLabel(beginLabel);
      StatSequ1(endsy);
      Emit1(BA, "%l", beginLabel); Emit(NOP, "");
      GetSymbol; 
      EmitLabel(endLabel);
      StrEmit("%* END (* LOOP *)");

      loops := loops^.outerLoop;
      DISPOSE(loopStatement);
   END LoopStatement;  
   
   PROCEDURE ExitStatement;   
   BEGIN  
      Assert(loops # NIL);
      StrEmit("%* EXIT");
      Emit1(BA, "%l", loops^.endLabel);
      Emit(NOP, "");
   END ExitStatement;  

   (*
    *	FOR index := start TO end BY step DO
    *	   .
    *      .
    *   END;
    *
    *	design of for-loop:
    *
    *	(strategy = withOverflow)
    *
    *		l	r0,start
    *		b	compare
    *	begin	equ *
    *		st	r0,index
    *		.
    *		.
    *		l	r0,index
    *		ais	r0,step
    *	compare	equ *
    *		c	r0,end
    *		b<=	begin
    *
    *	(strategy = noOverflow / ABS(step) = 1)
    *
    *		l	r0,start
    *		c	r0,end
    *		b>	endfor
    *		b	assign
    *	begin	equ	*
    *		ais	r0,step
    *	assign	equ	*
    *		st	r0,index
    *		.
    *		.
    *		l	r0,index
    *		c	r0,end
    *		b<	begin
    *
    *   (strategy = noOverflow / ABS(step) > 1)
    *
    *		l	r0,start
    *		c	r0,end
    *		b>	endfor
    *		b	assign
    *	begin	equ	*
    *		ais	r0,step
    *	assign	equ	*
    *		st	r0,index
    *		.
    *		.
    *		l	r0,index
    *		l	r1,end
    *		sr	r1,r0
    *		c	r1,step
    *		b>=	begin
    *)

   PROCEDURE ForStatement;
      VAR
         counterVarAt, counterAt: Attribute; (* attribut of counter/index *)
         startAt: Attribute; (* attribut of start value *)
         limitAt: Attribute; (* attribut of limit value *)
         step: INTEGER;
	 compLabel: Label;
	 beginforLabel: Label;
         endforLabel: Label;
         assignLabel: Label;

	 limitConst: CARDINAL;
	 atype: ArithmeticType; (* signed or unsigned compare ? *)
	 rstype: Stptr;
         atleastone: BOOLEAN;
         strategy: (withOverflow, noOverflow);
         min, max: CARDINAL;

      PROCEDURE Compare(test: TestType; atype: ArithmeticType; dest: Label);
	 VAR
	    low, high: Offset;
	    limitReg: Reg;
	    stepReg: Reg;
      BEGIN
	 IF limitAt = NIL THEN
	    LowHigh(limitConst, low, high);
	    IF high # 0 THEN
	       GetReg(limitReg);
	       LoadConst(limitReg, limitConst);
	       Emit3(SUBcc, "%a,%r,%r", counterAt, limitReg, g0);
	       FreeReg(limitReg);
	    ELSE
	       Emit3(SUBcc, "%a,%i,%r", counterAt, limitConst, g0);
	    END;
	 ELSE
	    GetReg(limitReg);
	    Emit3(LOAD, "%a,%r", limitAt^.attype, limitAt, limitReg);
	    Emit3(SUBcc, "%a,%r,%r", counterAt, limitReg, g0);
	    FreeReg(limitReg);
	 END;
	 IF step < 0 THEN ReverseTest(test) END;
	 GenTest(test, atype, dest);
	 Emit(NOP, "");
	 ccat := NIL;
      END Compare;

      PROCEDURE CompareWithStep(test: TestType;
				atype: ArithmeticType; dest: Label);
	 VAR
	    cmpReg: Reg;
	    stepReg: Reg;
	    absStep: INTEGER;
	    low, high: INTEGER;
      BEGIN
	 GetReg(cmpReg);
	 IF limitAt = NIL THEN
	    LoadConst(cmpReg, limitConst);
	 ELSE
	    Emit3(LOAD, "%a,%r", limitAt^.attype, limitAt, cmpReg);
	 END;
	 IF step > 0 THEN
	    Emit3(SUB, "%r,%a,%r", cmpReg, counterAt, cmpReg);
	 ELSE
	    Emit3(SUB, "%a,%r,%r", counterAt, cmpReg, cmpReg);
	 END;
	 absStep := ABS(step);
	 LowHigh(absStep, low, high);
	 IF high # 0 THEN
	    GetReg(stepReg); LoadConst(stepReg, absStep);
	    Emit3(SUBcc, "%r,%r,%r", cmpReg, stepReg, g0);
	    FreeReg(stepReg);
	 ELSE
	    Emit3(SUBcc, "%r,%i,%r", cmpReg, absStep, g0);
	 END;
	 FreeReg(cmpReg);
	 GenTest(test, atype, dest);
	 Emit(NOP, "");
	 ccat := NIL;
      END CompareWithStep;

      PROCEDURE LoadIndex;
	 VAR
	    at: Attribute;
      BEGIN
	 IF counterAt # counterVarAt THEN
	    IF counterVarAt^.mode = varAt THEN
	       NewAttribute(at); at^ := counterVarAt^; Address(at);
	    ELSE
	       at := counterVarAt;
	    END;
	    Emit3(LOAD, "%a,%a", counterVarAt^.attype, at, counterAt);
	    IF at # counterVarAt THEN
	       ReturnAt(at);
	    END;
	 END;
      END LoadIndex;

      PROCEDURE StoreIndex;
	 VAR
	    at: Attribute;
      BEGIN
	 IF counterAt # counterVarAt THEN
	    IF counterVarAt^.mode = varAt THEN
	       NewAttribute(at); at^ := counterVarAt^; Address(at);
	    ELSE
	       at := counterVarAt;
	    END;
	    Emit3(STORE, "%a,%a", counterVarAt^.attype, counterAt, at);
	    IF at # counterVarAt THEN
	       ReturnAt(at);
	    END;
	 END;
      END StoreIndex;

      PROCEDURE Increment(step: INTEGER);
	 VAR
	    low, high: Offset;
	    tmpReg: Reg;
      BEGIN
         (* increment/decrement counter value *)
	 LowHigh(step, low, high);
	 IF high = 0 THEN
	    Emit3(ADD, "%a,%i,%a", counterAt, step, counterAt);
	 ELSE
	    GetReg(tmpReg);
	    LoadConst(tmpReg, step);
	    Emit3(ADD, "%a,%r,%a", counterAt, tmpReg, counterAt);
	    FreeReg(tmpReg);
	 END;
      END Increment;

      PROCEDURE MinMax(type: Stptr; VAR cmin, cmax: CARDINAL);
      BEGIN
         WITH type^ DO
            CASE form OF
            | subranges: cmin := min; cmax := max;
            | enums:     cmin := 0; cmax := cstnr;
            | bools:     cmin := 0; cmax := 1;
            | cards,
	      longcards: cmin := 0; cmax := maxcard;
            | ints,
	      longints:  cmin := CARDINAL(-maxint-1); cmax := maxint;
            | chars:     cmin := 0; cmax := 377B;
            | opens:     MinMax(openstruc, cmin, cmax);
            ELSE
               CompilerError;
            END;
         END;
      END MinMax;

      PROCEDURE InRange() : BOOLEAN;
	 VAR
	    absstep: CARDINAL;
	    maxchar: CARDINAL;
      BEGIN
         IF (rstype = intcarptr) & (step < 0) THEN
            RETURN TRUE
         END;
         IF limitAt # NIL THEN RETURN FALSE END;
	 IF rstype = charptr THEN
	    (* in case of rstype=charptr byte-arithmetic will be taken! *)
	    absstep := VAL(CARDINAL, ABS(step));
	    maxchar := ORD(MAX(CHAR));
	    RETURN (step > 0) & (limitConst <= maxchar-absstep) OR
		   (step < 0) & (limitConst >= absstep)
	 END;
	 (* long arithmetic *)
         IF atype = intAT THEN
            RETURN (step > 0) & (INTEGER(limitConst) <= maxint-step) OR
                   (step < 0) & (INTEGER(limitConst) >= -maxint-1-step);
         ELSE
            RETURN (step > 0) & (limitConst <= maxcard-CARDINAL(step)) OR
                   (step < 0) & (limitConst >= CARDINAL(ABS(step)));
         END;
      END InRange;

   BEGIN (* ForStatement *)
      StrEmit("%* FOR-statement");
      GetLabel(beginforLabel);
      GetLabel(compLabel);
      GetLabel(assignLabel);
      GetLabel(endforLabel);

      ParseDesignator(counterVarAt);
      WITH counterVarAt^ DO
	 Assert((mode = varAt) & (atip # NIL));
	 IF (atip^.state = local) & ~atip^.indaccess & atip^.inReg THEN
	    Address(counterVarAt); counterAt := counterVarAt;
	 ELSE
	    NewAttribute(counterAt); counterAt^ := counterVarAt^;
	    WITH counterAt^ DO
	       mode := regMode;
	       reg := tmpreg;
	    END;
	 END;
      END;
      Assert(counterAt^.mode = regMode);

      GetSymbol; (* comma *)
      ParseExpression(startAt);
      IF IsStringConst(startAt) THEN
	 ConvertCharConst(startAt);
      ELSE
	 GenExpr(startAt);
      END;
      atype := Arithmetic(counterAt, startAt);
      rstype := TestBaseType(counterAt^.attype);

      GetSymbol; (* tosy *)
      ParseExpression(limitAt);
      IF IsStringConst(limitAt) THEN
	 ConvertCharConst(limitAt);
	 limitConst := limitAt^.simm13val; limitAt := NIL;
      ELSE
	 WITH limitAt^ DO
	    IF mode = constAt THEN
	       limitConst := cval.value;
	    ELSE
	       GenExpr(limitAt);
	       Load(limitAt);
	       OntoStack(limitAt);
	    END;
	 END;
	 IF limitAt^.mode = constAt THEN
	    limitAt := NIL;
	 END;
      END;

      IF sy = bysy THEN
	 GetSymbol;
	 GetSymbol;
         step := val;
      ELSE
	 step := 1;
      END;
      IF step = 0 THEN
	 Error(202);
      END;

      LoadReg(startAt, counterAt^.reg);

      (* strategy ?? *)
      IF InRange() THEN
         (* limitConst + step is in [min..max] *)
         strategy := withOverflow;
	 StrEmit("%*   with overflow");
      ELSE
         strategy := noOverflow;
	 StrEmit("%*   without overflow");
      END;
      MinMax(counterAt^.attype, min, max);
      atleastone := (limitAt = NIL) &
         ((step < 0) & (limitConst = min) OR
          (step > 0) & (limitConst = max));

      IF strategy = withOverflow THEN
         (* branch to loop condition *)
	 Emit1(BA, "%l", compLabel); Emit(NOP, "");
      ELSE
         IF ~atleastone THEN
            Compare(gt, atype, endforLabel);
         END;
	 Emit1(BA, "%l", assignLabel); Emit(NOP, "");
      END;

      (* top of the loop *)
      EmitLabel(beginforLabel);

      IF strategy = noOverflow THEN
         Increment(step);
      END;

      EmitLabel(assignLabel);
      StoreIndex;

      (* body of the loop *)
      StatSequ1(endsy);

      LoadIndex;
      IF strategy = withOverflow THEN
         Increment(step);
      END;

      (* condition part *)
      EmitLabel(compLabel);
      IF strategy = withOverflow THEN
         Compare(le, atype, beginforLabel);
      ELSE
         IF ABS(step) = 1 THEN
            Compare(ne, atype, beginforLabel);
         ELSE
	    (* the difference between counter and limit must
	       be greater or equal to the step size
	    *)
            CompareWithStep(ge, atype, beginforLabel);
         END;
      END;

      EmitLabel(endforLabel);
      IF limitAt # NIL THEN
	 ReturnAt(limitAt);
      END;
      ReturnAt(counterAt);
      GetSymbol;
      StrEmit("%* END (* FOR *)");
   END ForStatement;

   PROCEDURE CaseStatement;                           
      (* the case expression is assumed to have integer type *)

      TYPE 
         Case = POINTER TO CaseRec;
         CaseRec = 
            RECORD
               (* [clow..chigh] *)
               clow: INTEGER;
               chigh: INTEGER;
	       clabel: Label;
               next: Case;
            END;

      VAR 
         cHeader: Case; (* header of circular ordered list *)
	 low, high: INTEGER; lhInit: BOOLEAN;
         caseAt: Attribute; newReg: Reg;
	 compareLabel, caseTableLab, elseLab, endLabel: Label;
         strategy: (withTable, noTable);
         cost: INTEGER; (* memory needed for 2nd strategy *)
         diff: INTEGER;

      PROCEDURE CaseLabels;
         VAR
	    label: Label;
            rlow, rhigh: INTEGER;

         PROCEDURE EnterCaseLabel(flow, fhigh: INTEGER; flabel: Label);
            (* enter new element into circular ordered list *)
            VAR
               cp: Case; (* new element *)
               pred, succ: Case;
         BEGIN
	    IF lhInit THEN
	       lhInit := FALSE;
	       low := flow;
	       high := fhigh;
            ELSE
	       IF flow < low THEN
	          low := flow;
               END;
               IF fhigh > high THEN
	          high := fhigh;
               END;
	    END;
            NEW(cp);
            WITH cp^ DO
               clow := flow;
               chigh := fhigh;
	       clabel := flabel;
            END;

            cHeader^.chigh := fhigh; (* guarantees termination of loop *)
            pred := cHeader;
            succ := cHeader^.next;
            WHILE fhigh > succ^.chigh DO
               pred := succ;
               succ := succ^.next;
            END;
            IF (succ # cHeader) & (fhigh >= succ^.clow) OR
               (pred # cHeader) & (flow  <= pred^.chigh) THEN
               Error(223);
            END;

            (* try to join if labels are equal *)
	    (*
            IF (flabel = pred^.clabel) & (pred^.chigh+1 = flow) THEN
               pred^.chigh := fhigh;
               DISPOSE(cp);
            ELSIF (flabel = succ^.clabel) & (fhigh+1 = succ^.clow) THEN
               succ^.clow := flow;
               DISPOSE(cp);
            ELSE (* new element in list *)
	    *)
               INC(cost);
               cp^.next := succ;
               pred^.next := cp;
	    (*
            END;
	    *)
         END EnterCaseLabel;

      BEGIN (* CaseLabels *)
	 GetLabel(label);
	 EmitLabel(label);

         REPEAT
            rlow := val;
	    GetSymbol;
            IF sy = range THEN
	       GetSymbol;
               rhigh := val;
               GetSymbol;
            ELSE
               rhigh := rlow;
            END;
            EnterCaseLabel(rlow, rhigh, label);
         UNTIL sy = colon;
	 GetSymbol;
      END CaseLabels;

      PROCEDURE CaseTable;
         VAR
            cp, cp2: Case;
            upper, i: INTEGER;
      BEGIN
         IF strategy = withTable THEN
	    EmitLabel(caseTableLab);
         END;
         cp := cHeader^.next;
         IF cp # cHeader THEN
            WITH cp^ DO
               upper := chigh;
               IF strategy = withTable THEN
                  FOR i := clow TO chigh DO
		     StrEmit1("%:L%l", clabel);
                  END;
               ELSE
                  IF clow > - maxint-1 THEN
		     CompareWithConst(caseAt, clow-1);
		     GenTest(le, intAT, elseLab); Emit(NOP, "");
                  END;
		  CompareWithConst(caseAt, chigh);
		  GenTest(le, intAT, clabel); Emit(NOP, "");
               END;
               cp2 := cp;
	       cp := next;
               DISPOSE(cp2);
            END;
            WHILE cp # cHeader DO
               WITH cp^ DO
                  IF strategy = withTable THEN
                     (* upper+1 TO clow-1 *)
                     FOR i := 2 TO INTEGER(clow - upper) DO
			StrEmit1("%:L%l", elseLab);
                     END;
                     FOR i := clow TO chigh DO
			StrEmit1("%:L%l", clabel);
                     END;
                  ELSE
                     IF upper # clow-1 THEN
			CompareWithConst(caseAt, clow-1);
			GenTest(le, intAT, elseLab); Emit(NOP, "");
                     END;
		     CompareWithConst(caseAt, chigh);
		     GenTest(le, intAT, clabel); Emit(NOP, "");
                  END;
                  upper := chigh;
                  cp2 := cp;
		  cp := next;
		  DISPOSE(cp2);
               END;
            END; (* WHILE *)
         END; (* IF *)
         IF strategy = noTable THEN
	    Emit1(BA, "%l", elseLab); Emit(NOP, "");
         END;
      END CaseTable;

      PROCEDURE CompareWithConst(at: Attribute; const: INTEGER);
      BEGIN
	 OpConst(SUBcc, at, const, g0);
      END CompareWithConst;

      PROCEDURE OpConst(op: Mnemonic; at: Attribute; const: INTEGER;
                        targetReg: Reg);
	 VAR
	    low, high: INTEGER;
      BEGIN
	 LowHigh(const, low, high);
	 IF high # 0 THEN
	    LoadConst(tmpreg, const);
	    Emit3(op, "%a,%r,%r", at, tmpreg, targetReg);
	 ELSE
	    Emit3(op, "%a,%i,%r", at, const, targetReg);
	 END;
      END OpConst;

   BEGIN (* CaseStatement *)
      StrEmit("%* CASE");
      cost := 0;
      lhInit := TRUE;
      NEW(cHeader); (* header of empty circular ordered list *)
      WITH cHeader^ DO
	 next := cHeader;
         clow := 0;
         chigh := 0;
         clabel.ok := FALSE;
      END;
      GetLabel(caseTableLab);
      GetLabel(elseLab);
      GetLabel(endLabel);
      GetLabel(compareLabel);
      ParseExpression(caseAt);
      Emit1(BA, "%l", compareLabel);
      Emit(NOP, "");

      WHILE sy = ofsy DO
	 GetSymbol;           
         CaseLabels; 
         StatSequ3(ofsy, elsesy, endsy); 
	 Emit1(BA, "%l", endLabel); Emit(NOP, "");
      END;  
      EmitLabel(elseLab);
      IF sy = elsesy THEN
	 GetSymbol; 
         StatSequ1(endsy);
	 Emit1(BA, "%l", endLabel); Emit(NOP, "");
      ELSE
	 LoadPosParams;
	 Emit1(CALL, "%_%s", ".case"); Emit(NOP, "");
      END;
      GetSymbol;

      (* choice strategy *)
      (* be carefully: high may be maxint and low minint !! *)
      diff := high DIV 2 - low DIV 2;
      IF diff <= cost + diff DIV 4 THEN
         strategy := withTable;
      ELSE
         strategy := noTable;
      END;

      (* branch through case table *)
      IF strategy = withTable THEN
	 StrEmit("%* case table");
	 CaseTable;
	 EmitLabel(compareLabel); GenExpr(caseAt); Load(caseAt);
	 IF Reserved(caseAt^.reg) THEN
	    GetReg(newReg); LoadReg(caseAt, newReg);
	 END;
	 IF low # MIN(INTEGER) THEN
	    CompareWithConst(caseAt, low);
	    GenTest(lt, intAT, elseLab); Emit(NOP, "");
	 END;
	 IF high # maxint THEN
	    CompareWithConst(caseAt, high);
	    GenTest(gt, intAT, elseLab); Emit(NOP, "");
	 END;
         IF low < 0 THEN
	    OpConst(ADD, caseAt, -low, caseAt^.reg);
         ELSIF low > 0 THEN
	    OpConst(SUB, caseAt, low, caseAt^.reg);
         END;
	 ConstMulReg(intAT, caseAt^.reg, oneword);
	 Emit2(SETHI, "%H(%l),%r", caseTableLab, tmpreg);
	 Emit3(ORop, "%L(%l),%r,%r", caseTableLab, tmpreg, tmpreg);
	 Emit3(LD, "%[r,r],%r", tmpreg, caseAt^.reg, tmpreg);
	 Emit2(JMPL, "%r,%r", tmpreg, g0);
	 Emit(NOP, "");
      ELSE
	 StrEmit("%* explicit comparisons");
	 EmitLabel(compareLabel);
	 GenExpr(caseAt); Load(caseAt);
	 IF Reserved(caseAt^.reg) THEN
	    GetReg(newReg); LoadReg(caseAt, newReg);
	 END;
	 CaseTable;
      END;
      ReturnAt(caseAt);
      EmitLabel(endLabel);

      (* clean up *)
      DISPOSE(cHeader);
      StrEmit("%* END (* CASE *)");
   END CaseStatement; 

   MODULE WithStatements;

      FROM MCBase IMPORT Offset, Symbol, Kindvar, Type, addrptr, oneword;
      FROM MCP4Attributes IMPORT Attribute, Reg, AtMode, NewAttribute;
      FROM MCP4BasicOps IMPORT LoadReg, OntoStack, Address, Load,
         DereferenceAt, LoadAddrReg, ReturnAt, LoadAddr, LowHigh;
      FROM MCP4CodeSys IMPORT StrEmit, StrEmit2;
      FROM MCP4Global IMPORT Assert;
      FROM MCP4Expr IMPORT ParseDesignator, GenExpr;
      FROM MCP4Register IMPORT GetReg, WithReg, FreeReg, Reserved;
      FROM MCP4Scanner IMPORT GetSymbol;
      FROM MCP4Stack IMPORT StackAlloc, StackFree;
      FROM Memory IMPORT ALLOCATE, DEALLOCATE;
      IMPORT StatSequ1;
      EXPORT WithType, UseWith, WithStatement;

      TYPE
	 WithStmt = POINTER TO WithStmtRec;
	 WithStmtRec = RECORD
	    desat: Attribute;
	       (* this attribute is either a register or a static
	          static location which holds the address
	       *)
	    reservedReg: BOOLEAN; (* do we have reserved a register? *)
	    prev, next: WithStmt; (* next inner & outer with statement *)
	 END;

      VAR
	 head, tail: WithStmt;
	    (* head points to the outermost, tail to
	       the innermost with statement
	    *)
	 withLevel: INTEGER; (* nest level of WITH statements *)

      PROCEDURE WithType(i: INTEGER; VAR type: Type);
	 VAR
	    withStmt: WithStmt;
	    count: INTEGER;
      BEGIN
	 Assert((i > 0) & (i <= withLevel));

	 DEC(i); withStmt := head;
	 WHILE i > 0 DO
	    withStmt := withStmt^.next; DEC(i);
	 END;
	 type := withStmt^.desat^.attype;
      END WithType;

      PROCEDURE UseWith(i: INTEGER; VAR at: Attribute);
	 (* at gets the description of the with element;
	    may produce code for loading the address of the with variable
	 *)
	 VAR
	    withStmt: WithStmt;
	    count: INTEGER;
	    origType: Type;
      BEGIN
	 StrEmit2("%* UseWith: withLevel = %c, i = %c", withLevel, i);
	 Assert((i > 0) & (i <= withLevel));

	 DEC(i); withStmt := head;
	 WHILE i > 0 DO
	    withStmt := withStmt^.next; DEC(i);
	 END;
	 
	 NewAttribute(at); at^ := withStmt^.desat^;
	    (* we need to take a copy -- then the
	       caller is free to cleanup & deallocate the attribute
	    *)
	 WITH at^ DO
	    IF (mode = indexMode) & tmp THEN
	       (* fetch with address from stack *)
	       origType := attype; attype := addrptr; tmp := FALSE;
	       Load(at); attype := origType;
	       mode := addrMode;
	    ELSIF mode = varAt THEN
	       Address(at);
	    ELSE
	       Assert((mode = addrMode) & Reserved(reg));
	    END;
	 END;
      END UseWith;

      PROCEDURE EnterWith(at: Attribute);
	 (* store at for further use by UseWith,
	    code for entry of a with statement
	 *)
	 VAR
	    withStmt: WithStmt;
	    withReg: Reg;
	    reserved: BOOLEAN;
	    origType: Type;

	 PROCEDURE HugeOffset() : BOOLEAN;
	    (* return TRUE if OntoStack is not useful due
	       to offsets which are not representable as simm13
	    *)
	    VAR
	       offset, low, high: Offset;
	 BEGIN
	    StackAlloc(offset, oneword); StackFree(offset, oneword);
	    LowHigh(offset, low, high);
	    RETURN high # 0
	 END HugeOffset;

      BEGIN (* EnterWith *)
	 reserved := FALSE;
	 IF at^.mode # varAt THEN
	    IF (at^.mode = refAt) & (at^.desat^.mode = varAt) &
		  (at^.desat^.atip^.state = local) &
		  at^.desat^.atip^.inReg & (at^.desat^.atip^.voffset = 0) &
		  ~at^.desat^.atip^.indaccess THEN
	       (* pointer is already allocated in a register --
		  so just take that register
	       *)
	       WITH at^ DO
		  GenExpr(desat); Assert(desat^.mode = regMode);
		  mode := addrMode;
		  reg := desat^.reg;
		  Assert(Reserved(reg));
		  tmp := FALSE;
	       END;
	    ELSIF (at^.mode = refAt) & (withLevel < 2) THEN
	       GenExpr(at^.desat);
	       GetReg(withReg);
	       LoadReg(at^.desat, withReg);
	       WithReg(withReg);
	       reserved := TRUE;
	       at^.mode := addrMode;
	       at^.reg := withReg;
	       at^.tmp := FALSE;
	    ELSE
	       GenExpr(at);
	       IF (withLevel < 2) OR HugeOffset() THEN
		  (* in case of huge offsets we resort to with registers
		     even with the danger to run out of registers;
		     otherwise we run into the assertion below
		  *)
		  GetReg(withReg);
		  LoadAddrReg(at, withReg);
		  WithReg(withReg);
		  reserved := TRUE;
	       ELSE
		  (* don't consume too much registers for WITH statements *)
		  LoadAddr(at); origType := at^.attype; at^.attype := addrptr;
		  at^.mode := regMode; OntoStack(at); at^.attype := origType;
		  (* assert that at does not consume any registers *)
		  Assert((at^.mode = indexMode) & (at^.xreg = noReg) &
		         Reserved(at^.reg));
	       END;
	    END;
	 END;

	 NEW(withStmt);
	 WITH withStmt^ DO
	    desat := at;
	    reservedReg := reserved;
	    prev := tail;
	    next := NIL;
	 END;
	 IF head = NIL THEN
	    head := withStmt;
	 ELSE
	    tail^.next := withStmt;
	 END;
	 tail := withStmt;
	 INC(withLevel);
      END EnterWith;

      PROCEDURE ExitWith;
	 (* exit the innermost with statement *)
	 VAR
	    withStmt: WithStmt;
      BEGIN
	 Assert(withLevel > 0);
	 withStmt := tail;
	 tail := tail^.prev;
	 IF tail = NIL THEN
	    head := NIL;
	 ELSE
	    tail^.next := NIL;
	 END;

	 WITH withStmt^ DO
	    IF reservedReg THEN
	       FreeReg(desat^.reg); desat^.reg := noReg;
	    END;
	    desat^.attype := addrptr; ReturnAt(desat);
	 END;
	 DISPOSE(withStmt);
	 DEC(withLevel);
      END ExitWith;

      PROCEDURE WithStatement;
	 VAR
	    at: Attribute;
      BEGIN
	 StrEmit("%* WITH");
	 ParseDesignator(at); EnterWith(at);
	 StatSequ1(endsy);
	 GetSymbol; ExitWith;
	 StrEmit("%* END (* WITH *)");
      END WithStatement;

   BEGIN
      withLevel := 0;
      head := NIL; tail := NIL;
   END WithStatements;

   PROCEDURE Statement;
      VAR
	 at: Attribute;
   BEGIN
      IF sy = becomes THEN GetSymbol; Assignment;
      ELSIF sy = call THEN
	 GetSymbol; ParseAndGenExpression(at, noReg); Assert(at = NIL);
      ELSIF sy = ifsy THEN GetSymbol; IfStatement;
      ELSIF sy = whilesy THEN GetSymbol; WhileStatement;
      ELSIF sy = loopsy THEN GetSymbol; LoopStatement;
      ELSIF sy = exitsy THEN GetSymbol; ExitStatement;
      ELSIF sy = repeatsy THEN GetSymbol; RepeatStatement;
      ELSIF sy = forsy THEN GetSymbol; ForStatement;
      ELSIF sy = casesy THEN GetSymbol; CaseStatement;
      ELSIF sy = withsy THEN GetSymbol; WithStatement;
      ELSIF sy = returnsy THEN GetSymbol; ReturnStatement;
      (* ELSE empty statement without GetSymbol*)
      END;
   END Statement;

   PROCEDURE StatSequ1(s: Symbol);
   BEGIN
      REPEAT
         Statement;
      UNTIL sy = s;
   END StatSequ1;

   PROCEDURE StatSequ3(s1, s2, s3: Symbol);
   BEGIN
      REPEAT
         Statement;
      UNTIL (sy = s1) OR (sy = s2) OR (sy = s3);
   END StatSequ3;

BEGIN
   loops := NIL;
END MCP4Statements.
