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
   $Id: MCP4BasicOp.m2,v 0.2 1998/04/23 18:05:36 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4BasicOp.m2,v $
   Revision 0.2  1998/04/23  18:05:36  borchert
   bug fix: LoadAddrReg must not release stack reservations

   Revision 0.1  1997/02/21  18:40:29  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4BasicOps; (* AFB 3/96 *)

   (* generate basic operations for common use of 4th pass;
      every procedure may generate code and is authorized to
      modify `at' but not `at^.attype'
      `at' and everything in connection with `at' must be valid
   *)

   FROM MCBase IMPORT Offset, Size, Ident, Type, Idclass, Structform,
      procmarkspace, BitsPerWord, intptr, cardptr, Varkind, Label, Kindvar,
      oneword, doubleword, charptr, addrptr, mainmodp, Stset;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Attributes IMPORT ArithmeticType, Attribute, Reg,
      TestType, AtMode, Simm13, GetLabel, ArithmeticTypeSet, AtModeSet,
      RegSet, NewAttribute, base, tmpreg;
   FROM MCP4Blocks IMPORT level;
   FROM MCP4CodeSys IMPORT Emit, Emit1, Emit2, Emit3, Emit4, EmitLabel,
      StrEmit, StrEmit1, StrEmit2, StrEmit3;
   FROM MCP4Global IMPORT Assert, Error;
   FROM MCP4Register IMPORT GetReg, FreeReg, Reserved,
      GetFloatReg, FreeFloatReg, SaveParamRegs, RestoreParamRegs,
      Allocated;
   FROM MCP4Scanner IMPORT line;
   FROM MCP4Stack IMPORT StackAlloc, StackFree;
   FROM MCP4Types IMPORT IsReal, ArithType, ByteSize, IsDyn;
   FROM Memory IMPORT ALLOCATE, DEALLOCATE;

   (* (* exported by definition *)
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
   *)

   PROCEDURE LowHigh(off: Offset; VAR low, high: Offset);
      (* most instructions are able to take a signed 13-bit value only;
	 therefore we need to split larger values into a
	 upper half of 22 bits (for SETHI instruction) and
	 a lower half of 10 bits
      *)
      CONST
	 modulo = 1024;
   BEGIN
      IF (off >= MIN(Simm13)) & (off <= MAX(Simm13)) THEN
	 low := off; high := 0;
      ELSE
	 (* perform splitting in unsigned arithmetic *)
	 high := INTEGER(CARDINAL(off) DIV modulo);
	 low := INTEGER(CARDINAL(off) MOD modulo);
      END;
      (*
      StrEmit3("%* LowHigh %i ---> high: %i, low: %i", off, high, low);
      *)
   END LowHigh;

   PROCEDURE High(off: Offset) : Offset;
      VAR
	 low, high: Offset;
   BEGIN
      LowHigh(off, low, high); RETURN high
   END High;

   PROCEDURE Low(off: Offset) : Offset;
      VAR
	 low, high: Offset;
   BEGIN
      LowHigh(off, low, high); RETURN low
   END Low;

   PROCEDURE LoadConst(reg: Reg; const: Offset);
      VAR
	 low, high: Offset;
   BEGIN
      IF const = 0 THEN
	 Emit3(ORop, "%r,%r,%r", g0, g0, reg);
      ELSE
	 LowHigh(const, low, high);
	 IF high # 0 THEN
	    Emit2(SETHI, "%i,%r", high, reg);
	 END;
	 IF low # 0 THEN
	    IF high # 0 THEN
	       Emit3(ORop, "%r,%i,%r", reg, low, reg);
	    ELSE
	       Emit3(ORop, "%r,%i,%r", g0, low, reg);
	    END;
	 END;
      END;
   END LoadConst;

   PROCEDURE CreateConstAt(const: Offset; VAR at: Attribute);
      VAR
	 low, high: Offset;
   BEGIN
      NewAttribute(at);
      WITH at^ DO
	 link := NIL; attype := intptr;
	 LowHigh(const, low, high);
	 IF high # 0 THEN
	    mode := simm13Mode; simm13val := const;
	 ELSE
	    mode := regMode; GetReg(reg); LoadConst(reg, const);
	 END;
      END;
   END CreateConstAt;

   PROCEDURE IsStringConst(at: Attribute) : BOOLEAN;
   BEGIN
      WITH at^ DO
	 RETURN (at^.mode = constAt) & (attype^.form = arrays)
      END;
   END IsStringConst;

   PROCEDURE ConvertCharConst(at: Attribute);
      (* convert character array constants into char constants;
	 PRE:  at^.mode = constAt
	 POST: at^.mode = simm13Mode
      *)
      TYPE
	 CharPtr = POINTER TO CHAR;
      VAR
	 cp: CharPtr;
   BEGIN
      WITH at^ DO
	 Assert((mode = constAt) & (attype^.form = arrays));
	 cp := CharPtr(cval.svalue^.valentry);
	 mode := simm13Mode; attype := charptr;
	 simm13val := ORD(cp^);
      END;
   END ConvertCharConst;

   PROCEDURE Address(at: Attribute);
      (* convert `at' and generate code such that
	 `at' is a valid addressing mode for LD and similar instructions
	 and can be given as %a-argument to Emit;
	 `at.mode' must be a leaf-node, i.e. not unaryAt,...
      *)
      VAR
	 ip: Ident;

      PROCEDURE AccessGlobalVar(at: Attribute; ip: Ident);
      BEGIN
	 WITH at^ DO
	    Assert(~atip^.indaccess);
	    mode := indexMode;
	    GetReg(reg); labelip := ip;
	    rlabel.ok := FALSE; addr := 0; xreg := noReg; tmp := FALSE;
	    Emit2(SETHI, "%H(%n),%r", ip, reg);
	 END;
      END AccessGlobalVar;

      PROCEDURE AccessAbsVar(at: Attribute; address: Size);
      BEGIN
	 WITH at^ DO
	    Assert(~atip^.indaccess);
	    mode := indexMode;
	    GetReg(reg); addr := Low(address);
	    labelip := NIL; rlabel.ok := FALSE;
	    xreg := noReg; tmp := FALSE;
	    Emit2(SETHI, "%c,%r", High(address), reg);
	 END;
      END AccessAbsVar;

      PROCEDURE AccessLocalVar(at: Attribute; ip: Ident; levelDiff: CARDINAL);
	 VAR
	    lastreg: Reg;
	    origType: Type;
	    low, high: Offset;
      BEGIN
	 WITH at^ DO
	    IF ip^.voffset = 0 THEN
	       Assert((levelDiff = 0) & ip^.inReg);
	       IF ip^.indaccess THEN
		  mode := addrMode; tmp := FALSE;
	       ELSE
		  mode := regMode;
	       END;
	       IF ip^.vkind = noparam THEN
		  reg := VAL(Reg, ORD(l0) + ip^.lreg);
	       ELSE
		  reg := VAL(Reg, ORD(i0) + ip^.preg);
	       END;
	    ELSE
	       mode := indexMode;
	       labelip := NIL; rlabel.ok := FALSE; tmp := FALSE;
	       IF levelDiff = 0 THEN
		  reg := base;
	       ELSE
		  Assert(ip^.voffset # 0);
		  GetReg(reg); lastreg := base;
		  WHILE levelDiff > 0 DO
		     Emit3(LD, "%[%r,%c],%r", lastreg, procmarkspace, reg);
		     DEC(levelDiff); lastreg := reg;
		  END;
	       END;
	       LowHigh(ip^.voffset, low, high);
	       IF high = 0 THEN
		  addr := ip^.voffset; xreg := noReg;
	       ELSE
		  addr := 0;
		  GetReg(xreg); LoadConst(xreg, ip^.voffset);
	       END;
	       IF ip^.indaccess THEN
		  origType := attype; attype := addrptr;
		  Load(at);
		  attype := origType;
		  mode := addrMode; tmp := FALSE;
	       END;
	    END;
	 END;
      END AccessLocalVar;

      PROCEDURE AccessConstByLabel(at: Attribute; VAR label: Label);
      BEGIN
	 IF ~label.ok THEN
	    GetLabel(label); label.head := 'C';
	 END;
	 WITH at^ DO
	    mode := indexMode;
	    GetReg(reg); rlabel := label;
	    addr := 0; xreg := noReg; tmp := FALSE; labelip := NIL;
	    Emit2(SETHI, "%H(%l),%r", label, reg);
	 END;
      END AccessConstByLabel;

      PROCEDURE AccessProcedure(at: Attribute; ip: Ident);
      BEGIN
	 Assert(ip # NIL);
	 WITH at^ DO
	    mode := regMode;
	    GetReg(reg); Emit2(SETHI, "%H(%n),%r", ip, reg);
	    Emit3(ORop, "%r,%L(%n),%r", reg, ip, reg);
	 END;
      END AccessProcedure;

      PROCEDURE AccessConstVal(at: Attribute; value: CARDINAL);
	 VAR
	    ival, low, high: INTEGER;
      BEGIN
	 ival := INTEGER(value); (* conversion without checks *)
	 LowHigh(ival, low, high);
	 WITH at^ DO
	    IF value = 0 THEN
	       mode := regMode; reg := g0;
	    ELSIF high = 0 THEN
	       mode := simm13Mode; simm13val := low;
	    ELSE
	       mode := regMode; GetReg(reg);
	       Emit2(SETHI, "%i,%r", high, reg);
	       IF low # 0 THEN
		  Emit3(ORop, "%r,%i,%r", reg, low, reg);
	       END;
	    END;
	 END;
      END AccessConstVal;

   BEGIN (* Address *)
      WITH at^ DO
	 CASE mode OF
	 | varAt: ip := atip;
		  WITH ip^ DO
		     Assert(klass = vars);
		     CASE state OF
		     | global:   AccessGlobalVar(at, ip);
		     | local:    AccessLocalVar(at, ip, level - vlevel);
		     | absolute: AccessAbsVar(at, vaddr);
		     | separate: AccessGlobalVar(at, ip);
		     END;
		  END;
	 | constAt:
		  CASE attype^.form OF
		  | arrays:
			Assert(cval.svalue^.inlist);
			AccessConstByLabel(at, cval.svalue^.label);
		  | reals, longreals:
			IF attype^.form = longreals THEN
			   Error(214); (* not supported yet *)
			END;
			AccessConstByLabel(at, cval.rvalue^.label);
		  | bigsets:
			AccessConstByLabel(at, cval.setvalue^.label);
		  ELSE
		     AccessConstVal(at, cval.value);
		  END;
	 | procAt:
		  AccessProcedure(at, atip);
	 | regMode, floatRegMode,
	   indexMode, addrMode, simm13Mode, condMode, cmpMode:
		  (* already OK *)
	 ELSE
	    Assert(FALSE);
	 END;
      END;
   END Address;

   PROCEDURE DynArray(at: Attribute);
      (* PRE:  at must be in varAt;
         POST: at is set so that both, the pointer to the array contents
	       and the high value, are accessible;
	       note that regMode may be returned -- in this case
	       the high value may be found in reg+1
      *)
      VAR
	 ip: Ident;
   BEGIN
      WITH at^ DO
	 Assert(mode = varAt);
	 Assert((attype^.form = arrays) & (attype^.dyn));
	 Assert((atip # NIL) & atip^.indaccess);
	 ip := atip;
	 ip^.indaccess := FALSE;
	 Address(at);
	 ip^.indaccess := TRUE;
      END;
   END DynArray;

   PROCEDURE DynArrayHigh(at: Attribute);
   BEGIN
      DynArray(at);
      WITH at^ DO
	 IF mode = regMode THEN
	    reg := VAL(Reg, ORD(reg) + 1);
	 ELSE
	    OffsetAt(at, oneword);
	 END;
      END;
   END DynArrayHigh;

   PROCEDURE DynArraySize(at: Attribute);
      VAR
	 srcReg, destReg: Reg;
   BEGIN
      DynArrayHigh(at); Load(at);
      WITH at^ DO
	 srcReg := reg;
	 IF Reserved(reg) THEN
	    GetReg(destReg); reg := destReg;
	 END;
	 Emit3(ADD, "%r,%c,%r", srcReg, 1, reg);
	 IF ~ByteSize(attype) THEN
	    ConstMulReg(cardAT, reg, attype^.elp^.size);
	 END;
      END;
   END DynArraySize;

   PROCEDURE AlignReg(srcReg, destReg: Reg);
      (* align contents of `reg' *)
   BEGIN
      Emit3(ADD, "%r,%c,%r", srcReg, 7, destReg);
      Emit3(ANDN, "%r,%c,%r", destReg, 7, destReg); (* 8-byte boundary *)
   END AlignReg;

   PROCEDURE Min(reg1, reg2: Reg);
      (* return unsigned minimum of reg1 and reg2 in reg1;
	 code sequence has been found by GNU super optimizer
      *)
      VAR
	 tmpreg2: Reg;
   BEGIN
      GetReg(tmpreg2);
      Emit3(SUBcc, "%r,%r,%r", reg2, reg1, tmpreg);
      Emit3(SUBX, "%r,%r,%r", g0, g0, tmpreg2);
      Emit3(ANDop, "%r,%r,%r", tmpreg2, tmpreg, reg2);
      Emit3(ADDcc, "%r,%r,%r", reg2, reg1, reg1);
      FreeReg(tmpreg2);
      ccat := NIL;
   END Min;

   PROCEDURE Max(reg1, reg2: Reg);
      (* return unsigned maximum of reg1 and reg2 in reg1;
	 code sequence has been found by GNU super optimizer
      *)
      VAR
	 tmpreg2: Reg;
   BEGIN
      GetReg(tmpreg2);
      Emit3(SUBcc, "%r,%r,%r", reg1, reg2, tmpreg);
      Emit3(SUBX, "%r,%r,%r", g0, g0, tmpreg2);
      Emit3(ANDN, "%r,%r,%r", tmpreg2, tmpreg, reg2);
      Emit3(ADDXcc, "%r,%r,%r", reg2, reg1, reg1);
      FreeReg(tmpreg2);
      ccat := NIL;
   END Max;

   PROCEDURE Load(at: Attribute);
      (* load the corresponding value of `at' into a register;
	 `at' must not be a record or an array
      *)
      VAR
	 newreg: Reg;
   BEGIN
      WITH at^ DO
	 IF (mode = regMode) OR (mode = floatRegMode) THEN RETURN END;
	 IF IsReal(attype) THEN
	    GetFloatReg(newreg, attype^.size);
	    LoadReg(at, newreg);
	 ELSIF mode = addrMode THEN
	    IF Reserved(reg) THEN
	       GetReg(newreg); LoadReg(at, newreg);
	    ELSE
	       LoadReg(at, reg);
	    END;
	 ELSIF mode = indexMode THEN
	    IF (reg # noReg) & ~Reserved(reg) THEN
	       LoadReg(at, reg);
	    ELSIF (xreg # noReg) & ~Reserved(xreg) THEN
	       LoadReg(at, xreg);
	    ELSE
	       GetReg(newreg); LoadReg(at, newreg);
	    END;
	 ELSE
	    IF IsReal(attype) THEN
	       GetFloatReg(newreg, attype^.size);
	    ELSE
	       GetReg(newreg);
	    END;
	    LoadReg(at, newreg);
	 END;
      END;
   END Load;

   PROCEDURE NextReg(r: Reg; inc: CARDINAL) : Reg;
   BEGIN
      RETURN VAL(Reg, ORD(r) + inc)
   END NextReg;

   PROCEDURE LoadReg(at: Attribute; r: Reg);
      (* like Load with destination register `r' *)
      VAR
	 cnt: CARDINAL;
	 label: Label;
   BEGIN
      WITH at^ DO
	 CASE mode OF
	 | regMode:
	       IF reg # r THEN
		  Emit3(ORop, "%r,%r,%r", reg, g0, r);
		  ReleaseReg(reg); reg := r;
	       END;
	 | floatRegMode:
	       IF reg # r THEN
		  FOR cnt := 0 TO attype^.size DIV oneword - 1 DO
		     Emit2(FMOVs, "%r,%r", NextReg(reg, cnt), NextReg(r, cnt));
		  END;
		  IF Allocated(reg) THEN
		     FreeFloatReg(reg, attype^.size);
		  END;
		  reg := r;
	       END;
	 | addrMode, indexMode:
	       IF IsReal(attype) THEN
		  Emit3(LOAD, "%a,%r", attype, at, r);
		  IF attype^.size = 4 * oneword THEN
		     OffsetAt(at, 2 * oneword);
		     Emit3(LOAD, "%a,%r", attype, at, NextReg(r, 2));
		  END;
		  ReleaseAt(at);
		  mode := floatRegMode; reg := r;
	       ELSE
		  Assert(attype^.size <= oneword);
		  Emit3(LOAD, "%a,%r", attype, at, r);
		  RestrictedRelease(at, r, (* keepstack = *) FALSE);
		  mode := regMode; reg := r;
	       END;
	 | simm13Mode:
	       Emit3(ORop, "%r,%i,%r", g0, simm13val, r);
	       mode := regMode; reg := r;
	 | cmpMode, condMode:
	       SetBool(at, r);
	 ELSE
	    Assert(FALSE);
	 END;
      END;
   END LoadReg;

   PROCEDURE LoadAddr(at: Attribute);
      (* load address of `at' into a register and
	 convert mode of `at' into addrMode
      *)
      VAR
	 addrReg: Reg;
   BEGIN
      WITH at^ DO
	 IF mode # addrMode THEN
	    IF mode = indexMode THEN
	       IF (reg # noReg) & ~Reserved(reg) THEN
		  addrReg := reg;
	       ELSIF (xreg # noReg) & ~Reserved(xreg) THEN
		  addrReg := xreg;
	       ELSE
		  GetReg(addrReg);
	       END;
	    END;
	    LoadAddrReg(at, addrReg);
	 END;
      END;
   END LoadAddr;

   PROCEDURE LoadAddrReg(at: Attribute; r: Reg);
      (* like LoadAddr with destination register `r' *)
   BEGIN
      WITH at^ DO
	 IF (mode # addrMode) OR (reg # r) THEN
	    CASE mode OF
	    | addrMode:
		  Emit3(ORop, "%r,%r,%r", reg, g0, r);
	    | indexMode:
		  (* four cases: d, r1+d, r1, r1+r2 *)
		  IF xreg # noReg THEN
		     (* r1+r2 *)
		     Emit3(ADD, "%r,%r,%r", reg, xreg, r);
		  ELSIF reg = noReg THEN
		     (* d *)
		     IF (labelip # NIL) OR rlabel.ok THEN
			IF labelip # NIL THEN
			   Emit4(ORop, "%r,%L(%n+%c),%r",
			      g0, labelip, addr, r);
			ELSE
			   Assert(addr = 0);
			   Emit3(ORop, "%r,%L(%l),%r", g0, rlabel, r);
			END;
		     ELSE
			Emit3(ORop, "%r,%i,%r", g0, addr, r);
		     END;
		  ELSIF (addr # 0) OR (labelip # NIL) OR rlabel.ok THEN
		     (* r1+d *)
		     IF (labelip # NIL) OR rlabel.ok THEN
			IF labelip # NIL THEN
			   Emit4(ADD, "%r,%L(%n+%c),%r",
			      reg, labelip, addr, r);
			ELSE
			   Assert(addr = 0);
			   Emit3(ADD, "%r,%L(%l),%r", reg, rlabel, r);
			END;
		     ELSE
			Emit3(ADD, "%r,%i,%r", reg, addr, r);
		     END;
		  ELSE
		     (* r1 *)
		     IF reg # r THEN
			Emit3(ORop, "%r,%r,%r", reg, g0, r);
		     END;
		  END;
	    ELSE
	       Assert(FALSE);
	    END;
	    RestrictedRelease(at, r, (* keepstack = *) TRUE);
	    IF mode = indexMode THEN
	       mode := addrMode;
	    ELSIF mode # addrMode THEN
	       mode := addrMode; tmp := FALSE;
	    END;
	    reg := r;
	 END;
      END;
   END LoadAddrReg;

   PROCEDURE LoadCond(at: Attribute);
      (* set condition codes of `at';
	 at is in condMode afterwards and ccat equals at
      *)
   BEGIN
      WITH at^ DO
	 IF mode = cmpMode THEN
	    IF reg2 = noReg THEN
	       Emit3(SUBcc, "%r,%i,%r", reg1, cmpval, g0);
	       ReleaseReg(reg1);
	    ELSE
	       Emit3(SUBcc, "%r,%r,%r", reg1, reg2, g0);
	       ReleaseReg(reg1); ReleaseReg(reg2);
	    END;
	    mode := condMode; tlabel.ok := FALSE; flabel.ok := FALSE;
	    ccat := at;
	 END;
	 Assert((mode # condMode) OR (ccat = at) OR tlabel.ok OR flabel.ok);
	 IF mode # condMode THEN
	    IF ccat # at THEN
	       Assert(~IsReal(attype));
	       Load(at);
	       Emit3(SUBcc, "%r,%r,%r", reg, g0, g0);
	       ccat := at;
	    END;
	    ReleaseAt(at);
	    mode := condMode;
	    test := ne;
	    atype := ArithType(attype);
	    tlabel.ok := FALSE;
	    flabel.ok := FALSE;
	 END;
      END;
   END LoadCond;

   PROCEDURE OntoStack(at: Attribute);
      (* put the value onto stack if it is not addressable yet *)
      VAR
	 stackOffset: Offset;
	 low, high: Offset;
	 indexReg: Reg;
   BEGIN
      Address(at);
      WITH at^ DO
	 IF mode IN AtModeSet{regMode, simm13Mode, floatRegMode} THEN
	    Load(at);
	    IF mode = regMode THEN
	       StackAlloc(stackOffset, oneword);
	    ELSE
	       StackAlloc(stackOffset, attype^.size);
	    END;
	    LowHigh(stackOffset, low, high);
	    IF high = 0 THEN
	       IF mode = regMode THEN
		  Emit4(STORE, "%r,%[r,d]", attype, reg, base, stackOffset);
		  ReleaseReg(reg);
	       ELSE
		  CASE attype^.size OF
		  |  4: Emit3(STF, "%r,%[r,d]", reg, base, stackOffset);
		  |  8: Emit3(STDF, "%r,%[r,d]", reg, base, stackOffset);
		  | 16: Emit3(STDF, "%r,%[r,d]", reg, base, stackOffset);
			(* offset is simm13 because stackOffset + doubleword
			   is smaller than just stackOffset because of the
			   backward growing stack
			*)
		        Emit3(STDF, "%r,%[r,d]", NextReg(reg, 2), base,
			   stackOffset + doubleword);
		  END;
		  IF Allocated(reg) THEN
		     FreeFloatReg(reg, attype^.size);
		  END;
	       END;
	       mode := indexMode; addr := stackOffset; xreg := noReg;
	    ELSE
	       GetReg(indexReg);
	       LoadConst(indexReg, stackOffset);
	       IF mode = regMode THEN
		  Emit3(ST, "%r,%[r,r]", reg, base, indexReg);
		  ReleaseReg(reg);
	       ELSE
		  CASE attype^.size OF
		  |  4: Emit3(STF, "%r,%[r,r]", reg, base, indexReg);
		  |  8: Emit3(STDF, "%r,%[r,r]", reg, base, indexReg);
		  | 16: Emit3(STDF, "%r,%[r,r]", reg, base, indexReg);
			Emit3(ADD, "%r,%c,%r", indexReg, doubleword, indexReg);
		        Emit3(STDF, "%r,%[r,r]", reg, base, indexReg);
		  END;
		  IF Allocated(reg) THEN
		     FreeFloatReg(reg, attype^.size);
		  END;
	       END;
	       mode := indexMode; addr := 0; xreg := indexReg;
	    END;
	    reg := base; tmp := TRUE; stackoffset := stackOffset;
	    labelip := NIL; rlabel.ok := FALSE;
	 ELSE
	    Assert(mode IN AtModeSet{indexMode, addrMode});
	 END;
      END;
   END OntoStack;

   PROCEDURE AllocOnStack(type: Type; VAR at: Attribute);
      (* return stack allocation represented by at
         which is sufficient to hold a value of the given type
      *)
      VAR
	 stackOffset: Offset;
	 low, high: Offset;
   BEGIN
      Assert(type # NIL);
      StackAlloc(stackOffset, type^.size);
      LowHigh(stackOffset, low, high);
      NewAttribute(at);
      WITH at^ DO
	 link := NIL; attype := type;
	 mode := indexMode;
	 reg := base;
	 tmp := TRUE; stackoffset := stackOffset;
	 labelip := NIL; rlabel.ok := FALSE;
	 IF high = 0 THEN
	    xreg := noReg; addr := stackOffset;
	 ELSE
	    GetReg(xreg); LoadConst(xreg, stackOffset);
	    addr := 0;
	 END;
      END;
   END AllocOnStack;

   PROCEDURE Convert(at: Attribute; dtype: Type);
      (* convert `at' to type `dtype'; at^.attype becomes dtype;
	 `at' may be loaded
      *)
   BEGIN
      Assert((at^.attype # NIL) & (dtype # NIL));
      IF at^.attype^.form # dtype^.form THEN
	 StrEmit("%* MCP4BasicOps.Convert should do some interesting now...");
      END;
   END Convert;

   PROCEDURE DereferenceAt(at: Attribute);
      (* dereferencing of `at'; like '^'-operator *)
      VAR
	 newReg: Reg;
	 origType: Type;
   BEGIN
      WITH at^ DO
	 Address(at);
	 CASE mode OF
	 | regMode:        mode := addrMode; tmp := FALSE;
	 | addrMode:       IF Reserved(reg) THEN
			      GetReg(newReg);
			      Emit3(LOAD, "%a,%r", attype, at, newReg);
			      reg := newReg;
			   ELSE
			      Emit3(LOAD, "%a,%r", attype, at, reg);
			   END;
	 | indexMode:      origType := attype;
			   IF attype^.form = arrays THEN
			      (* fix type to survive assertion in Load *)
			      attype := addrptr;
			   END;
			   Load(at);
			   attype := origType;
			   mode := addrMode; tmp := FALSE;
	 ELSE
	    Assert(FALSE);
	 END;
      END;
   END DereferenceAt;

   PROCEDURE OffsetAt(at: Attribute; offset: Offset);
      (* `at' must have an addressable addressing mode (i.e. not regMode);
	 the address described by `at' is then incremented by `offset';
	 `offset' may be negative
      *)
      VAR
	 low, high: Offset;
	 hreg: Reg;
	 newReg: Reg;
   BEGIN
      IF offset = 0 THEN RETURN END;
      WITH at^ DO
	 Address(at);
	 IF mode = indexMode THEN
	    LoadAddr(at);
	 END;
	 CASE mode OF
	 | addrMode:       LowHigh(offset, low, high);
			   IF high # 0 THEN
			      Emit2(SETHI, "%c,%r", high, tmpreg);
			      IF low # 0 THEN
				 Emit3(ADD, "%r,%i,%r", tmpreg, low, tmpreg);
			      END;
			      IF Reserved(reg) THEN
				 GetReg(newReg);
				 Emit3(ADD, "%r,%r,%r", tmpreg, reg, newReg);
				 reg := newReg;
			      ELSE
				 Emit3(ADD, "%r,%r,%r", tmpreg, reg, reg);
			      END;
			   ELSE
			      mode := indexMode;
			      addr := low;
			      labelip := NIL; rlabel.ok := FALSE;
			      xreg := noReg;
			   END;
	 | regMode:        (* may be used to switch from dyn array address
	                      to high value and vice versa
			   *)
			   Assert((atip # NIL) & IsDyn(attype));
			   Assert(atip^.inReg);
			   Assert(Reserved(reg));
			   CASE offset OF
			   | oneword:  reg := VAL(Reg, ORD(reg) + 1);
			   | -oneword: reg := VAL(Reg, ORD(reg) - 1);
			   ELSE
			      Assert(FALSE);
			   END;
			   Assert(reg IN RegSet{i0..i5});
			   Assert(Reserved(reg));
	 ELSE
	    Assert(FALSE);
	 END;
      END;
   END OffsetAt;

   PROCEDURE IndexAtReg(at: Attribute; indexreg: Reg; scalefactor: CARDINAL);
      (* generate []-operation for `at' with index in `indexreg'
	 `indexreg' is released afterwards
      *)
      VAR
	 dreg: Reg;
	 newReg: Reg;
   BEGIN
      Address(at);
      IF scalefactor # 1 THEN
	 IF Reserved(indexreg) THEN
	    GetReg(newReg);
	    Emit3(ORop, "%r,%r,%r", indexreg, g0, newReg);
	    indexreg := newReg;
	 END;
	 ConstMulReg(intAT, indexreg, scalefactor); scalefactor := 1;
      END;
      WITH at^ DO
	 CASE mode OF
	 | addrMode:    mode := indexMode;
			labelip := NIL; rlabel.ok := FALSE;
			addr := 0; xreg := indexreg;
	 | indexMode:   Assert(reg # noReg);
			IF Reserved(reg) THEN
			   GetReg(dreg);
			ELSE
			   dreg := reg;
			END;
			Emit3(ADD, "%r,%r,%r", reg, indexreg, dreg);
			reg := dreg;
			ReleaseReg(indexreg);
	 ELSE
	    Assert(FALSE);
	 END;
      END;
   END IndexAtReg;

   PROCEDURE ReleaseAt(at: Attribute);
      (* release anything (registers and stack reservations) of `at' *)
   BEGIN
      RestrictedRelease(at, noReg, (* keepstack = *) FALSE);
   END ReleaseAt;

   PROCEDURE ReleaseReg(r: Reg);
      (* free r if it is not reserved *)
   BEGIN
      IF (r # noReg) & ~(r IN RegSet{o0..o5}) & ~Reserved(r) THEN
	 FreeReg(r);
      END;
   END ReleaseReg;

   PROCEDURE RestrictedRelease(at: Attribute; r: Reg; keepstack: BOOLEAN);
      (* release anything (registers and stack reservations or only
         registers) of `at' but not the given register
      *)

      PROCEDURE DropReg(VAR reg: Reg);
      BEGIN
	 IF (reg # noReg) & (reg # r) &
	       ~(reg IN RegSet{o0..o5}) & ~Reserved(reg) THEN
	    FreeReg(reg); reg := noReg;
	 END;
      END DropReg;

   BEGIN (* RestrictedRelease *)
      WITH at^ DO
	 CASE mode OF
	 | regMode:        DropReg(reg);
	 | floatRegMode:   IF reg # r THEN
			      FreeFloatReg(reg, attype^.size);
			   END;
	 | addrMode:       DropReg(reg);
	                   IF ~keepstack & tmp THEN
			      StackFree(stackoffset, attype^.size);
			      tmp := FALSE;
			   END;
	 | indexMode:      DropReg(reg); DropReg(xreg);
	                   IF ~keepstack & tmp THEN
			      StackFree(stackoffset, attype^.size);
			      tmp := FALSE;
			   END;
	 ELSE
	 END;
      END;
   END RestrictedRelease;

   PROCEDURE ReturnAt(VAR at: Attribute);
      (* release anything (registers and stack reservations) of `at' and
	 dispose `at'
      *)
   BEGIN
      RestrictedReturnAt(at, noReg);
   END ReturnAt;

   PROCEDURE RestrictedReturnAt(VAR at: Attribute; r: Reg);
   BEGIN
      RestrictedRelease(at, r, (* keepstack = *) FALSE);
      IF at = ccat THEN
	 ccat := NIL;
      END;
      DISPOSE(at); at := NIL;
   END RestrictedReturnAt;

   PROCEDURE GetPower(VAR power: CARDINAL; VAR mask: CARDINAL;
		      val: CARDINAL) : BOOLEAN;
   BEGIN
      mask := 1;
      FOR power := 1 TO BitsPerWord-1 DO
         mask := mask * 2;
         IF mask = val THEN RETURN TRUE END;
      END;
      RETURN FALSE
   END GetPower;

   PROCEDURE InitAttributes(VAR at1, at2: Attribute;
			    atype: ArithmeticType;
			    r: Reg; value: INTEGER);
      VAR
	 type: Type;
   BEGIN
      CASE atype OF
      | intAT:    type := intptr;
      | cardAT:   type := cardptr;
      END;
      NewAttribute(at1);
      WITH at1^ DO
	 mode := regMode;
	 link := NIL; attype := type;
	 reg := r;
      END;
      NewAttribute(at2);
      WITH at2^ DO
	 mode := constAt;
	 link := NIL; attype := type;
	 atip := NIL; cval.value := value;
      END;
      Address(at2);
   END InitAttributes;

   PROCEDURE ConstMulReg(atype: ArithmeticType; r: Reg; value: INTEGER);
      VAR
	 at1, at2: Attribute;
	 power, mask: CARDINAL;
   BEGIN
      StrEmit2("%* multiply contents of %r by %i", r, value);
      IF value = 0 THEN
	 Emit3(ORop, "%r,%r,%r", g0, g0, r);
      ELSIF value = -1 THEN
	 Emit3(SUB, "%r,%r,%r", g0, r, r);
      ELSIF value # 1 THEN
	 IF GetPower(power, mask, ABS(value)) THEN
	    IF value < 0 THEN
	       Emit3(SUB, "%r,%r,%r", g0, r, r);
	    END;
	    Emit3(SLL, "%r,%c,%r", r, power, r);
	 ELSE
	    InitAttributes(at1, at2, atype, r, value);
	    Mult(atype, at1, at2);
	    WITH at1^ DO
	       Assert((mode = regMode) & (reg = r));
	    END;
	 END;
      END;
   END ConstMulReg;

   PROCEDURE ConstDivReg(atype: ArithmeticType; r: Reg; value: INTEGER);
      VAR
	 at1, at2: Attribute;
	 power, mask: CARDINAL;
   BEGIN
      StrEmit2("%* divide contents of %r by %i", r, value);
      IF value = 0 THEN
	 Error(221);
      ELSIF value = -1 THEN
	 Emit3(SUB, "%r,%r,%r", g0, r, r);
      ELSIF value # 1 THEN
	 IF (atype = cardAT) & GetPower(power, mask, ABS(value)) THEN
	    Emit3(SRL, "%r,%c,%r", r, power, r);
	 ELSE
	    InitAttributes(at1, at2, atype, r, value);
	    Div(atype, at1, at2);
	    WITH at1^ DO
	       Assert((mode = regMode) & (reg = r));
	    END;
	 END;
      END;
   END ConstDivReg;

   PROCEDURE ConstModReg(atype: ArithmeticType; r: Reg; value: INTEGER);
      VAR
	 at1, at2: Attribute;
	 power, mask: CARDINAL;
	 low, high: INTEGER;
   BEGIN
      IF value = 0 THEN
	 Error(221);
      ELSIF ABS(value) = 1 THEN
	 Emit3(ORop, "%r,%r,%r", g0, g0, r);
      ELSIF (atype = cardAT) & GetPower(power, mask, ABS(value)) THEN
	 DEC(mask);
	 LowHigh(mask, low, high);
	 IF high = 0 THEN
	    Emit3(ANDop, "%r,%i,%r", r, mask, r);
	 ELSE
	    Emit3(SLL, "%r,%c,%r", r, BitsPerWord - power, r);
	    Emit3(SRL, "%r,%c,%r", r, BitsPerWord - power, r);
	 END;
      ELSE
	 SaveParamRegs;
	 Emit3(ORop, "%r,%r,%r", r, g0, o0);
	 LoadConst(o1, value);
	 CASE atype OF
	 | intAT:    Emit2(CALL, "%s,%c", ".rem", 2);
	 | cardAT:   Emit2(CALL, "%s,%c", ".urem", 2);
	 END;
	 Emit(NOP, "");
	 Emit3(ORop, "%r,%r,%r", o0, g0, r);
	 RestoreParamRegs;
      END;
   END ConstModReg;

   PROCEDURE Mult(at: ArithmeticType; desat, opat: Attribute);
      (* generate code for `desat' := `desat' * `opat'
	 and release `opat'
	 `desat' is in regMode afterwards
      *)
      VAR
	 srcReg: Reg;
   BEGIN
      Load(desat); srcReg := desat^.reg;
      IF Reserved(srcReg) THEN
	 GetReg(desat^.reg);
      END;
      IF opat^.mode # simm13Mode THEN
	 Load(opat);
      END;
      CASE at OF
      | intAT:    Emit3(SMUL, "%r,%a,%a", srcReg, opat, desat);
      | cardAT:   Emit3(UMUL, "%r,%a,%a", srcReg, opat, desat);
      ELSE
	 Assert(FALSE);
      END;
      ReleaseAt(opat);
   END Mult;

   PROCEDURE Div(at: ArithmeticType; desat, opat: Attribute);
      (* generate code for `desat' := `desat' DIV `opat'
	 and release `opat'
	 `desat' is in regMode afterwards
      *)
      VAR
	 srcReg, tmpReg: Reg;
   BEGIN
      Load(desat); srcReg := desat^.reg;
      IF Reserved(srcReg) THEN
	 GetReg(desat^.reg);
      END;
      IF opat^.mode # simm13Mode THEN
	 Load(opat);
      END;
      CASE at OF
      | intAT:    GetReg(tmpReg);
		  Emit3(SRA, "%r,%c,%r", srcReg, 31, tmpReg);
		  Emit3(WRY, "%r,%r,%r", tmpReg, g0, y);
		  Emit(NOP, ""); Emit(NOP, ""); Emit(NOP, "");
		  Emit3(SDIV, "%r,%a,%a", srcReg, opat, desat);
		  FreeReg(tmpReg);
      | cardAT:   Emit3(WRY, "%r,%r,%r", g0, g0, y);
		  Emit(NOP, ""); Emit(NOP, ""); Emit(NOP, "");
		  Emit3(UDIV, "%r,%a,%a", srcReg, opat, desat);
      ELSE
	 Assert(FALSE);
      END;
      ReleaseAt(opat);
   END Div;

   PROCEDURE Mod(at: ArithmeticType; desat, opat: Attribute);
      (* generate code for `desat' := `desat' MOD `opat'
	 and release `opat'
	 `desat' is in regMode afterwards
      *)
      VAR
	 reg: Reg;
   BEGIN
      SaveParamRegs;
      LoadReg(desat, o0); LoadReg(opat, o1);
      CASE at OF
      | intAT:    Emit2(CALL, "%s,%c", ".rem", 2);
      | cardAT:   Emit2(CALL, "%s,%c", ".urem", 2);
      END;
      Emit(NOP, "");
      GetReg(reg); LoadReg(desat, reg); ReleaseAt(opat);
      RestoreParamRegs;
      (*
      Div(at, desat, opat);
      WITH desat^ DO
	 Assert(mode = regMode);
	 Emit2(RDY, "%r,%r", y, reg);
      END;
      *)
   END Mod;

   PROCEDURE InvertTest(VAR t: TestType);
      (* invert test type (NOT), e.g. `le' becomes `gt' *)
   BEGIN
      CASE t OF
      | lt: t := ge;
      | le: t := gt;
      | eq: t := ne;
      | ne: t := eq;
      | ge: t := lt;
      | gt: t := le;
      | always: t := never;
      | never: t := always;
      END;
   END InvertTest;

   PROCEDURE ReverseTest(VAR t: TestType);
      (* reverse test type (exchange of operands), e.g. `le' becomes `ge' *)
   BEGIN
      CASE t OF
      | lt: t := gt;
      | le: t := ge;
      | eq, ne, always, never: (* ok *)
      | ge: t := le;
      | gt: t := lt;
      END;
   END ReverseTest;

   PROCEDURE GenTest(t: TestType; atype: ArithmeticType; dest: Label);
      (* condition codes are set; generate code for branching
	 to `dest' if `t' is true, i.e. "bcc test,dest";
	 warning: GenTest does not fill the delay slot
      *)
      VAR
	 bm: Mnemonic;
   BEGIN
      IF t = always THEN
	 Emit1(BA, "%l", dest);
      ELSIF t # never THEN
	 IF (atype IN ArithmeticTypeSet{float32AT..float128AT}) THEN
	    (* exception on unordered! *)
	    CASE t OF
	    | lt: bm := FBL;
	    | le: bm := FBLE;
	    | eq: bm := FBE;
	    | ne: bm := FBNE;
	    | ge: bm := FBGE;
	    | gt: bm := FBG;
	    END;
	 ELSIF (atype = logAT) OR (atype = cardAT) THEN
	    (* unsigned *)
	    CASE t OF
	    | lt: bm := BCS;
	    | le: bm := BLEU;
	    | eq: bm := BE;
	    | ne: bm := BNE;
	    | ge: bm := BCC;
	    | gt: bm := BGU;
	    END;
	 ELSE
	    (* signed *)
	    CASE t OF
	    | lt: bm := BL;
	    | le: bm := BLE;
	    | eq: bm := BE;
	    | ne: bm := BNE;
	    | ge: bm := BGE;
	    | gt: bm := BG;
	    END;
	 END;
	 Emit1(bm, "%l", dest);
      END;
   END GenTest;

   PROCEDURE SetBool(at: Attribute; destreg: Reg);
      (* set destreg according to condition codes *)
      VAR
	 trueLabel, endLabel: Label;
   BEGIN
      WITH at^ DO
	 Assert((mode = condMode) OR (mode = cmpMode));
	 IF mode = condMode THEN
	    GetLabel(endLabel);
	    IF tlabel.ok THEN
	       GenTest(test, atype, tlabel); Emit(NOP, "");
	       IF flabel.ok THEN
		  EmitLabel(flabel);
	       END;
	       Emit1(BA, "%l", endLabel);
	       Emit3(ORop, "%r,%r,%r", g0, g0, destreg); (* delay slot *)
	       EmitLabel(tlabel);
	       Emit3(ORop, "%r,%c,%r", g0, 1, destreg);
	    ELSIF flabel.ok THEN
	       InvertTest(test);
	       GenTest(test, atype, flabel); Emit(NOP, "");
	       IF tlabel.ok THEN
		  EmitLabel(tlabel);
	       END;
	       Emit1(BA, "%l", endLabel);
	       Emit3(ORop, "%r,%c,%r", g0, 1, destreg); (* delay slot *)
	       EmitLabel(flabel);
	       Emit3(ORop, "%r,%r,%r", g0, g0, destreg);
	    ELSE
	       GetLabel(trueLabel);
	       GenTest(test, atype, trueLabel); Emit(NOP, "");
	       Emit1(BA, "%l", endLabel);
	       Emit3(ORop, "%r,%r,%r", g0, g0, destreg); (* delay slot *)
	       EmitLabel(trueLabel);
	       Emit3(ORop, "%r,%c,%r", g0, 1, destreg);
	    END;
	    EmitLabel(endLabel);
	 ELSE
	    IF cmpval # 0 THEN
	       GetReg(reg2);
	       Emit3(ORop, "%r,%c,%r", g0, cmpval, reg2);
	       cmpval := 0;
	    END;
	    Assert(reg2 # noReg);
	    (* following code sequences have been
	       returned by the GNU super optimizer
	    *)
	    IF test = eq THEN
	       Emit3(SUBcc, "%r,%r,%r", reg2, reg1, tmpreg);
	       Emit3(ADDXcc, "%r,%i,%r", tmpreg, -1, g0);
	       Emit3(SUBX, "%r,%i,%r", g0, -1, destreg);
	    ELSIF test = ne THEN
	       Emit3(SUBcc, "%r,%r,%r", reg2, reg1, tmpreg);
	       Emit3(ADDXcc, "%r,%i,%r", tmpreg, -1, g0);
	       Emit3(ADDX, "%r,%r,%r", g0, g0, destreg);
	    ELSE
	       IF atype = intAT THEN
		  CASE test OF
		  | lt:    Emit3(SUBcc, "%r,%r,%r", reg1, reg2, g0);
			   Emit3(SRA, "%r,%c,%r", reg2, 31, tmpreg);
			   Emit3(SRL, "%r,%c,%r", reg1, 31, destreg);
			   Emit3(ADDX, "%r,%r,%r", destreg, tmpreg, destreg);
		  | le:    Emit3(SUBcc, "%r,%r,%r", reg2, reg1, g0);
		           Emit3(XOR, "%r,%r,%r", reg2, reg1, destreg);
			   Emit3(ADDXcc, "%r,%r,%r", destreg, destreg, destreg);
			   Emit3(ADDXcc, "%r,%i,%r", destreg, -1, destreg);
			   Emit3(ANDop, "%r,%c,%r", destreg, 1, destreg);
		  | ge:    Emit3(SUBcc, "%r,%r,%r", reg1, reg2, g0);
		           Emit3(XOR, "%r,%r,%r", reg2, reg1, destreg);
			   Emit3(ADDXcc, "%r,%r,%r", destreg, destreg, destreg);
			   Emit3(ADDXcc, "%r,%i,%r", destreg, -1, destreg);
			   Emit3(ANDop, "%r,%c,%r", destreg, 1, destreg);
		  | gt:    Emit3(SUBcc, "%r,%r,%r", reg2, reg1, g0);
		           Emit3(SRL, "%r,%c,%r", reg2, 31, tmpreg);
			   Emit3(SRA, "%r,%c,%r", reg1, 31, destreg);
			   Emit3(ADDX, "%r,%r,%r", destreg, tmpreg, destreg);
		  END;
	       ELSE
		  CASE test OF
		  | lt:    Emit3(SUBcc, "%r,%r,%r", reg1, reg2, g0);
		           Emit3(ADDX, "%r,%r,%r", g0, g0, destreg);
		  | le:    Emit3(SUBcc, "%r,%r,%r", reg2, reg1, g0);
		           Emit3(SUBX, "%r,%c,%r", g0, -1, destreg);
		  | ge:    Emit3(SUBcc, "%r,%r,%r", reg1, reg2, g0);
		           Emit3(SUBX, "%r,%c,%r", g0, -1, destreg);
		  | gt:    Emit3(SUBcc, "%r,%r,%r", reg2, reg1, g0);
		           Emit3(ADDX, "%r,%r,%r", g0, g0, destreg);
		  END;
	       END;
	    END;
	    IF reg1 # destreg THEN ReleaseReg(reg1) END;
	    IF reg2 # destreg THEN ReleaseReg(reg2) END;
	    ccat := NIL;
	 END;
	 mode := regMode; reg := destreg;
      END;
   END SetBool;

   PROCEDURE MoveBytes(from, to: Reg; nbytes: Size);
      (* addresses are in `from' and `to'; both registers
	 are released afterwards
      *)
      VAR
	 count: CARDINAL;
	 countReg: Reg;
   BEGIN
      Assert((nbytes = 1) OR (nbytes = 2) OR (nbytes MOD oneword = 0));
      IF nbytes = 1 THEN
	 Emit2(LDUB, "%[r],%r", from, tmpreg);
	 Emit2(STB, "%r,%[r]", tmpreg, to);
      ELSIF nbytes = 2 THEN
	 Emit2(LDUH, "%[r],%r", from, tmpreg);
	 Emit2(STH, "%r,%[r]", tmpreg, to);
      ELSIF nbytes <= oneword * 3 THEN
	 Emit2(LD, "%[r],%r", from, tmpreg);
	 Emit2(ST, "%r,%[r]", tmpreg, to);
	 count := nbytes DIV oneword - 1;
	 WHILE count > 0 DO
	    Emit3(LD, "%[r,d],%r", from, count * oneword, tmpreg);
	    Emit3(ST, "%r,%[r,d]", tmpreg, to, count * oneword);
	    DEC(count);
	 END;
      ELSE
	 GetReg(countReg); LoadConst(countReg, nbytes);
	 MoveVarBytes(from, to, countReg);
	 from := noReg; to := noReg;
      END;
      ReleaseReg(from); ReleaseReg(to);
   END MoveBytes;

   PROCEDURE MoveVarBytes(from, to, nbytes: Reg);
      (* like MoveBytes but with variable size;
	 nbytes must be a multiple of oneword;
         all registers are released afterwards
      *)
      VAR
	 loopLabel: Label;
   BEGIN
      GetLabel(loopLabel); EmitLabel(loopLabel);
      Assert(~Reserved(nbytes));
      Emit3(SUBcc, "%r,%c,%r", nbytes, oneword, nbytes);
      Emit3(LD, "%[r,r],%r", from, nbytes, tmpreg);
      Emit1(BG, "%l", loopLabel);
      Emit3(ST, "%r,%[r,r]", tmpreg, to, nbytes);
      ReleaseReg(from); ReleaseReg(to); ReleaseReg(nbytes);
      ccat := NIL;
   END MoveVarBytes;

   PROCEDURE MoveBytesAt(from, to: Attribute; nbytes: Size);
      (* both attributes are released afterwards *)
   BEGIN
      Address(from); Address(to);
      IF (nbytes MOD oneword = 0) & (nbytes <= 3 * oneword) THEN
	 WHILE nbytes > 0 DO
	    Emit2(LD, "%a,%r", from, tmpreg);
	    Emit2(ST, "%r,%a", tmpreg, to);
	    OffsetAt(from, oneword); OffsetAt(to, oneword);
	    DEC(nbytes, oneword);
	 END;
	 ReleaseAt(from); ReleaseAt(to);
      ELSE
	 LoadAddr(from); LoadAddr(to);
	 MoveBytes(from^.reg, to^.reg, nbytes);
	 (* don't call ReleaseAt here -- registers are released by MoveBytes *)
      END;
   END MoveBytesAt;

   PROCEDURE LoadPosParams;
   BEGIN
      Emit3(SETHI, "%H(%n%s),%r", mainmodp, ".N", o0);
      Emit4(ORop, "%r,%L(%n%s),%r", o0, mainmodp, ".N", o0);
      LoadConst(o1, line);
   END LoadPosParams;

   PROCEDURE CompareWith(reg: Reg; const: INTEGER);
      VAR
	 low, high: INTEGER;
	 constReg: Reg;
   BEGIN
      LowHigh(const, low, high);
      IF high = 0 THEN
	 Emit3(SUBcc, "%r,%i,%r", reg, const, g0);
      ELSE
	 GetReg(constReg);
	 LoadConst(constReg, const);
	 Emit3(SUBcc, "%r,%r,%r", reg, constReg, g0);
	 FreeReg(constReg);
      END;
   END CompareWith;

   PROCEDURE SignedRangeCheck(at: Attribute; lowerBound, upperBound: INTEGER);
      (* at^.mode = regMode;
	 check at^.reg for being inside [lowerBound..upperBound]
      *)
      VAR
	 failureLab, okLab: Label;
   BEGIN
      IF (lowerBound = MIN(INTEGER)) & (upperBound = MAX(INTEGER)) THEN
	 RETURN
      END;
      StrEmit3("%* signed range check for %a in [%i..%i]",
	 at, lowerBound, upperBound);
      GetLabel(failureLab); GetLabel(okLab);
      WITH at^ DO
	 Assert(mode = regMode);
	 IF lowerBound # MIN(INTEGER) THEN
	    CompareWith(reg, lowerBound);
	    IF upperBound # MAX(INTEGER) THEN
	       GenTest(lt, intAT, failureLab);
	    ELSE
	       GenTest(ge, intAT, okLab);
	    END;
	    Emit(NOP, "");
	 END;
	 IF upperBound # MAX(INTEGER) THEN
	    CompareWith(reg, upperBound);
	    GenTest(le, intAT, okLab); Emit(NOP, "");
	 END;
      END;
      EmitLabel(failureLab);
      LoadPosParams;
      LoadConst(o3, upperBound);
      IF lowerBound = 0 THEN
	 Emit2(CALL, "%_%s,%c", ".signed0", 5);
      ELSE
	 LoadConst(o2, lowerBound);
	 Emit2(CALL, "%_%s,%c", ".signed", 5);
      END;
      Emit3(ORop, "%r,%r,%r", at^.reg, g0, o4);
      EmitLabel(okLab);
      ccat := NIL;
   END SignedRangeCheck;

   PROCEDURE UnsignedRangeCheck(at: Attribute;
                                lowerBound, upperBound: CARDINAL);
      (* at^.mode = regMode;
	 check at^.reg for being inside [lowerBound..upperBound]
      *)
      VAR
	 failureLab, okLab: Label;
   BEGIN
      StrEmit3("%* unsigned range check for %a in [%c..%c]",
	 at, lowerBound, upperBound);
      IF (lowerBound = MIN(CARDINAL)) & (upperBound = MAX(CARDINAL)) THEN
	 RETURN
      END;
      GetLabel(failureLab); GetLabel(okLab);
      WITH at^ DO
	 Assert(mode = regMode);
	 IF lowerBound # MIN(CARDINAL) THEN
	    CompareWith(reg, INTEGER(lowerBound));
	    IF upperBound # MAX(CARDINAL) THEN
	       GenTest(lt, cardAT, failureLab); Emit(NOP, "");
	    ELSE
	       GenTest(ge, cardAT, okLab); Emit(NOP, "");
	    END;
	 END;
	 IF upperBound # MAX(CARDINAL) THEN
	    CompareWith(reg, INTEGER(upperBound));
	    GenTest(le, cardAT, okLab); Emit(NOP, "");
	 END;
      END;
      EmitLabel(failureLab);
      LoadPosParams;
      LoadConst(o3, Offset(upperBound));
      IF lowerBound = 0 THEN
	 Emit2(CALL, "%_%s,%c", ".unsigned0", 5);
      ELSE
	 LoadConst(o2, lowerBound);
	 Emit2(CALL, "%_%s,%c", ".unsigned", 5);
      END;
      Emit3(ORop, "%r,%r,%r", at^.reg, g0, o4);
      EmitLabel(okLab);
      ccat := NIL;
   END UnsignedRangeCheck;

   PROCEDURE DynArrayCheck(indexat, highat: Attribute);
      (* check indexat for being inside [0..highat] *)
      VAR
	 okLab: Label;
   BEGIN
      StrEmit2("%* dyn array check for %a in [0..%a]",
	 indexat, highat);
      Load(indexat); Address(highat);
      IF highat^.mode # simm13Mode THEN
	 Load(highat);
      END;
      Emit3(SUBcc, "%r,%a,%r", indexat^.reg, highat, g0);
      GetLabel(okLab);
      GenTest(le, cardAT, okLab); Emit(NOP, "");
      LoadPosParams;
      LoadReg(highat, o3);
      Emit2(CALL, "%_%s,%c", ".dyn", 5);
      Emit3(ORop, "%r,%r,%r", indexat^.reg, g0, o4);
      EmitLabel(okLab);
      ReturnAt(highat);
      ccat := NIL;
   END DynArrayCheck;

   PROCEDURE ConversionCheck(at: Attribute);
      (* at^.attype = intptr or cardptr
	 check at for being convertable from INTEGER to CARDINAL
	 or vice versa
      *)
      VAR
	 okLab: Label;
   BEGIN
      StrEmit1("%* conversion check for %a", at);
      Load(at);
      Emit3(SUBcc, "%r,%r,%r", at^.reg, g0, g0);
      GetLabel(okLab);
      GenTest(ge, intAT, okLab); Emit(NOP, "");
      LoadPosParams;
      Emit2(CALL, "%_%s,%c", ".conv", 5);
      Emit3(ORop, "%r,%r,%r", at^.reg, g0, o4);
      EmitLabel(okLab);
      ccat := NIL;
   END ConversionCheck;

   PROCEDURE CheckAgainst(at: Attribute; dtype: Type);
      (* check that at can be safely converted to type *)
      CONST
	 maxint = CARDINAL(MAX(INTEGER));
      TYPE
	 Form = (intR, cardR, bothR, otherR);
	 Range =
	    RECORD
	       CASE form: Form OF
	       | cardR, bothR:
		     min, max: CARDINAL;
	       | intR:
		     imin, imax: INTEGER;
	       END;
	    END;
      VAR
	 stype: Type;
	 srange, drange: Range;
	 ilow, ihigh: INTEGER;
	 low, high: CARDINAL;

      PROCEDURE GetRange(type: Type; VAR range: Range);
      BEGIN
	 WITH range DO
	    CASE type^.form OF
	    | ints, longints:
		  form := intR;
		  imin := MIN(INTEGER); imax := MAX(INTEGER);
	    | cards, longcards:
		  form := cardR;
		  min := MIN(CARDINAL); max := MAX(CARDINAL);
	    | chars:
		  form := cardR;
		  min := 0; max := ORD(MAX(CHAR));
	    | enums:
		  form := bothR;
		  min := 0; max := type^.cstnr;
	    | bools:
		  form := bothR;
		  min := 0; max := 1;
	    | subranges:
		  GetRange(type^.scalp, range);
		  min := type^.min; max := type^.max;
		  IF (form = intR) & (imin >= 0) OR
		        (form = cardR) & (max <= maxint) THEN
		     form := bothR;
		  END;
	    ELSE
	       form := otherR;
	    END;
	 END;
      END GetRange;

   BEGIN (* CheckAgainst *)
      stype := at^.attype;
      IF stype = dtype THEN RETURN END;
      GetRange(stype, srange); GetRange(dtype, drange);
      IF (srange.form = otherR) OR (drange.form = otherR) THEN
	 RETURN
      END;
      IF (srange.form = intR) & (drange.form = cardR) OR
	    (drange.form = intR) & (srange.form = cardR) THEN
	 ConversionCheck(at);
	 WITH srange DO
	    IF form = intR THEN
	       imin := 0;
	    ELSE
	       max := maxint;
	    END;
	    form := bothR;
	 END;
      END;
      IF drange.form = intR THEN
	 (* signed range checks *)
	 IF srange.imin < drange.imin THEN
	    ilow := drange.imin;
	 ELSE
	    ilow := MIN(INTEGER); (* do not check lower bound *)
	 END;
	 IF srange.imax > drange.imax THEN
	    ihigh := drange.imax;
	 ELSE
	    ihigh := MAX(INTEGER); (* do not check upper bound *)
	 END;
	 Load(at); SignedRangeCheck(at, ilow, ihigh);
      ELSE
	 (* unsigned range checks *)
	 IF srange.min < drange.min THEN
	    low := drange.min;
	 ELSE
	    low := MIN(CARDINAL); (* do not check lower bound *)
	 END;
	 IF srange.max > drange.max THEN
	    high := drange.max;
	 ELSE
	    high := MAX(CARDINAL); (* do not check upper bound *)
	 END;
	 Load(at); UnsignedRangeCheck(at, low, high);
      END;
   END CheckAgainst;

   PROCEDURE GenReturn;
   BEGIN
      Emit(RET, ""); Emit(RESTORE, "");
   END GenReturn;

END MCP4BasicOps.
