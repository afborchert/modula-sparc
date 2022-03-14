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
   $Id: MCP4Registe.m2,v 0.1 1997/02/21 18:40:32 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Registe.m2,v $
   Revision 0.1  1997/02/21  18:40:32  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)
IMPLEMENTATION MODULE MCP4Register; (* AFB 3/96 *)

   FROM MCBase IMPORT Size, Offset, oneword;
   FROM MCMnemonics IMPORT ST, LD;
   FROM MCP4Attributes IMPORT Reg, RegSet, FloatType,
      top, base, return, global, tmpreg;
   FROM MCP4CodeSys IMPORT Emit3, StrEmit1;
   FROM MCP4Global IMPORT Assert, Error;
   FROM MCP4Stack IMPORT StackAlloc, StackFree;
   IMPORT MCP4Attributes;

   (* (* exported from definition *)
   TYPE
      FloatReg = [f0..f31];
      RegRange = [MIN(Reg)..MAX(Reg)];
      FloatRegRange = [MIN(FloatReg)..MAX(FloatReg)];
   *)

   CONST
      fregs = RegSet{f0..f31};
      dfregs = RegSet{
	 f0, f2, f4, f6, f8, f10, f12, f14, f16,
	 f18, f20, f22, f24, f26, f28, f30
      };
      qfregs = RegSet{f0, f4, f8, f12, f16, f20, f24, f28};
      volatileRegs = RegSet{g1..g7, f0..f31};
   TYPE
      Status =
	 (permanentReserved, tmpReserved, paramReg, withReg, locked, free);
   VAR
      regtab: ARRAY Reg OF Status;
      freglen: ARRAY FloatReg OF Size;
      alloctab: ARRAY Reg OF Offset; (* stack offsets for saved regs *)
      outOfRegs: BOOLEAN; (* did we run out of registers? *)
      regsSaved: BOOLEAN; (* are we inside SaveRegs .. RestoreRegs? *)
      paramsSaved: BOOLEAN;
	 (* are we inside SaveParamRegs .. RestoreParamRegs? *)

   PROCEDURE OutOfRegs;
      (* implementation restriction: we run out of registers *)
   BEGIN
      IF ~outOfRegs THEN
	 Error(204); outOfRegs := TRUE;
      END;
   END OutOfRegs;

   PROCEDURE InitRegTab;
      CONST
         permanentReservedRegs = RegSet{
	    g0, top, base, return, global, tmpreg,
	    noReg, y, fsr
	 };
      VAR
	 reg: Reg;
   BEGIN
      FOR reg := MIN(Reg) TO MAX(Reg) DO
	 IF reg IN permanentReservedRegs THEN
	    regtab[reg] := permanentReserved;
	 ELSE
	    regtab[reg] := free;
	 END;
      END;
      outOfRegs := FALSE;
      regsSaved := FALSE;
      paramsSaved := FALSE;
   END InitRegTab;

   PROCEDURE GetReg(VAR r: Reg);
      (* post: r IN RegSet{g1..l7} *)
      VAR
	 done: BOOLEAN;
   BEGIN
      GetRegOutOf(r, RegSet{i0..i7}, done); IF done THEN RETURN END;
      GetRegOutOf(r, RegSet{l0..l7}, done); IF done THEN RETURN END;
      GetRegOutOf(r, RegSet{g0..g7}, done); IF done THEN RETURN END;
      OutOfRegs;
      r := g1; (* well, we try to survive it... *)
   END GetReg;

   PROCEDURE GetRegOutOf(VAR r: Reg; regset: RegSet; VAR done: BOOLEAN);
      VAR
	 reg: Reg;
   BEGIN
      Assert(~regsSaved);
      FOR reg := MIN(Reg) TO MAX(Reg) DO
	 IF (reg IN regset) & (regtab[reg] = free) THEN
	    regtab[reg] := locked;
	    r := reg;
	    done := TRUE;
	    RETURN
	 END;
      END;
      done := FALSE;
      RETURN
   END GetRegOutOf;

   PROCEDURE RegRequest(r: Reg; VAR done: BOOLEAN);
      (* try to allocate the given register *)
   BEGIN
      Assert(~regsSaved);
      IF regtab[r] = free THEN
	 regtab[r] := locked;
	 done := TRUE;
      ELSE
	 done := FALSE;
      END;
   END RegRequest;

   PROCEDURE Reserve(r: Reg);
      (* reserve given register until end of block *)
   BEGIN
      Assert(r IN RegSet{l0..i7});
      Assert(regtab[r] = free);
      regtab[r] := tmpReserved;
   END Reserve;

   PROCEDURE WithReg(r: Reg);
      (* declare this allocated (but not yet reserved) register
	 as reserved (needed for WITH references);
	 this register must be still freed by FreeReg
      *)
   BEGIN
      Assert(r IN RegSet{g0..i7});
      Assert(regtab[r] = locked);
      regtab[r] := withReg;
   END WithReg;

   PROCEDURE Reserved(r: Reg) : BOOLEAN;
      (* return TRUE if r is reserved; register which are
         not reserved may be returned by FreeReg/FreeFloatReg
      *)
   BEGIN
      RETURN (regtab[r] = permanentReserved) OR
             (regtab[r] = tmpReserved) OR
	     (regtab[r] = withReg) OR
	     (regtab[r] = paramReg)
   END Reserved;

   PROCEDURE Allocated(r: Reg) : BOOLEAN;
      (* return TRUE if r may be passed to FreeReg *)
   BEGIN
      RETURN regtab[r] = locked
   END Allocated;

   PROCEDURE FreeReg(r: Reg);
   BEGIN
      Assert(~regsSaved);
      Assert(outOfRegs OR (regtab[r] = locked) OR (regtab[r] = withReg));
      Assert(r IN RegSet{g0..g7, l0..l7, i0..i7});
      regtab[r] := free;
   END FreeReg;

   PROCEDURE ParamReg(r: Reg);
      (* declare this register as used parameter register;
	 r must be in RegSet{o0..o5}
      *)
   BEGIN
      Assert(r IN RegSet{o0..o5});
      Assert(regtab[r] = free);
      regtab[r] := paramReg;
   END ParamReg;

   PROCEDURE FreeParamRegs;
      (* undo all previous ParamReg calls *)
      VAR
	 r: Reg;
   BEGIN
      FOR r := o0 TO o5 DO
	 regtab[r] := free;
      END;
   END FreeParamRegs;

   PROCEDURE SeekFloatRegs(VAR r: Reg; length: CARDINAL);
      VAR
	 ordr: CARDINAL;
	 allfree: BOOLEAN;
	 slot: CARDINAL;
	 reg: Reg;
   BEGIN
      ordr := ORD(f4); (* f0..f3 are reserved for return values *)
      WHILE ordr + length - 1 <= ORD(MAX(FloatReg)) DO
	 allfree := TRUE; slot := 0;
	 WHILE allfree & (slot < length) DO
	    reg := VAL(Reg, ordr + slot);
	    IF regtab[reg] # free THEN
	       allfree := FALSE;
	    END;
	    INC(slot);
	 END;
	 IF allfree THEN
	    r := VAL(Reg, ordr);
	    slot := 0;
	    WHILE slot < length DO
	       reg := VAL(Reg, ordr + slot);
	       regtab[reg] := locked;
	       freglen[reg] := 0;
	       INC(slot);
	    END;
	    freglen[r] := length;
	    RETURN
	 END;
	 INC(ordr, length);
      END;
      OutOfRegs;
      r := f0; (* well, we try to survive it... *)
   END SeekFloatRegs;

   PROCEDURE GetFloatReg(VAR r: Reg; size: Size);
      (* post: r in f0..f31 *)
   BEGIN
      Assert(~regsSaved);
      CASE size OF
      |  4: SeekFloatRegs(r, 1);
      |  8: SeekFloatRegs(r, 2);
      | 16: SeekFloatRegs(r, 4);
      END;
   END GetFloatReg;

   PROCEDURE FreeFloatReg(r: Reg; size: Size);
      VAR
	 length: CARDINAL;
	 ordr: CARDINAL;
	 reg: Reg;
   BEGIN
      Assert(~regsSaved);
      CASE size OF
      |  4: length := 1; Assert(r IN fregs);
      |  8: length := 2; Assert(r IN dfregs);
      | 16: length := 4; Assert(r IN qfregs);
      END;
      Assert(freglen[r] = length);
      FOR ordr := ORD(r) TO ORD(r) + length - 1 DO
	 reg := VAL(Reg, ordr);
	 Assert(outOfRegs OR (regtab[reg] = locked));
	 regtab[reg] := free;
      END;
   END FreeFloatReg;

   PROCEDURE BeginBlock;
      (* forget any information about returned registers *)
   BEGIN
      InitRegTab;
   END BeginBlock;

   PROCEDURE EndBlock;
      (* check that all registers has been released *)
      VAR
	 reg: Reg;
	 allReleased: BOOLEAN;
   BEGIN
      Assert(~regsSaved); Assert(~paramsSaved); allReleased := TRUE;
      FOR reg := MIN(Reg) TO MAX(Reg) DO
	 IF regtab[reg] = tmpReserved THEN
	    regtab[reg] := free;
	 ELSE
	    IF regtab[reg] = locked THEN
	       StrEmit1("%* unreleased register: %r", reg);
	       allReleased := FALSE;
	    END;
	 END;
      END;
      Assert(allReleased);
   END EndBlock;

   (* note that SaveRegs & RestoreRegs must not be nested;
      no registers may be allocated between SaveRegs and RestoreRegs
   *)

   PROCEDURE SaveRegs;
      (* save currently used registers *)
      VAR
	 reg: Reg;
	 offset: Offset;
   BEGIN
      Assert(~regsSaved);
      FOR reg := MIN(Reg) TO MAX(Reg) DO
	 IF (reg IN volatileRegs) &
	       ((regtab[reg] = locked) OR (regtab[reg] = withReg)) THEN
	    StackAlloc(offset, oneword);
	    Emit3(ST, "%r,%[r,d]", reg, base, offset);
	    alloctab[reg] := offset;
	 END;
      END;
      regsSaved := TRUE;
   END SaveRegs;

   PROCEDURE RestoreRegs;
      (* restore registers saved previously *)
      VAR
	 reg: Reg;
	 offset: Offset;
   BEGIN
      Assert(regsSaved);
      FOR reg := MIN(Reg) TO MAX(Reg) DO
	 IF (reg IN volatileRegs) &
	       ((regtab[reg] = locked) OR (regtab[reg] = withReg)) THEN
	    offset := alloctab[reg];
	    Emit3(LD, "%[r,d],%r", base, offset, reg);
	    StackFree(offset, oneword);
	 END;
      END;
      regsSaved := FALSE;
   END RestoreRegs;

   PROCEDURE SaveParamRegs;
      (* save currently used parameter registers *)
      VAR
	 reg: Reg;
	 offset: Offset;
   BEGIN
      Assert(~paramsSaved);
      FOR reg := o0 TO o5 DO
	 IF regtab[reg] = paramReg THEN
	    StackAlloc(offset, oneword);
	    Emit3(ST, "%r,%[r,d]", reg, base, offset);
	    alloctab[reg] := offset;
	 END;
      END;
      paramsSaved := TRUE;
   END SaveParamRegs;

   PROCEDURE RestoreParamRegs;
      (* restore currently used parameter registers *)
      VAR
	 reg: Reg;
	 offset: Offset;
   BEGIN
      Assert(paramsSaved);
      FOR reg := o0 TO o5 DO
	 IF regtab[reg] = paramReg THEN
	    offset := alloctab[reg];
	    Emit3(LD, "%[r,d],%r", base, offset, reg);
	    StackFree(offset, oneword);
	 END;
      END;
      paramsSaved := FALSE;
   END RestoreParamRegs;

END MCP4Register.
