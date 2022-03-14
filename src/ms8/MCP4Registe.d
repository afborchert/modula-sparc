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
   $Id: MCP4Registe.d,v 0.1 1997/02/21 18:40:14 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Registe.d,v $
   Revision 0.1  1997/02/21  18:40:14  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP4Register; (* AFB 3/96 *)

   FROM MCBase IMPORT Size;
   FROM MCP4Attributes IMPORT Reg, RegSet, FloatType;
   IMPORT MCP4Attributes;

   TYPE
      FloatReg = [f0..f31];
      RegRange = [MIN(Reg)..MAX(Reg)];
      FloatRegRange = [MIN(FloatReg)..MAX(FloatReg)];

   PROCEDURE GetReg(VAR r: Reg);
      (* post: r IN RegSet{g1..l7} *)

   PROCEDURE GetRegOutOf(VAR r: Reg; regset: RegSet; VAR done: BOOLEAN);

   PROCEDURE RegRequest(r: Reg; VAR done: BOOLEAN);
      (* try to allocate the given register *)

   PROCEDURE Reserve(r: Reg);
      (* reserve given register until end of block *)

   PROCEDURE WithReg(r: Reg);
      (* declare this allocated (but not yet reserved) register
	 as reserved (needed for WITH references);
	 this register must be still freed by FreeReg
      *)

   PROCEDURE ParamReg(r: Reg);
      (* declare this register as used parameter register;
	 r must be in RegSet{o0..o5}
      *)

   PROCEDURE FreeParamRegs;
      (* undo all previous ParamReg calls *)

   PROCEDURE Reserved(r: Reg) : BOOLEAN;
      (* return TRUE if r is reserved; register which are
         not reserved may be returned by FreeReg/FreeFloatReg
      *)

   PROCEDURE Allocated(r: Reg) : BOOLEAN;
      (* return TRUE if r may be passed to FreeReg *)

   PROCEDURE FreeReg(r: Reg);

   PROCEDURE GetFloatReg(VAR r: Reg; size: Size);
      (* post: r in f0..f31 *)

   PROCEDURE FreeFloatReg(r: Reg; size: Size);

   PROCEDURE BeginBlock;
      (* forget any information about returned registers *)

   PROCEDURE EndBlock;
      (* check that all registers has been released *)

   (* note that SaveRegs & RestoreRegs must not be nested;
      no registers may be allocated between SaveRegs and RestoreRegs
   *)

   PROCEDURE SaveRegs;
      (* save currently used registers *)

   PROCEDURE RestoreRegs;
      (* restore registers saved previously *)

   PROCEDURE SaveParamRegs;
      (* save currently used parameter registers *)

   PROCEDURE RestoreParamRegs;
      (* restore currently used parameter registers *)

END MCP4Register.
