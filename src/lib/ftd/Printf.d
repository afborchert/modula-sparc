(* Ulm's Modula-2 Library
   Copyright (C) 1984-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm's Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: Printf.d,v 0.2 1997/02/28 16:00:01 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Printf.d,v $
   Revision 0.2  1997/02/28  16:00:01  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:36  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

(*
 *   Printf - formatted output  (ws 6/88)
 *   =====================================
 *
 *)

DEFINITION MODULE Printf;

   FROM SYSTEM IMPORT BYTE;
   FROM LongStrings IMPORT Long;

   (* ---- program exit codes ----  *)

   CONST 
      Pabort     = 201;
      Fabort     = 202;      (* standard exit codes of the indicated modules *)
      Sabort     = 203;
      Eabort     = 204;
      Labort     = 205;
      WabortP    = 206;
      WabortA    = 207;
      PanicAbort = 255;

      (* --- error handling modes (for procedures "setmode") --- *)

   CONST 
      TellSyntax  = 1;
      TellOutput  = 2;
      AbortSyntax = 3;
      AbortOutput = 4;
      Default     = {TellSyntax, TellOutput, AbortSyntax, AbortOutput};

      (* --- values returned by procedures "success" --- *)

   TYPE 
      FmtExitCode = ( Success, Undefined, FmtPanic, IllegalWidth,
         TooFewFormElems, TooManyFormElems, TooManyPercentOrStar,
         IllegalConvChar, MinHat0Comb, BslashAnd0, BadOctalChar, AllocFailed,
         StringTooSmall, CannotWriteFile, CannotWriteStderr, CannotWriteStdout
         , IllegalWindowOffset, WindowTooSmall, CannotFlushWindow,
         CannotAccessWindow, CannotWriteWindow);

      FmtExitSet  = SET OF FmtExitCode;

   CONST 
      SynError = FmtExitSet {IllegalWidth .. BadOctalChar};
      OutError = FmtExitSet {AllocFailed .. CannotWriteWindow};

   PROCEDURE ErrorReaction(reason : FmtExitCode; mode : BITSET; no : CARDINAL;
      VAR exit : INTEGER; fmt : ARRAY OF CHAR);

   PROCEDURE Printf (output : Long; no : CARDINAL; fmt: ARRAY OF CHAR;
      i1 ,i2, i3, i4, i5, i6, i7,i8 : ARRAY OF BYTE) : FmtExitCode;

END Printf. 
