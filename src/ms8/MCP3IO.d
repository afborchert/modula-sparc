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
   $Id: MCP3IO.d,v 0.1 1997/02/21 18:40:09 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP3IO.d,v $
   Revision 0.1  1997/02/21  18:40:09  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP3IO;               (* LG *)

   FROM SYSTEM IMPORT WORD;
   FROM MCBase IMPORT Idptr, Symbol, Spellix, Stset;

   TYPE 
      Savepos;

   VAR 
      sy     : Symbol;
      val    : CARDINAL;                (* value *)
      typeset: Stset;
      length : CARDINAL;                (* string length *)
      spix   : Spellix;                 (* spelling index of identifier *)
      nptr   : Idptr;                   (* pointer to referenced name *)

   PROCEDURE PutSy(s: Symbol);

   (* put symbol s and pos on output-stream *)

   PROCEDURE PutWord(w: WORD);

   (* put word w on output-stream *)

   PROCEDURE InitSave(VAR s: Savepos);

   PROCEDURE ResetSave(s: Savepos);

   PROCEDURE ReleaseSave(s: Savepos);

   PROCEDURE Error(n: CARDINAL);

   (* put error number n to current symbol *)

   PROCEDURE ErrorLS(n: CARDINAL);

   (* put error number n to last symbol *)

   PROCEDURE GetSy;
   (* get next symbol from input-stream *)

   (* symbol is assigned to sy          *)

   PROCEDURE PutGetSy;

   (* put last symbol sy, get next symbol sy *)

   PROCEDURE StartInOut;

   (* start of input- and output-streams *)

   PROCEDURE TermInOut;

   (* termination of input- and output-streams *)

END MCP3IO. 
