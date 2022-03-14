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
   $Id: MCP1IO.d,v 0.1 1997/02/21 18:40:06 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP1IO.d,v $
   Revision 0.1  1997/02/21  18:40:06  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP1IO;               (* LG *)

   FROM MCBase IMPORT Symbol, Spellix, Constval, Stset;

   VAR 
      sy    : Symbol;
      val   : Constval;
      typeset: Stset; (* for intcarcon *)
      length: CARDINAL;
      spix  : Spellix;                 (* index generated to each identifier *)

   TYPE 
      String14 = ARRAY [0 .. 13] OF CHAR;

   PROCEDURE PutS;
      (* put current symbol *)

   PROCEDURE PutSy(sy: Symbol);
      (* put symbol *)

   PROCEDURE PutSyVal(sy: Symbol; val: CARDINAL);
      (* put symbol and value *)

   PROCEDURE PutIdent(spix: Spellix);
      (* put symbol ident and spix *)

   PROCEDURE Error(n: CARDINAL);
      (* put error with number n *)

   PROCEDURE InitSave;
      (* output must be saved *)

   PROCEDURE StopSave;
      (* stop saving output *)

   PROCEDURE RestartSave;
      (* restart saving output *)

   PROCEDURE ReleaseSys;
      (* release saved output *)

   PROCEDURE GetSy;
      (* get next symbol *)

   PROCEDURE GetSeparateModule;
      (* get symbol file belonging to module with current spix *)

   PROCEDURE GetDmId;
      (* generate a dummy spix for an inexisting identifier *)

   PROCEDURE HashIdent(VAR str: String14);
      (* enter identifier into identifier table *)
      (* an index  spix  is generated *)

   PROCEDURE EnterResWord(str: String14; sy: Symbol);
      (* enter reserved word with his symbol *)

   PROCEDURE InitInOut;
      (* initialisation of input- and output-streams *)

   PROCEDURE TermInOut;
      (* termination of input- and output-streams *)

END MCP1IO. 
