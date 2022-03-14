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
   $Id: MCP2IO.d,v 0.2 1997/02/27 17:55:24 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP2IO.d,v $
   Revision 0.2  1997/02/27  17:55:24  borchert
   - GetModuleKey removed (now in MCKey)
   - DefModStatus removed (was empty)

   Revision 0.1  1997/02/21  18:40:07  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP2IO;   (* LG *)

  FROM SYSTEM IMPORT WORD;
  FROM MCBase IMPORT Symbol, Spellix, Keyarr, Stset;

  CONST maxspix = ORD(MAX(INTEGER)); (* maximal value of legal spix *)

  VAR sy : Symbol;
      line : CARDINAL;
      pos : CARDINAL;
      spix : Spellix;
      val : CARDINAL;
      typeset : Stset;
      length : CARDINAL; (* string length *)

  PROCEDURE PutSy(s : Symbol);
    (* put Symbol ans pos on il2-file *)

  PROCEDURE PutWord(w : WORD);
    (* put word on il2-file *)

  PROCEDURE StopOutput;
  
  PROCEDURE RestartOutput;

  PROCEDURE Error(n: CARDINAL);
    (* no suppression of writing on il2 file *)

  PROCEDURE ErrorLS(n: CARDINAL);

  PROCEDURE GetSy;

  PROCEDURE PutGetSy; 

  PROCEDURE StartInOut;
         
  PROCEDURE TermInOut;

  PROCEDURE AsciiSetPos(spix: Spellix); 
    (* set position on ASCII file *) 

  PROCEDURE AsciiRead(VAR ch: CHAR);   
    (* read character from ASCII file *) 

  PROCEDURE SkipConstant;
    (* skip a constant in a symbol module *)

  PROCEDURE SkipType;
    (* skip type structures in a symbol module *)

END MCP2IO.
