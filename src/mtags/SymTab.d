(* Ulm's Modula-2 System: Tags File Generator
   Copyright (C) 1986-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Tags File Generator for Modula-2 is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   Ulm's Tags File Generator for Modula-2 is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY; without even the
   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: SymTab.d,v 0.1 1997/02/24 08:56:39 borchert Exp borchert $
   ----------------------------------------------------------------------------
   $Log: SymTab.d,v $
   Revision 0.1  1997/02/24  08:56:39  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE SymTab;

   CONST
      FileNameLength = 256;
      LineLength = 256;
      IdentLen = 64;

   TYPE
      FileName = ARRAY[0..FileNameLength-1] OF CHAR;
      Line = ARRAY[0..LineLength-1] OF CHAR;
      Identifier = ARRAY[0..IdentLen-1] OF CHAR;
      SymRef = POINTER TO SymEntry;
      SymEntry =
	 RECORD
	    name: Identifier;  (* name of procedure/module *)
	    file: FileName;
	    line: Line;
	    multdecl: BOOLEAN; (* multiple declared ? *)
	    father: SymRef;
	    link: SymRef;
	 END;

   PROCEDURE EnterProc(pname: Identifier; line: Line);

   PROCEDURE EndProc;

   PROCEDURE EnterFile(fname: FileName);

   PROCEDURE EndFile;

   PROCEDURE FirstProc(VAR sym: SymRef);

   PROCEDURE NextProc(VAR sym: SymRef);

END SymTab.
