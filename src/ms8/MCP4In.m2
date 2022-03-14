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
   $Id: MCP4In.m2,v 0.1 1997/02/21 18:40:31 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4In.m2,v $
   Revision 0.1  1997/02/21  18:40:31  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4In;           (* AFB 8/83 *)

   FROM SYSTEM IMPORT WORD;
   FROM StdIO IMPORT FILE, Fopen, Fclose, read;
   FROM FtdIO IMPORT FreadWord, Done;
   FROM MCHalfword IMPORT ReadHalfword;
   FROM MCFatal IMPORT IOFault;
   FROM MCP4Public IMPORT il1Name;

   VAR
      il1: FILE;

   PROCEDURE ReadInputWord(VAR w: WORD);
   BEGIN 
      FreadWord(il1, w);
      IF NOT Done THEN
         IOFault(il1Name);
      END;
   END ReadInputWord;

   PROCEDURE ReadInputHalfword(VAR w: WORD);
      VAR c: CARDINAL;
   BEGIN
      ReadHalfword(il1, c);
      w := WORD(c);
   END ReadInputHalfword;

   PROCEDURE CloseInput;
   BEGIN 
      IF NOT Fclose(il1) THEN
         IOFault(il1Name);
      END;
   END CloseInput;

   PROCEDURE OpenInput;
   BEGIN
      IF NOT Fopen(il1, il1Name, read, (* buffered = *) TRUE) THEN
	 IOFault(il1Name);
      END;
   END OpenInput;

END MCP4In. 
