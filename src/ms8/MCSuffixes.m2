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
   $Id: MCSuffixes.m2,v 0.1 1997/02/21 18:40:37 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCSuffixes.m2,v $
   Revision 0.1  1997/02/21  18:40:37  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCSuffixes;		(* Martin Hasch, Jan 1989 *)

   FROM Strings IMPORT StrLen;
   FROM MCP4Global IMPORT Assert;

(***** EXPORTED:
   CONST
      ...SX = ...;
*****)

   PROCEDURE TestSuffix(filename,suffix: ARRAY OF CHAR): BOOLEAN;(* EXPORTED *)
      VAR
	 sxindex: CARDINAL;
	 fnlen,
	 sxlen: CARDINAL;
   BEGIN
      fnlen := StrLen(filename);
      sxlen := StrLen(suffix);
      IF sxlen > fnlen THEN
	 RETURN FALSE
      END;
      FOR sxindex := 0 TO sxlen-1 DO
	 IF filename[fnlen-sxlen+sxindex] # suffix[sxindex] THEN
	    RETURN FALSE
	 END;
      END;
      RETURN TRUE
   END TestSuffix;

   PROCEDURE MakeName(  VAR filename: ARRAY OF CHAR;
	       (*read*) VAR sourcename: ARRAY OF CHAR;
			oldsuffix, newsuffix: ARRAY OF CHAR);	(* EXPORTED *)
      VAR
	 index: CARDINAL;
	 fnlen,
	 baselen: CARDINAL;
   BEGIN
      baselen := StrLen(sourcename) - StrLen(oldsuffix);
      fnlen := baselen + StrLen(newsuffix);
      Assert( fnlen-1 <= HIGH(filename) );
      FOR index := 0 TO baselen-1 DO
	 filename[index] := sourcename[index];
      END;
      FOR index := baselen TO fnlen-1 DO
	 filename[index] := newsuffix[index-baselen];
      END;
      IF fnlen <= HIGH(filename) THEN
	 filename[fnlen] := 0C;
      END;
   END MakeName;

END MCSuffixes.
