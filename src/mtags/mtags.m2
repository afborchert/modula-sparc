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
   $Id: mtags.m2,v 0.1 1997/02/24 08:56:41 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: mtags.m2,v $
   Revision 0.1  1997/02/24  08:56:41  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

MODULE mtags; (* AFB 6/86 *)

   FROM SymTab IMPORT SymRef, EnterProc, EndProc, EnterFile, EndFile,
      FirstProc, NextProc, FileName, Identifier, Line;
   FROM Scan IMPORT Symbol, OpenScan, GetSy;
   FROM Arguments IMPORT GetFlag, Usage, InitArgs, GetArg;
   FROM StdIO IMPORT FILE, write;
   FROM FtdIO IMPORT FwriteString, FwriteLn, FwriteChar;
   FROM ASCII IMPORT tab;
   FROM PipeIO IMPORT Popen, Pclose;
   FROM SysPerror IMPORT Perror;
   FROM SysExit IMPORT Exit;

   CONST
      Sort = "sort > tags"; (* output filter *)
   VAR
      file: FileName;
      sy: Symbol;
      id: Identifier;
      line: Line;
      ch: CHAR;
      out: FILE;

   PROCEDURE Print;
      VAR sym: SymRef; i: CARDINAL;

      PROCEDURE PrintSequence(sr: SymRef);
      BEGIN
	 WITH sr^ DO
	    IF father <> NIL THEN
	       PrintSequence(father);
	       FwriteString(out, ".");
	    END;
	    FwriteString(out, name);
	 END;
      END PrintSequence;

   BEGIN
      FirstProc(sym);
      WHILE sym <> NIL DO
	 WITH sym^ DO
	    IF multdecl THEN
	       PrintSequence(sym);
	    ELSE
	       FwriteString(out, name);
	    END;
	    FwriteChar(out, tab);
	    FwriteString(out, file); FwriteChar(out, tab);
	    FwriteString(out, "?^");
	    i := 0;
	    WHILE (i <= HIGH(line)) AND (line[i] <> 0C) DO
	       CASE line[i] OF (* character with special meaning in regexp ? *)
	       | '?', '\':
		  FwriteChar(out, "\");
	       ELSE
	       END;
	       FwriteChar(out, line[i]);
	       INC(i);
	    END;
	    FwriteString(out, "$?");
	    FwriteLn(out);
	 END;
	 NextProc(sym);
      END;
   END Print;

BEGIN
   InitArgs("file.m2 ...");
   WHILE GetFlag(ch) DO
      CASE ch OF
      ELSE
	 Usage;
      END;
   END;
   IF NOT Popen(out, Sort, write, (* buffered = *) TRUE) THEN
      Perror(Sort);
      Exit(1);
   END;
   WHILE GetArg(file) DO
      OpenScan(file);
      EnterFile(file);
      WHILE GetSy(sy, id, line) DO
	 CASE sy OF
	 | modulesy, procsy: EnterProc(id, line);
	 | endsy: EndProc;
	 END;
      END;
      EndFile;
   END;
   Print;
   IF NOT Pclose(out) THEN
      Perror(Sort);
      Exit(1);
   END;
END mtags.
