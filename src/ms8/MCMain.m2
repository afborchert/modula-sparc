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
   $Id: MCMain.m2,v 0.1 1997/02/21 18:40:22 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCMain.m2,v $
   Revision 0.1  1997/02/21  18:40:22  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

MODULE MCMain; (* AFB 3/96 *)

   FROM FtdIO IMPORT FwriteString, FwriteLn;
   FROM StdIO IMPORT stderr;
   FROM MCArgs IMPORT ScanArgs;
   FROM MCBase IMPORT passes, verbose;
   FROM MCPass1 IMPORT Pass1;
   FROM MCPass2 IMPORT Pass2;
   FROM MCPass3 IMPORT Pass3;
   FROM MCPass4 IMPORT Pass4;
   FROM MCSymFile IMPORT GenSymFile;
   FROM SysExit IMPORT Exit;
   IMPORT MCBase, MCP1Public, MCP2Public, MCP3Public, MCP4Public;

   (* note that the exit code indicates the number of passes run
      in case of non-successful runs; this allows the invocation
      environment to find the final interpass file which contains
      the error messages
   *)

   PROCEDURE Run(proc: PROC; msg: ARRAY OF CHAR);
   BEGIN
      IF verbose THEN
	 FwriteString(stderr, "# ");
	 FwriteString(stderr, msg);
	 FwriteLn(stderr);
      END;
      proc;
      INC(passes);
   END Run;

   PROCEDURE CheckResult(errors: BOOLEAN);
   BEGIN
      IF verbose & errors THEN
	 FwriteString(stderr, "# errors found");
	 FwriteLn(stderr);
      END;
   END CheckResult;

   PROCEDURE Abort;
   BEGIN
      IF verbose THEN
	 FwriteString(stderr, "# aborting compilation");
	 FwriteLn(stderr);
      END;
      Exit(passes);
   END Abort;

BEGIN
   passes := 0;
   ScanArgs;
   Run(Pass1, "pass1: syntactic analysis");
   CheckResult(MCP1Public.SymFilesMissing OR MCP1Public.ErrorsFound);
   IF MCP1Public.SymFilesMissing THEN Exit(1) END;
   Run(Pass2, "pass2: semantic analysis of declarations");
   CheckResult(MCP2Public.ErrorsFound);
   IF MCBase.isdef THEN
      IF MCP1Public.ErrorsFound OR MCP2Public.ErrorsFound THEN
	 Abort;
      END;
      Run(GenSymFile, "pass3: symbol file generation");
   ELSE
      Run(Pass3, "pass3: semantic analysis of statements");
      CheckResult(MCP3Public.ErrorsFound);
      IF MCP1Public.ErrorsFound OR MCP2Public.ErrorsFound OR
	    MCP3Public.ErrorsFound THEN
	 Abort;
      END;
      Run(Pass4, "pass4: code generation");
      CheckResult(MCP4Public.ErrorsFound);
      IF MCP4Public.ErrorsFound THEN
	 Abort;
      END;
   END;
END MCMain.
