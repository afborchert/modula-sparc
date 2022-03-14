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
   $Id: MCP4Global.m2,v 0.1 1997/02/21 18:40:31 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Global.m2,v $
   Revision 0.1  1997/02/21  18:40:31  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4Global;       (* rev AFB 8/83 *)

   FROM MCP4Public IMPORT ErrorsFound, errName, errFile;
   FROM StdIO IMPORT stderr, FILE, Fopen, write, CloseAll;
   FROM FtdIO IMPORT FwriteString, FwriteCard, FwriteLn;
   FROM MCP4Scanner IMPORT line, position;
   FROM MCFatal IMPORT IOFault;
   FROM MCHalfword IMPORT WriteHalfword;

   VAR
      fp: FILE;

   PROCEDURE Assert(expr: BOOLEAN);
   BEGIN 
      IF NOT expr THEN 
         FwriteString(stderr,  "---- Assertion failed");
         FwriteLn(stderr);
         CompilerError;
      END 
   END Assert;

   PROCEDURE Error(n: CARDINAL);
   BEGIN 
      IF errFile THEN
         WriteHalfword(fp, line);
         WriteHalfword(fp, position);
         WriteHalfword(fp, n);
      ELSE
         FwriteString(stderr, "---- error at line ");
         FwriteCard(stderr, line, 1);
         FwriteString(stderr, ", pos ");
         FwriteCard(stderr, position, 1);
         FwriteString(stderr,  ": ");
         FwriteCard(stderr, n, 1);
         FwriteLn (stderr);
      END;
      ErrorsFound := TRUE
   END Error;

   PROCEDURE CompilerError;
      VAR dummy: INTEGER;
          ign: BOOLEAN;
   BEGIN 
      FwriteString(stderr,  "---- Compiler Error");
      FwriteLn(stderr);
      errFile := FALSE;
      Error(0);
      FwriteString(stderr,  "---- STOP");
      FwriteLn(stderr);
      (* flush i/o buffers *)
      ign := CloseAll();
      (* produce a core dump *)
      dummy := 0;
      dummy := dummy DIV dummy;
   END CompilerError;

   PROCEDURE InitErrorHandling;
   BEGIN
      IF errFile THEN
	 IF NOT Fopen(fp, errName, write, (* buffered = *) FALSE) THEN
	    IOFault(errName);
	 END;
      END;
   END InitErrorHandling;

END MCP4Global. 
