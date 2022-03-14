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
   $Id: MCP4Scanner.m2,v 0.1 1997/02/21 18:40:32 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Scanner.m2,v $
   Revision 0.1  1997/02/21  18:40:32  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4Scanner;      (* rev AFB 8/83 *)

   FROM MCBase IMPORT Symbol, Idptr, Stptr, Stringptr;
   FROM MCP4In IMPORT ReadInputWord, ReadInputHalfword;
   FROM MCP4CodeSys IMPORT StrEmit;
   FROM MCP4Public IMPORT Rflag, lflag, Sflag;
   FROM Conversions IMPORT ConvertCardinal;

   (* (* from definition module *)
   VAR
      position             : INTEGER;
      line                 : INTEGER;
      sy                   : Symbol;
      val                  : INTEGER;
      nptr                 : Idptr;
      cstPtr               : Stptr;
      cString              : Stringptr;
      controlRangeCheck    : BOOLEAN;
      arithmeticRangeCheck : BOOLEAN;
      pline                : INTEGER;
   *)

   TYPE
      State =
	 RECORD
	    position, line, pline: INTEGER;
	    controlRangeCheck, arithmeticRangeCheck: BOOLEAN;
	 END;
   VAR
      state: State; (* current state *)

   PROCEDURE GetState;
   BEGIN
      IF controlRangeCheck # state.controlRangeCheck THEN
	 IF state.controlRangeCheck THEN
	    StrEmit("%* switching control range checks ON");
	 ELSE
	    StrEmit("%* switching control range checks OFF");
	 END;
      END;
      position := state.position;
      line := state.line;
      pline := state.pline;
      controlRangeCheck := state.controlRangeCheck;
      arithmeticRangeCheck := state.arithmeticRangeCheck;
   END GetState;

   PROCEDURE OptionCode;
      (* Treat a compiler directive *)
      VAR
	 option: CHAR;
	 optval: BOOLEAN;
   BEGIN
      option := CHR(val);
      GetSymbol;
      optval := sy = plus;
      GetSymbol;
      CASE option OF
      | 'T':   state.controlRangeCheck := optval;
      | 'R':   state.arithmeticRangeCheck := optval;
      ELSE
      END;
   END OptionCode;

   PROCEDURE GetSymbol;
    (* Read one Symbol (skipping the eol) and set exported variables *)
      VAR
         i: INTEGER;
   BEGIN
      GetState;
      REPEAT
         ReadInputHalfword(i);
         state.position := i MOD 400B;
         sy := Symbol(i DIV 400B);    (* no sign extension *)
         IF (sy = namesy) OR (sy = modulesy) OR (sy = proceduresy) THEN
            ReadInputWord(nptr);
         ELSIF sy = eol THEN
            ReadInputWord(state.line);
	    IF state.pline <> 0 THEN
	       INC(state.pline);
	    END;
         ELSIF sy = field THEN
            ReadInputWord(val);
         ELSIF sy = option THEN
            ReadInputWord(val);
            OptionCode;
         ELSIF sy = anycon THEN
            ReadInputWord(cstPtr);
            ReadInputWord(cString);
            val := INTEGER(cString);
         END;
      UNTIL sy # eol;
   END GetSymbol;

   PROCEDURE Skip(x1, x2: Symbol);
   BEGIN
      WHILE (sy <> endsy) AND (sy <> x1) AND (sy <> x2) DO
         IF (sy = casesy) OR (sy = ifsy) OR (sy = withsy) OR (sy = loopsy) OR
	    (sy = forsy) OR (sy = whilesy) THEN
            GetSymbol;
            Skip(endsy, endsy);
         END;
         GetSymbol;
      END
   END Skip;

BEGIN
   IF Rflag THEN
      state.controlRangeCheck := FALSE;
      state.arithmeticRangeCheck := FALSE;
   ELSE
      state.controlRangeCheck := TRUE;
      state.arithmeticRangeCheck := TRUE;
   END;
   state.line := 1; state.pline := 0;
   GetState;
END MCP4Scanner.
