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
   $Id: MCP2IO.m2,v 0.2 1997/02/27 17:56:04 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP2IO.m2,v $
   Revision 0.2  1997/02/27  17:56:04  borchert
   - GetModuleKey removed (now in MCKey)
   - DefModStatus removed (was empty)

   Revision 0.1  1997/02/21  18:40:25  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP2IO; (* LG / AFB *)

   (* $T- *)

   IMPORT SYSTEM, StdIO, FtdIO, MCHalfword, MCBase, MCP2Public,
      MCFatal;
   FROM MCBase IMPORT Keyarr;

   VAR 
      lpos, lline: CARDINAL;

   MODULE OutputSystem;

      FROM SYSTEM IMPORT WORD;
      FROM StdIO IMPORT FILE, write, Fopen, Fclose;
      FROM FtdIO IMPORT FwriteWord;
      FROM MCHalfword IMPORT WriteHalfword;
      FROM MCFatal IMPORT IOFault;
      FROM MCBase IMPORT Symbol;
      FROM MCP2Public IMPORT il2Name;
      IMPORT pos;

      EXPORT il2, PutSy, PutWord, StopOutput, RestartOutput, TermOutput,
	 StartOutput;

      VAR 
         il2    : FILE;
         output : BOOLEAN;              (* output on il2 allowed *)

      PROCEDURE PutSy(s : Symbol);
	 (* put Symbol and pos on il2-file *)
	 (* pack symbol with pos into two bytes *)
      BEGIN 
         IF output THEN 
            WriteHalfword(il2, ORD(s) * 400B + pos)
         END 
      END PutSy;

      PROCEDURE PutWord(w : WORD);
	 (* put word on il2-file *)
      BEGIN 
         IF output THEN 
            FwriteWord(il2, w)
         END 
      END PutWord;

      PROCEDURE StopOutput;
      BEGIN 
         output := FALSE 
      END StopOutput;

      PROCEDURE RestartOutput;
      BEGIN 
         output := TRUE 
      END RestartOutput;

      PROCEDURE StartOutput;
      BEGIN
	 IF NOT Fopen(il2, il2Name, write, (* buffered = *) TRUE) THEN 
	    IOFault(il2Name);
	 END;
	 output := TRUE;
      END StartOutput;

      PROCEDURE TermOutput;
      BEGIN 
         PutSy(eop);
         IF NOT Fclose(il2) THEN 
            IOFault(il2Name);
         END;
      END TermOutput;

   END OutputSystem;

   MODULE ErrorSystem;

      FROM MCP2Public IMPORT ErrorsFound;
      FROM MCHalfword IMPORT WriteHalfword;
      FROM FtdIO IMPORT FwriteWord;
      FROM MCBase IMPORT Symbol;
      IMPORT line, pos, lline, lpos, OutputSystem;

      EXPORT Error, ErrorLS;

      CONST 
         errmax = 300;

      VAR 
         errsymval, eolsymval : CARDINAL;
         errcount             : CARDINAL;

      PROCEDURE Error(n: CARDINAL);
      (* no suppression of writing on il2 file *)
      BEGIN 
         INC(errcount);
         ErrorsFound := TRUE;
         IF errcount < errmax THEN 
            WriteHalfword(il2,errsymval + pos); (* pack symbols and pos into two bytes *)
            FwriteWord(il2,n);
         ELSIF errcount = errmax THEN 
            WriteHalfword(il2,errsymval);
            FwriteWord(il2,5);       (* too many errors *)
         END;
      END Error;

      PROCEDURE ErrorLS(n: CARDINAL);
         VAR 
            hpos : CARDINAL;
      BEGIN 
         hpos := pos;
         pos := lpos;
         IF lline <> line THEN 
            WriteHalfword(il2,eolsymval);
            FwriteWord(il2,lline);
            Error(n);
            WriteHalfword(il2,eolsymval);
            FwriteWord(il2,line);
         ELSE 
            Error(n);
         END;
         pos := hpos;
      END ErrorLS;

   BEGIN 
      errcount := 0;
      ErrorsFound := FALSE;
      errsymval := ORD(errorsy) * 400B; (* value for errorsy *)
      eolsymval := ORD(eol) * 400B;     (* value for eol *)
   END ErrorSystem;

   MODULE Scanner;

      FROM SYSTEM IMPORT WORD;
      FROM StdIO IMPORT FILE, read, Fopen, Fclose;
      FROM MCHalfword IMPORT ReadHalfword, WriteHalfword;
      FROM FtdIO IMPORT FwriteWord, FreadWord, Done;
      FROM MCFatal IMPORT IOFault;
      FROM MCBase IMPORT Symbol, Spellix;
      FROM MCP2Public IMPORT il1Name;
      IMPORT ErrorSystem, OutputSystem, sy, spix, maxspix, val, length, pos,
         line, lpos, lline, typeset;

      EXPORT GetSy, PutGetSy, TermInput, StartInput;

      VAR 
         card : CARDINAL;
         il1  : FILE;
         issy : BOOLEAN;

      PROCEDURE ReadWord(VAR w: WORD);
      BEGIN
         FreadWord(il1, w);
         IF NOT Done THEN
            IOFault(il1Name);
         END;
      END ReadWord;

      PROCEDURE GetSy;
      BEGIN                             (* get next Symbol *)
         lpos := pos;
         lline := line;
         REPEAT 
            issy := TRUE;
            ReadHalfword(il1, card);
            pos := card MOD 400B;
            sy := VAL(Symbol,card DIV 400B);
            CASE sy OF 
            | ident: 
                  ReadWord(spix);
            | intcon,cardcon,charcon,realcon,bigsetcon: 
                  ReadWord(val);
	    | intcarcon: (* revision in interpass file *)
		  ReadWord(typeset);
		  ReadWord(val);
            | stringcon: 
                  ReadWord(val);
                  ReadWord(length);
            | option: 
                  ReadWord(val);
                  ReadHalfword(il1, card);
                  PutSy(option);
                  PutWord(val);
                  PutSy(Symbol(card DIV 100H));
                  issy := FALSE;
            | errorsy,eol: 
                  ReadWord(val);
                  IF sy = eol THEN 
                     line := val 
                  END;
                  WriteHalfword(il2,card); (* errorsy or eol *)
                  FwriteWord(il2,val);
		  (* no suppression *)
                  issy := FALSE;
            ELSE                        (* no activity *)
            END;                        (* case *)
         UNTIL issy;
      END GetSy;

      PROCEDURE PutGetSy;
      BEGIN                             (* put last Symbol, get next Symbol *)
         PutSy(sy);
         IF sy = ident THEN 
            PutWord(spix)
         ELSIF (sy >= intcon) AND (sy <= stringcon) THEN 
            IF (sy = intcarcon) THEN
               PutWord(typeset);
            END;
            PutWord(val);
            IF sy = stringcon THEN 
               PutWord(length)
            END 
         END;
         GetSy;
      END PutGetSy;

      PROCEDURE TermInput;
      BEGIN 
         IF NOT Fclose(il1) THEN 
            IOFault(il1Name);
         END;
      END TermInput;

      PROCEDURE StartInput;
      BEGIN
	 IF NOT Fopen(il1, il1Name, read, (* buffered = *) TRUE) THEN 
	    IOFault(il1Name);
	 END;
	 line := 1;
	 pos := 1;
      END StartInput;

   END Scanner;

   MODULE AsciiHandling;                (* $T- *)
      (* handling with the identifier-file ASCII *)

      FROM StdIO IMPORT FILE, Fopen, read, Fclose, Fseek, Fgetc;
      FROM MCFatal IMPORT IOFault;
      FROM MCBase IMPORT Spellix;
      FROM MCP2Public IMPORT ascName;
      IMPORT maxspix;

      EXPORT AsciiSetPos, AsciiRead, TermAscii, StartAscii;

      VAR 
         asc : FILE;
	 returnDummy: BOOLEAN;

      PROCEDURE AsciiSetPos(spix: Spellix);
      (* set position on ASCII file *)
      BEGIN 
	 IF spix >= maxspix THEN
	    returnDummy := TRUE;
	 ELSE
	    returnDummy := FALSE;
	    IF NOT Fseek(asc, INTEGER(spix), 0) THEN 
	       IOFault(ascName);
	    END;
	 END;
      END AsciiSetPos;

      PROCEDURE AsciiRead(VAR ch: CHAR);
      (* read character from ASCII file *)
      BEGIN 
	 IF returnDummy THEN
	    ch := ' '; (* end of identifier in ascii-file *)
	    RETURN
	 END;
         IF NOT Fgetc(ch, asc) THEN 
            IOFault(ascName);
         END;
      END AsciiRead;

      PROCEDURE TermAscii;
      BEGIN 
         IF NOT Fclose(asc) THEN 
            IOFault(ascName);
         END;
      END TermAscii;

      PROCEDURE StartAscii;
      BEGIN
	 IF NOT Fopen(asc, ascName, read, (* buffered = *) TRUE) THEN 
	    IOFault(ascName);
	 END;
      END StartAscii;

   END AsciiHandling;                   (* $T= *)

   MODULE SkipInSymbolModule;

      FROM MCBase IMPORT Symbol;
      FROM Scanner IMPORT GetSy;
      IMPORT sy;

      EXPORT SkipConstant, SkipType;

      PROCEDURE Skip(s: Symbol);
      BEGIN 
         WHILE sy <> s DO 
            GetSy 
         END;
         GetSy;
      END Skip;

      PROCEDURE SkipQualIdent;
      BEGIN 
         IF sy = ident THEN 
            GetSy;
            WHILE sy = period DO 
               GetSy;
               GetSy 
            END;
         END;
      END SkipQualIdent;

      PROCEDURE SkipConstant;
	 (* skip constant in a symbol module *)
      BEGIN 
         IF sy = cardcon THEN 
            GetSy;
            SkipQualIdent;
         ELSIF (sy = stringcon) OR (sy = realcon) OR (sy = intcarcon) THEN 
            GetSy;
         END;
      END SkipConstant;

      PROCEDURE SkipType;
	 (* skip type structures in a symbol module *)

         PROCEDURE SkipVariants;
	    (* skip variant structures *)
         BEGIN 
            IF sy = casesy THEN 
               Skip(colon);
               SkipQualIdent;
               WHILE sy = ofsy DO 
                  Skip(colon);
                  SkipVariants;
                  GetSy;                (* size *)
               END;
               IF sy = elsesy THEN 
                  GetSy;
                  SkipVariants;
                  GetSy;                (* size *)
               END;
               GetSy;                   (* endsy *)
            END;
         END SkipVariants;

      BEGIN                             (* SkipType *)
         CASE sy OF 
         | arraysy: 
               Skip(ofsy);
               SkipType;
         | recordsy: 
               GetSy;
               WHILE sy = ident DO 
                  Skip(colon);
                  SkipType 
               END;
               SkipVariants;
               GetSy;                   (* endsy *)
               GetSy;                   (* size *)
         | setsy,pointersy: 
               GetSy;
               SkipType;
         | proceduresy: 
               Skip(rparent);
               IF sy = colon THEN 
                  GetSy;
                  SkipType 
               END;
         | hidden: 
               GetSy;
         | lparent: 
               Skip(rparent);
         | ident: 
               SkipQualIdent;
               (* revision in interpass file *)
               IF sy = lbrack THEN Skip(rbrack) END;
         | lbrack: 
               Skip(rbrack);
         END;
      END SkipType;

   END SkipInSymbolModule;

   PROCEDURE StartInOut;
   BEGIN
      StartInput;
      StartOutput;
      StartAscii;
   END StartInOut;

   PROCEDURE TermInOut;
   BEGIN 
      TermInput;
      TermOutput;
      TermAscii;
   END TermInOut;

END MCP2IO. 
