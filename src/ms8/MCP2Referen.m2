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
   $Id: MCP2Referen.m2,v 0.1 1997/02/21 18:40:27 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP2Referen.m2,v $
   Revision 0.1  1997/02/21  18:40:27  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP2Reference;    (* AV / LG *)
                                        (* REV AFB 7/84: parlength *)

   FROM StdIO IMPORT FILE, Fopen, write, Fclose;
   FROM MCHalfword IMPORT WriteHalfword;
   FROM MCFatal IMPORT IOFault;
   FROM MCP2IO IMPORT line, AsciiSetPos, AsciiRead, maxspix;
   FROM MCBase IMPORT Idptr, Stptr, Idclass, Structform, Varkind, Kindvar,
      Spellix, processptr;
   FROM MCP2Public IMPORT refName;

   TYPE 
      RefSymbol = (Modsy, Procsy, Varsy, Int, Card, Char, Bool, Word, Real,
         Arr, ArrDyn, Rec, Point, Setsy, Scal, Procvar, Hide, Process, Abs,
         Ind, Rel, Endsy, Undef, ByteType);
      Byte      = [0..377B];

   VAR 
      ref      : FILE;
      writeref : BOOLEAN;               (* writing on ref file allowed *)
      wordbuff : CARDINAL;
      highbyte : BOOLEAN;

   PROCEDURE WriteRef(b: Byte);
   BEGIN 
      (* format to 0..377B *)
      b := b MOD 400B;
      IF highbyte THEN 
         INC(wordbuff,b*400B);
         WriteHalfword(ref,wordbuff);
      ELSE 
         wordbuff := b;
      END;
      highbyte := NOT highbyte;
   END WriteRef;

   PROCEDURE RefSym(sym: RefSymbol);
      (* write a Refsymbol on the ref file *)
   BEGIN 
      WriteRef(ORD(sym))
   END RefSym;

   PROCEDURE RefIdent(spix: Spellix);
      VAR 
         ch: CHAR;
   BEGIN 
      IF spix > maxspix THEN 
         WriteRef(ORD("*"));
      ELSE 
         AsciiSetPos(spix);
         AsciiRead(ch);
         WHILE ch <> ' ' DO 
            WriteRef(ORD(ch));
            AsciiRead(ch);
         END;
      END;
      WriteRef(0);
   END RefIdent;

   PROCEDURE RefNum(num: CARDINAL);
      (* write a number on the ref file *)
   BEGIN 
      num := num MOD 10000H; (* bug fix: be sure to be not out of range *)
      WriteRef(num DIV 400B);
      WriteRef(num MOD 400B);
   END RefNum;

   PROCEDURE Reference(ip: Idptr);
      (* write a reference to the identifier *)
      VAR 
         tp  : Stptr;
         sym : RefSymbol;
   BEGIN 
      IF writeref AND (ip <> NIL) THEN 
         WITH ip^ DO 
            CASE klass OF 
            | mods : 
                  RefSym (Modsy);
                  RefNum (line);
                  RefIdent (name);
                  RefNum (procnum);
            | pures,funcs : 
                  RefSym (Procsy);
                  RefNum (line);
                  RefIdent (name);
                  RefNum (procnum);
                  (* syntax of reference file changed: *)
                  IF idtyp <> NIL THEN
                     RefNum(idtyp^.parlength);
                  ELSE
                     RefNum(0);
                  END;
            | vars : 
                  RefSym (Varsy);
                  RefNum (line);
                  RefIdent (name);
                  tp := idtyp;
                  IF (tp <> NIL) AND (tp^.form = subranges) THEN 
                     tp := tp^.scalp;
                  END;
                  IF tp<>NIL THEN 
                     WITH tp^ DO 
                        CASE form OF 
                        | ints : 
                              sym := Int;
                        | cards : 
                              sym := Card;
                        | bools : 
                              sym := Bool;
                        | chars : 
                              sym := Char;
			| bytes :
			      sym := ByteType;
                        | words : 
                              sym := Word;
                        | reals : 
                              sym := Real;
                        | arrays : 
                              IF dyn THEN 
                                 sym := ArrDyn;
                              ELSE 
                                 sym := Arr;
                              END;
                        | records : 
                              sym := Rec;
                        | pointers : 
                              IF tp = processptr THEN
                                 sym := Process;
                              ELSE
                                 sym := Point;
                              END;
                        | sets : 
                              sym := Setsy;
                        | enums : 
                              sym := Scal;
                        | proctypes: 
                              sym := Procvar;
                        | hides : 
                              sym := Hide;
                        ELSE (* setoftypes not possible *)
                           sym := Undef 
                        END (* case *) ;
                     END (*with*) ;
                  ELSE 
                     sym := Undef;
                  END;
                  RefSym(sym);
                  IF state = absolute THEN 
                     RefSym(Abs)
                  ELSE 
                     IF indaccess THEN 
                        RefSym(Ind)
                     ELSE 
                        RefSym(Rel)
                     END 
                  END;
                  RefNum(vaddr);
                  IF tp <> NIL THEN 
                     RefNum(tp^.size);
                  ELSE 
                     RefNum(0);
                  END;
            ELSE 
            END                         (*case*)
         END                            (*with*)
      END;
   END Reference;

   PROCEDURE EndReference(ip: Idptr);
   BEGIN 
      IF writeref AND (ip<>NIL) THEN 
         RefSym(Endsy)
      END;
   END EndReference;

   PROCEDURE InitRef;
      (* initialisation of ref file *)
   BEGIN                                (*InitRef*)
      writeref := TRUE;
      IF NOT Fopen(ref, refName, write, (* buffered = *) TRUE) THEN 
         IOFault(refName);
      END;
      highbyte := FALSE;
   END InitRef;

   PROCEDURE TermRef;
   BEGIN 
      IF highbyte THEN 
         WriteRef(0)
      END;
      IF NOT Fclose(ref) THEN 
         IOFault(refName);
      END;
      writeref := FALSE;
   END TermRef;

BEGIN                                   (* MCP2Reference *)
   writeref := FALSE;
END MCP2Reference. 
