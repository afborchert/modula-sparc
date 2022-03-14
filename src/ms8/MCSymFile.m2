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
   $Id: MCSymFile.m2,v 0.1 1997/02/21 18:40:37 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCSymFile.m2,v $
   Revision 0.1  1997/02/21  18:40:37  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCSymFile; (* LG / AFB *)

   (* $T- *)

   IMPORT StdIO, Memory, Conversions, MCBase, MCFatal, MCSymFileDefs,
      FtdIO, MCHalfword;

   MODULE AsciiHandling;                (* $T- *)
      (* handling with the identifier-file ASCII *)

      FROM StdIO IMPORT Fopen, Fclose, Fgetc, Fseek, FILE, read;
      FROM MCBase IMPORT Spellix;
      FROM MCFatal IMPORT IOFault;
      IMPORT ascName;

      EXPORT StartAscii, AsciiSetPos, AsciiRead, TermAscii, maxspix;

      CONST 
         maxspix = 17777B;              (* maximal value of legal spix *)

      VAR 
         asc : FILE;

      PROCEDURE AsciiSetPos(spix: Spellix);
      (* set position on ASCII file *)
      BEGIN 
         IF NOT Fseek(asc, INTEGER(spix), 0) THEN 
            IOFault(ascName);
         END;
      END AsciiSetPos;

      PROCEDURE AsciiRead(VAR ch: CHAR);
      (* read character from ASCII file *)
      BEGIN 
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

   MODULE SymbolFileHandling;

      FROM StdIO IMPORT FILE, Fopen, Fclose, write;
      FROM MCHalfword IMPORT WriteHalfword;
      FROM MCFatal IMPORT IOFault;
      FROM Conversions IMPORT ConvertOctal;
      FROM MCBase IMPORT Spellix;
      FROM MCSymFileDefs IMPORT SymFileSymbols;
      FROM AsciiHandling IMPORT maxspix, AsciiSetPos, AsciiRead;
      IMPORT symName;

      EXPORT SymPutS, SymPutNumber, SymPutCard, SymPutStr, SymPutIdent,
         InitSym, TermSym;

      TYPE 
         Byte = [0..377B];

      VAR 
         symf     : FILE;
         wordbuff : CARDINAL;
         highbyte : BOOLEAN;

      PROCEDURE WriteSym(b: Byte);
      BEGIN 
         (* format to 0..377B *)
         b := b MOD 400B;
         IF highbyte THEN 
            INC(wordbuff,b*400B);
            WriteHalfword(symf,wordbuff);
         ELSE 
            wordbuff := b;
         END;
         highbyte := NOT highbyte;
      END WriteSym;

      PROCEDURE SymPutS(s: SymFileSymbols);
      BEGIN 
         WriteSym(ORD(s));
      END SymPutS;

      PROCEDURE SymPutNumber(c: CARDINAL);
	 TYPE buf4 = ARRAY[0..3] OF CHAR;
	 VAR buf: buf4;
	     index: [0..3];
      BEGIN 
	 buf := buf4(c);
	 FOR index := 0 TO 3 DO
	    WriteSym(ORD(buf[index]));
	 END;
      END SymPutNumber;

      PROCEDURE SymPutCard(c: CARDINAL);
      BEGIN 
         SymPutS(normalconstSS);
         SymPutNumber(c);
      END SymPutCard;

      PROCEDURE SymPutStr(addr,length: CARDINAL);
         TYPE 
            Bufptr = POINTER TO ARRAY [0 .. 100] OF CHAR;
         VAR 
            string : Bufptr;
            ix     : CARDINAL;
      BEGIN 
	 (* $T- *)
         SymPutS(stringconstSS);
         string := Bufptr(addr);
	 ix := 0;
	 WHILE string^[ix] <> 0C DO
            WriteSym(ORD(string^[ix]));
	    INC(ix);
         END;
	 WriteSym(0);
	 (* $T= *)
      END SymPutStr;

      PROCEDURE SymPutIdent(sx: Spellix);
         VAR 
            ch  : CHAR;
            str : ARRAY [0 .. 5] OF CHAR;
            ix  : CARDINAL;

      BEGIN 
         SymPutS(identSS);
         IF INTEGER(sx) < 0 THEN        (* dummy type name *)
            ConvertOctal(sx,6,str);
            FOR ix := 0 TO 5 DO 
               WriteSym(ORD(str[ix]))
            END;
         ELSIF sx <= maxspix THEN       (* identifier *)
            AsciiSetPos(sx);
            AsciiRead(ch);
            WHILE ch <> ' ' DO 
               WriteSym(ORD(ch));
               AsciiRead(ch)
            END;
         END;
         WriteSym(0);
      END SymPutIdent;

      PROCEDURE InitSym;
      BEGIN 
         IF NOT Fopen(symf, symName, write, (* buffered = *) TRUE) THEN 
            IOFault(symName);
         END;
         highbyte := FALSE;
      END InitSym;

      PROCEDURE TermSym;
      BEGIN 
         SymPutS(endfileSS);
         IF highbyte THEN 
            WriteSym(0)
         END;
         IF NOT Fclose(symf) THEN 
            IOFault(symName);
         END;
      END TermSym;

   END SymbolFileHandling;

   MODULE SymbolDump;

      FROM Memory IMPORT ALLOCATE, DEALLOCATE;
      FROM MCBase IMPORT Idptr, Stptr, Idclass, Structform, Spellix, Constval,
         mainmodp, root, sysmodp, Varkind, Kindvar, Recpart, Stset, SetValuePtr,
         maxcard, oneword;
      FROM MCSymFileDefs IMPORT symFileKey, SymFileSymbols;
      FROM SymbolFileHandling IMPORT SymPutS, SymPutNumber, SymPutCard,
         SymPutStr, SymPutIdent, InitSym, TermSym;

      EXPORT StartDump;

      TYPE 
         Nlptr    = POINTER TO Namelist;
         Namelist = RECORD 
            namep : Idptr;
            nxtnp : Nlptr;
         END;
         Mlptr    = POINTER TO Modlist;
         Modlist  = RECORD 
            mnamp : Idptr;
            nxtmp : Mlptr;
            fstnp : Nlptr;
            lstnp : Nlptr;
         END;

      VAR 
         mlroot : Mlptr;                (* root of module list *)
         dumix : INTEGER;(* index for dummy identifier i.e. negative numbers *)

      PROCEDURE EnterDumpList(namp: Idptr);

         PROCEDURE GenDummyType(sp: Stptr);
            VAR 
               ip : Idptr;

         BEGIN  (* generate dummy type name for a not explicit declared type *)
            DEC(dumix);                 (* new dummy identifier *)
            NEW(ip,types);
            WITH ip^ DO 
               name := CARDINAL(dumix);
               link := NIL;
               globmodp := mainmodp;
               idtyp := sp;
               klass := types;
            END;                        (* WITH *)
            sp^.stidp := ip;
         END GenDummyType;

         PROCEDURE EnterName(namp: Idptr);
            VAR 
               mp : Mlptr;
               np : Nlptr;

         BEGIN 
            IF namp <> NIL THEN     (* enter referrenced name in module list *)
               NEW(np);
               np^.namep := namp;
               np^.nxtnp := NIL;
               mp := mlroot;
               WHILE mp^.mnamp <> namp^.globmodp DO 
                  mp := mp^.nxtmp 
               END;
               WITH mp^ DO 
                  IF fstnp = NIL THEN 
                     fstnp := np;
                  ELSE 
                     lstnp^.nxtnp := np;
                  END;
                  lstnp := np 
               END;                     (* WITH *)
            END;
         END EnterName;

         PROCEDURE StructCheck(strp: Stptr);
         (* check if the names belonging to the structure are already *)
         (* entered into the separate compilation dump list *)
            VAR 
               ip : Idptr;
               sp : Stptr;

         BEGIN 
            IF NOT strp^.inlist THEN 
               WITH strp^ DO 
                  inlist := TRUE;
                  CASE form OF 
                    enums,hides: 
                        EnterName(stidp);
                  |subranges: 
                        StructCheck(scalp);
                        EnterName(stidp);
                  |pointers: 
                        EnterName(stidp);
                        IF elemp^.stidp = NIL THEN 
                           GenDummyType(elemp)
                        END;
                        StructCheck(elemp);
                  |sets, bigsets: 
                        StructCheck(basep);
                        EnterName(stidp);
                  |arrays: 
                        StructCheck(elp);
                        IF NOT dyn THEN 
                           StructCheck(ixp);
                           EnterName(stidp);
                        END;
                  |records: 
                        CASE rpart OF 
                          fixedpart: 
                              ip := fieldp;
                              WHILE ip <> NIL DO 
                                 WITH ip^ DO 
                                    IF idtyp^.stidp = NIL THEN 
                                       GenDummyType(idtyp)
                                    END;
                                    StructCheck(idtyp);
                                    ip := link;
                                 END;   (* WITH *)
                              END;
                              IF tagp <> NIL THEN 
                                 StructCheck(tagp)
                              END;
                              EnterName(stidp);
                        |tagfield: 
                              StructCheck(tagtyp);
                              sp := fstvarp;
                              WHILE sp <> NIL DO 
                                 IF sp^.subtagp <> NIL THEN 
                                    StructCheck(sp^.subtagp);
                                 END;
                                 sp := sp^.nxtvarp;
                              END;
                        END;            (* CASE *)
                  |proctypes: 
                        ip := fstparam;
                        WHILE ip <> NIL DO 
                           StructCheck(ip^.idtyp);
                           ip := ip^.vlink;
                        END;
                        IF rkind = funcs THEN 
                           StructCheck(funcp)
                        END;
                        EnterName(stidp);
                  END;                  (* CASE *)
               END;                     (* WITH *)
            END;
         END StructCheck;

         PROCEDURE IdentCheck(namp: Idptr);
         BEGIN 
            WITH namp^ DO 
               CASE klass OF 
                 types: 
                     StructCheck(idtyp);
                     (* check IF a second name of this type exists *)
                     IF namp <> idtyp^.stidp THEN 
                        EnterName(namp)
                     END;
               |consts: 
                     StructCheck(idtyp);
                     EnterName(namp);
               |vars: 
                     IF idtyp^.stidp = NIL THEN 
                        GenDummyType(idtyp)
                     END;
                     StructCheck(idtyp);
                     EnterName(namp);
               |pures,funcs: 
                     StructCheck(idtyp)
               |indrct: 
                     IdentCheck(nxtidp);
               ELSE                     (* unknown,mods,fields *)
               END;                     (* CASE *)
            END;                        (* WITH *)
         END IdentCheck;

      BEGIN                             (* EnterDumpList *)
         WHILE namp <> NIL DO 
            IdentCheck(namp);
            namp := namp^.link;
         END;                           (* WHILE *)
      END EnterDumpList;

      PROCEDURE DumpModule(mp: Mlptr);
      (* dump symbolic module on symbol file *)
         VAR 
            hmp     : Mlptr;
            np1,np2 : Nlptr;
            explp   : Idptr;
            expname : Spellix;
            curmod  : Idptr;

         PROCEDURE DumpDeclaration(ip: Idptr);
         (* dump one declaration on symbol file *)

            PROCEDURE DumpQualIdent(ip: Idptr);
            BEGIN 
               WITH ip^ DO 
                  IF (globmodp <> curmod) AND (globmodp <> root) THEN 
                     SymPutIdent(globmodp^.name);
                     SymPutS(periodSS);
                  END;
                  SymPutIdent(name);
               END;
            END DumpQualIdent;

            PROCEDURE DumpConst(sp: Stptr; val: Constval);
               CONST 
                  rwordnum = 2;         (* number of words for a real number *)

               VAR 
                  ix    : CARDINAL;
                  rconv : RECORD 
                     CASE : BOOLEAN OF 
                       FALSE : 
                           ra: ARRAY [1..rwordnum] OF CARDINAL;
                     |TRUE : 
                           rc: REAL;
                     END;
                  END;

            BEGIN 
               WITH sp^ DO 
                  IF form = arrays THEN (* string constant *)
                     SymPutStr(val.svalue^.valentry,ixp^.max + 1);
                                        (* addr,length *)
                  ELSIF (form = reals) OR (form = longreals) OR
			(form = setoftypes) & (reals IN typeset) THEN 
                     SymPutS(realconstSS);
                     rconv.rc := val.rvalue^.rvalue;
                     FOR ix := 1 TO rwordnum DO 
                        SymPutNumber(rconv.ra[ix]);
                     END;
                  ELSIF form = setoftypes THEN
                     SymPutS(intcarconstSS);
                     SymPutNumber(CARDINAL(typeset));
                     SymPutNumber(val.value);
                  ELSIF form = bigsets THEN
                     DumpQualIdent(stidp);
                     DumpBigSet(val.setvalue);
                  ELSE 
                     SymPutCard(val.value);
                     DumpQualIdent(stidp);
                                        (* type identifier of constant *)
                  END;
               END;
            END DumpConst;

	    PROCEDURE DumpBigSet(sp: SetValuePtr);
	       TYPE BigSetPtr = POINTER TO
				   ARRAY [0..(maxcard-oneword)DIV oneword] OF
				      CARDINAL; (* BITSET *)
	       VAR setptr: BigSetPtr; index: CARDINAL;
	    BEGIN
	       WITH sp^ DO
		  SymPutS(bigsetconstSS);
		  SymPutNumber(size);
		  SymPutNumber(offset);
		  setptr := valentry;
		  FOR index := 0 TO size-1 DO
		     SymPutNumber(setptr^[index]);
		  END;
	       END;
	    END DumpBigSet;

            PROCEDURE DumpType(sp: Stptr; struct: BOOLEAN);
            (* dump type structure or type identifier *)
               VAR 
                  ip   : Idptr;
                  cval : Constval;

               PROCEDURE DumpVariants(sp: Stptr);
               (* dump structure of record variants *)
                  VAR 
                     sp1     : Stptr;
                     csize   : CARDINAL;(* current size of variant *)
                     csubtag : Stptr;   (* current pointer to subvariant *)

               BEGIN 
                  IF sp <> NIL THEN 
                     SymPutS(caseSS);
                     SymPutS(colonSS);
                     WITH sp^ DO 
                        DumpType(tagtyp,FALSE);
                        sp1 := fstvarp;
                        WHILE sp1 <> NIL DO 
                                        (* dump variants *)
                           SymPutS(ofSS);
                           csize := sp1^.size;
                           csubtag := sp1^.subtagp;
                           WHILE (sp1 <> NIL) AND (sp1^.size = csize) AND (sp1
                              ^.subtagp = csubtag) DO 
                              SymPutCard(sp1^.varval);
                              sp1 := sp1^.nxtvarp;
                           END;
                           SymPutS(colonSS);
                           DumpVariants(csubtag);
                           SymPutCard(csize);
                        END;
                        IF elsevarp <> NIL THEN 
                                        (* else variant *)
                           SymPutS(elseSS);
                           DumpVariants(elsevarp^.subtagp);
                           SymPutCard(elsevarp^.size);
                        END;
                     END;
                     SymPutS(endSS);
                  END;
               END DumpVariants;

            BEGIN                       (* DumpType *)
               WITH sp^ DO 
                  IF NOT struct AND (stidp <> NIL) THEN 
                     DumpQualIdent(stidp);
                  ELSE                  (* dump type structure *)
                     CASE form OF 
                       enums: 
                           SymPutS(lparentSS);
                           ip := fcstp;
                           WHILE ip <> NIL DO 
                              SymPutIdent(ip^.name);
                              SymPutCard(ip^.cvalue.value);
                              ip := ip^.link;
                           END;
                           SymPutS(rparentSS);
                     |subranges: 
                           (* revision of symbol file *)
                           DumpType(scalp, FALSE);
                           SymPutS(lbracketSS);
                           cval.value := min;
                           DumpConst(scalp,cval);
                           SymPutS(rangeSS);
                           cval.value := max;
                           DumpConst(scalp,cval);
                           SymPutS(rbracketSS);
                     |pointers: 
                           SymPutS(pointertypSS);
                           DumpType(elemp,FALSE);
                     |hides: 
                           SymPutS(hiddentypSS);
                     |sets, bigsets: 
                           SymPutS(settypSS);
                           DumpType(basep,FALSE);
                     |arrays: 
                           SymPutS(arraytypSS);
                           IF NOT dyn THEN 
                              DumpType(ixp,FALSE);
                              SymPutS(ofSS)
                           END;
                           DumpType(elp,FALSE);
                     |records: 
                           SymPutS(recordtypSS);
                           ip := fieldp;
                           WHILE ip <> NIL DO 
                                        (* dump fields *)
                              SymPutIdent(ip^.name);
                              SymPutCard(ip^.fldaddr);
                                        (* offset in record *)
                              SymPutS(colonSS);
                              DumpType(ip^.idtyp,FALSE);
                              ip := ip^.link;
                           END;
                           DumpVariants(tagp);
                           SymPutS(endSS);
                           SymPutCard(size);
                                        (* record size *)
                     |proctypes: 
                           SymPutS(procSS);
                           SymPutS(lparentSS);
                           ip := fstparam;
                           WHILE ip <> NIL DO 
                              IF ip^.vkind = varparam THEN 
                                 SymPutS(varSS)
                              END;
                              DumpType(ip^.idtyp,FALSE);
                              ip := ip^.vlink;
                           END;
                           SymPutS(rparentSS);
                           IF rkind = funcs THEN 
                              SymPutS(colonSS);
                              DumpType(funcp,FALSE);
                           END;
                     END                (* case *)
                  END;
               END;                     (* WITH *)
            END DumpType;

         BEGIN                          (* DumpDeclaration *)
            WITH ip^ DO 
               CASE klass OF 
                 consts: 
                     SymPutS(constSS);
                     SymPutIdent(name);
                     DumpConst(idtyp,cvalue);
               |types: 
                     SymPutS(typSS);
                     SymPutIdent(name);
                     DumpType(idtyp,idtyp^.stidp=ip);
                                        (*check on equivalent type names*)
               |vars: 
                     SymPutS(varSS);
                     SymPutIdent(name);
                     IF state = absolute THEN 
                        SymPutS(lbracketSS);
                        SymPutCard(vaddr);
                        SymPutS(rbracketSS);
                     ELSE 
                        SymPutCard(vaddr);
                     END;
                     SymPutS(colonSS);
                     DumpType(idtyp,FALSE);
               |pures,funcs: 
                     SymPutS(procSS);
                     SymPutIdent(name);
                     SymPutCard(procnum);
                     DumpType(idtyp,TRUE);
               END;                     (* case *)
            END;                        (* WITH *)
         END DumpDeclaration;

      BEGIN 
         WITH mp^ DO 
            curmod := mnamp;
            SymPutS(unitSS);
            WITH mnamp^ DO 
               SymPutCard(modulekey[0]);
               SymPutCard(modulekey[1]);
               SymPutCard(modulekey[2]);
               SymPutIdent(name);
            END;
            IF fstnp <> NIL THEN 
            (* import in definition module to be developed *)
               SymPutS(importSS);       (* import list *)
               SymPutIdent(sysmodp^.name);
                                        (* module SYSTEM always imported *)
               hmp := mlroot;
               WHILE hmp <> mp DO       (* import all preceding modules *)
                  SymPutIdent(hmp^.mnamp^.name);
                  hmp := hmp^.nxtmp;
               END;
               SymPutS(exportSS);       (* export list *)
               (* dump exported names on the name list *)
               explp := mnamp^.expp;
               WHILE explp <> NIL DO 
                  expname := explp^.name;
                  np1 := fstnp;
                  WHILE np1 <> NIL DO 
                     IF np1^.namep^.name = expname THEN 
                        SymPutIdent(expname);
                        np1 := NIL;
                     ELSE 
                        np1 := np1^.nxtnp 
                     END;
                  END;
                  explp := explp^.link;
               END;
               np1 := fstnp;
               WHILE np1 <> NIL DO      (* dump name list *)
                  np2 := np1;
                  np1 := np2^.nxtnp;
                  DumpDeclaration(np2^.namep);
                  DISPOSE(np2);
               END;
            END;                        (* IF *)
            SymPutS(endunitSS);
         END;                           (* WITH *)
      END DumpModule;

      PROCEDURE StartDump;
      (* dump of the symbol file *)
         VAR 
            mp, hmp, dmp : Mlptr;
            ip           : Idptr;

         PROCEDURE EnterModList(ip: Idptr);
         (* enter module into list in order of module number *)
            VAR 
               mp, hmp, nmp : Mlptr;
               num          : CARDINAL;

         BEGIN 
            num := ip^.modnum;
            NEW(nmp);
            WITH nmp^ DO 
               mnamp := ip;
               nxtmp := NIL;
               fstnp := NIL;
               lstnp := NIL;
            END;
            mp := mlroot;
            WHILE (mp <> NIL) AND (mp^.mnamp^.modnum < num) DO 
               hmp := mp;
               mp := mp^.nxtmp;
            END;
            nmp^.nxtmp := mp;
            IF mp = mlroot THEN 
               mlroot := nmp;
            ELSE 
               hmp^.nxtmp := nmp;
            END;
         END EnterModList;

      BEGIN 
         (* entry of modules into module list *)
         (* assume that main-module has the highest module number *)
         ip := root^.locp;              (* list of separate modules *)
         WHILE ip <> NIL DO 
            IF ip <> sysmodp THEN 
               EnterModList(ip)
            END;
            ip := ip^.link;
         END;
         (* generate lists of names to be dumped *)
         EnterDumpList(mainmodp^.expp);
         EnterDumpList(mainmodp^.locp);
         (* dump on symbol file *)
         InitSym;
         (* dump header *)
         SymPutCard(symFileKey);
         WITH mainmodp^ DO 
            SymPutIdent(name);
         END;
         (* dump modules *)
         mp := mlroot;
         WHILE mp <> NIL DO 
            DumpModule(mp);
            mp := mp^.nxtmp;
         END;
         TermSym;
         (* dispose module list *)
         mp := mlroot;
         WHILE mp <> NIL DO 
            dmp := mp;
            mp := mp^.nxtmp;
            DISPOSE(dmp);
         END;
      END StartDump;

   BEGIN                                (* SymbolDump *)
      dumix := 0;                      (* initialisation of dummy identifier *)
      mlroot := NIL;
   END SymbolDump;

   PROCEDURE GenSymFile;
   BEGIN 
      StartAscii;
      StartDump;
      TermAscii;
   END GenSymFile;

END MCSymFile. 
