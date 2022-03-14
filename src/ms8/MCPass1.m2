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
   $Id: MCPass1.m2,v 0.1 1997/02/21 18:40:34 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCPass1.m2,v $
   Revision 0.1  1997/02/21  18:40:34  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCPass1;                (* SEK / LG / AFB *)

(* $T- *)

   FROM MCBase IMPORT Symbol, Spellix, mainmodp, ismain, modrev, Constval,
      Structform, Stset, bigsetroot, modrev2;
   FROM MCP1IO IMPORT sy, spix, PutS, PutSy, PutSyVal, PutIdent, Error,
      InitSave, StopSave, RestartSave, ReleaseSys, GetSy, GetSeparateModule,
      GetDmId, InitInOut, TermInOut, typeset, val;
   FROM MCP1Ident IMPORT InitIdTables;
   FROM MCP1Public IMPORT SymFilesMissing, ErrorsFound;
   IMPORT MCBase;

   MODULE SymsetHandling;

      FROM MCBase IMPORT Symbol;

      EXPORT Symset, Set1, Set2, Set3, InSet, InclSet, AddSet, SubSet,
	 GenSet1, GenSet2, GenSet3, s0;

      CONST
         setlength = 2;

      TYPE
         Symset = ARRAY [0..setlength] OF BITSET;

      VAR
         i  : CARDINAL;
         s0 : Symset;

      PROCEDURE Set1(VAR s : Symset; sy : Symbol);
      BEGIN
         INCL(s[ORD(sy) DIV 32], ORD(sy) MOD 32);
      END Set1;

      PROCEDURE Set2(VAR s : Symset; sy1,sy2 : Symbol);
      BEGIN
         Set1(s,sy1);
         Set1(s,sy2);
      END Set2;

      PROCEDURE Set3(VAR s : Symset; sy1,sy2,sy3 : Symbol);
      BEGIN
         Set2(s,sy1,sy2);
         Set1(s,sy3);
      END Set3;

      PROCEDURE InSet(VAR s : Symset; sy : Symbol) : BOOLEAN;
      BEGIN
         RETURN ORD(sy) MOD 32 IN s[ORD(sy) DIV 32];
      END InSet;

      PROCEDURE AddSet(VAR s,s1,s2 : Symset);
         VAR
            i : CARDINAL;

      BEGIN
         FOR i := 0 TO setlength DO
            s[i] := s1[i] + s2[i]
         END;
      END AddSet;

      PROCEDURE InclSet(VAR s, s1: Symset);
         VAR
            i: CARDINAL;

      BEGIN
         FOR i := 0 TO setlength DO
            s[i] := s[i] + s1[i]
         END;
      END InclSet;

      PROCEDURE SubSet(VAR s,s1,s2 : Symset);
         VAR
            i : CARDINAL;

      BEGIN
         FOR i := 0 TO setlength DO
            s[i] := s1[i] - s2[i]
         END;
      END SubSet;

      PROCEDURE GenSet1(VAR s,s1 : Symset; sy : Symbol);
      BEGIN
         s := s1;
         Set1(s,sy);
      END GenSet1;

      PROCEDURE GenSet2(VAR s,s1 : Symset; sy1,sy2 : Symbol);
      BEGIN
         s := s1;
         Set2(s,sy1,sy2);
      END GenSet2;

      PROCEDURE GenSet3(VAR s,s1 : Symset; sy1,sy2,sy3 : Symbol);
      BEGIN
         s := s1;
         Set3(s,sy1,sy2,sy3);
      END GenSet3;

   BEGIN                                (* SymsetHandling *)
      FOR i := 0 TO setlength DO
         s0[i] := {}
      END;
   END SymsetHandling;

   VAR
      constbegsys, actypbegsys, facbegsys, mulopsys, addopsys, relopsys,
         exprbegsys, statbegsys, stattersys, simplebegsys, declsys, bodysys,
         blocksys, statbegbodysys, idents, plmis, plmiints, arids, statbegids,
         semistattersys, rparcomma, comrbracks, thenset, iduntstatbegsys,
         toset, bydoset, doset, ofset, beclbrperarr, rparset, startset
	 : Symset;

(* LEGENDE:
  constbegsys = [plus,minus,intcon,cardcon,intcarcon,realcon]
                +[charcon,ident,stringcon,lconbr,lparent,notsy];
  simplebegsys = [ident,lparent,lbrack];
  actypbegsys = simplebegsys+[arraysy,recordsy,setsy,pointersy,proceduresy];
  facbegsys   = [intcon,cardcon,intcarcon,charcon,realcon] +
                [ident,lparent,notsy,stringcon,lconbr];
  mulopsys = [times,slash,divsy,modsy,andsy];
  addopsys = [plus,minus,orsy];
  relopsys = [eql,neq,grt,geq,lss,leq,insy];
  exprbegsys = facbegsys + [plus,minus]
  statbegsys = [ifsy,whilesy,casesy,repeatsy,withsy,loopsy] +
               [exitsy,returnsy,forsy];
  stattersys = [elsifsy,elsesy,untilsy,endsy,variant];
  declsys = [constsy,varsy,typesy,proceduresy,modulesy];
  bodysys = [beginsy,endsy];
  blocksys = declsys + bodysys;
  statbegbodysys = statbegsys + bodysys;
  idents = [ident];
  plmis = [plus,minus];
  plmiints = [plus,minus,intcon,realcon,cardcon,intcarcon];
  arids = [arraysy,ident];
  statbegids = statbegsys + [ident];
  semistattersys = [semicolon]+stattersys;
  rparcomma = [rparent,comma];
  comrbracks = [comma,rbrack];
  thenset = [thensy];
  iduntstatbegsys = [untilsy,semicolon] + statbegids;
  doset = [dosy]; ofset = [ofsy];
  toset = [tosy]; bydoset = [bysy,dosy];
  beclbrperarr = [becomes,lbrack,period,arrow];
  rparset = [rparent];
  startset = statbegsys + declsys + [beginsy,eop];  *)

   PROCEDURE SymSetConstInit;
   BEGIN
      GenSet3(constbegsys,s0,plus,minus,intcon);
      Set3(constbegsys,cardcon,intcarcon,realcon);
      Set3(constbegsys,charcon,ident,stringcon);
      Set3(constbegsys,lconbr,lparent,notsy);
      GenSet3(simplebegsys,s0,ident,lparent,lbrack);
      GenSet2(actypbegsys,simplebegsys,arraysy,recordsy);
      Set3(actypbegsys,setsy,pointersy,proceduresy);
      GenSet3(facbegsys,s0,intcon,cardcon,intcarcon);
      Set2(facbegsys,realcon,charcon);
      Set2(facbegsys,ident,lparent);
      Set3(facbegsys,notsy,stringcon,lconbr);
      GenSet3(mulopsys,s0,times,andsy,divsy);
      Set2(mulopsys,modsy,slash);
      GenSet3(addopsys,s0,plus,minus,orsy);
      GenSet2(relopsys,s0,eql,neq);
      Set2(relopsys,grt,geq);
      Set3(relopsys,lss,leq,insy);
      GenSet2(plmis,s0,plus,minus);
      AddSet(exprbegsys,facbegsys,plmis);
      GenSet3(statbegsys,s0,ifsy,whilesy,repeatsy);
      Set3(statbegsys,casesy,withsy,loopsy);
      Set3(statbegsys,exitsy,returnsy,forsy);
      GenSet3(stattersys,s0,elsifsy,elsesy,variant);
      Set2(stattersys,untilsy,endsy);
      GenSet3(declsys,s0,constsy,typesy,varsy);
      Set2(declsys,proceduresy,modulesy);
      GenSet2(bodysys,s0,beginsy,endsy);
      AddSet(blocksys,declsys,bodysys);
      AddSet(statbegbodysys,statbegsys,bodysys);
      GenSet1(idents,s0,ident);
      GenSet3(plmiints,s0,plus,minus,intcon);
      Set3(plmiints,cardcon,intcarcon,realcon);
      GenSet2(arids,s0,arraysy,ident);
      AddSet(statbegids,statbegsys,idents);
      GenSet1(semistattersys,stattersys,semicolon);
      GenSet2(rparcomma,s0,rparent,comma);
      GenSet2(comrbracks,s0,comma,rbrack);
      GenSet1(thenset,s0,thensy);
      GenSet2(iduntstatbegsys, statbegids, semicolon, untilsy);
      GenSet1(doset,s0,dosy);
      GenSet1(ofset,s0,ofsy);
      GenSet1(toset,s0,tosy);
      GenSet2(bydoset,s0,bysy,dosy);
      GenSet2(beclbrperarr,s0,becomes,lbrack);
      Set2(beclbrperarr,period,arrow);
      GenSet1(rparset,s0,rparent);
      AddSet(startset,statbegsys,declsys);
      Set2(startset,beginsy,eop);
   END SymSetConstInit;

   VAR
      copysepmod : BOOLEAN;             (* separate module belonging to this *)
                                        (* identifier must be copied *)
      globalmod  : BOOLEAN;             (* current module is a global module *)
      defmod     : BOOLEAN;             (* module is a definition module *)

   PROCEDURE PutGetSy;
   BEGIN
      PutS;
      GetSy;
   END PutGetSy;

   PROCEDURE PutDummyConst;
      VAR oldsy: Symbol;
   BEGIN
      typeset := Stset{ints, cards, longcards, longints};
      val.value := 0;
      oldsy := sy;
      sy := intcarcon;
      PutS;
      sy := oldsy;
   END PutDummyConst;

   PROCEDURE PutDmId;
   BEGIN
      GetDmId;
      PutIdent(spix);
   END PutDmId;

   PROCEDURE Skip(VAR fsys: Symset; nr: CARDINAL);
   BEGIN
      Error(nr);
      WHILE NOT InSet(fsys, sy) DO
         GetSy
      END;
   END Skip;

   PROCEDURE Test(VAR s1, s2: Symset; nr: CARDINAL);
      VAR
         s: Symset;

   BEGIN
      IF NOT InSet(s1,sy) THEN
         AddSet(s, s1, s2);
         Skip(s, nr)
      END;
   END Test;

   PROCEDURE TestGet(s: Symbol; nr: CARDINAL);
   BEGIN
      IF sy=s THEN
         GetSy
      ELSE
         Error(nr)
      END;
   END TestGet;

   PROCEDURE TestPutGet(s: Symbol; nr: CARDINAL);
   BEGIN
      IF sy=s THEN
         PutGetSy
      ELSE
         Error(nr);
         PutSy(s)
      END;
   END TestPutGet;

   PROCEDURE TestSemicolon;
   BEGIN
      IF sy = semicolon THEN
         GetSy
      ELSE
         Error(23)
      END;
   END TestSemicolon;

   PROCEDURE TestEndSemi;
   BEGIN
      IF sy<>endsy THEN
         TestSemicolon
      END;
   END TestEndSemi;

   PROCEDURE TestIdent;
   BEGIN
      IF sy = ident THEN
         IF copysepmod THEN
            IF spix <> mainmodp^.name THEN
               GetSeparateModule
            END;
            (* mainmodp points to module SYSTEM *)
         END;
         PutGetSy;
      ELSE
         Error(20);
         PutDmId
      END
   END TestIdent;

   PROCEDURE IdentList;
   BEGIN
      TestIdent;
      WHILE sy=comma DO
         GetSy;
         TestIdent
      END
   END IdentList;

   PROCEDURE QualIdent;
   BEGIN
      TestIdent;
      WHILE sy=period DO
         PutGetSy;
         TestIdent
      END;
   END QualIdent;

   PROCEDURE TestEndsy;
   BEGIN
      TestPutGet(endsy, 40);
   END TestEndsy;

   PROCEDURE ConstElemList(fsys: Symset; list: BOOLEAN);
      VAR
         lfsys: Symset;

   BEGIN
      Set1(fsys, comma);
      GenSet1(lfsys, fsys, range);
      IF InSet(constbegsys, sy) OR list THEN
         LOOP
            Constant(lfsys);
            IF sy=range THEN
               PutGetSy;
               Constant(fsys)
            END;
            IF sy<>comma THEN
               EXIT
            END;
            GetSy
         END
      END
   END ConstElemList;

   PROCEDURE ConstSetConstructor(fsys: Symset);
      VAR
         lfsys : Symset;

   BEGIN
      PutGetSy;
      GenSet1(lfsys,fsys,rconbr);
      ConstElemList(lfsys,FALSE);
      TestPutGet(rconbr,58)
   END ConstSetConstructor;

   PROCEDURE Constant(VAR fsys: Symset);
   (* constant expression *)
      VAR
         lfsys : Symset;

      PROCEDURE ConstSimpleExpr(VAR fsys: Symset);
      (* simple expression in constants *)
         VAR
            lfsys : Symset;

         PROCEDURE ConstTerm(VAR fsys: Symset);
         (* term in constants *)
            VAR
               lfsys : Symset;

            PROCEDURE ConstFactor(VAR fsys: Symset);
            (* factor in constant *)
               VAR
                  lfsys : Symset;

            BEGIN                       (* ConstFactor *)
               IF (sy >= intcon) AND (sy <= stringcon) THEN
                  PutGetSy;
               ELSIF sy = ident THEN
                  QualIdent;
                  IF sy = lconbr THEN
                     ConstSetConstructor(fsys);
		  ELSIF modrev2 AND (sy = lparent) THEN
		     PutGetSy;
		     IF InSet(exprbegsys, sy) THEN
			LOOP
			   GenSet2(lfsys, fsys, rparent, comma);
			   Constant(lfsys);
			   IF sy <> comma THEN
			      EXIT
			   END;
			   PutGetSy;
			END;
		     END;
		     TestPutGet(rparent, 41);
                  END;
               ELSIF sy = lconbr THEN
                  ConstSetConstructor(fsys);
               ELSIF sy = lparent THEN
                  PutGetSy;
                  GenSet1(lfsys,fsys,rparent);
                  Constant(lfsys);
                  TestPutGet(rparent,41);
               ELSIF sy = notsy THEN
                  PutGetSy;
                  ConstFactor(fsys);
               ELSE
                  Error(42);
                  PutDummyConst;
               END;
               Test(fsys,s0,42);
            END ConstFactor;

         BEGIN                          (* ConstTerm *)
            AddSet(lfsys,fsys,mulopsys);
            ConstFactor(lfsys);
            WHILE InSet(mulopsys,sy) DO
               PutGetSy;
               ConstFactor(lfsys);
            END;
         END ConstTerm;

      BEGIN                             (* ConstSimpleExpr *)
         IF InSet(plmis,sy) THEN
            PutGetSy
         END;
         AddSet(lfsys,fsys,addopsys);
         ConstTerm(lfsys);
         WHILE InSet(addopsys,sy) DO
            PutGetSy;
            ConstTerm(lfsys);
         END;
      END ConstSimpleExpr;

   BEGIN                                (* Constant *)
      AddSet(lfsys,fsys,relopsys);
      ConstSimpleExpr(lfsys);
      IF InSet(relopsys,sy) THEN
         PutGetSy;
         ConstSimpleExpr(fsys);
      END;
   END Constant;

   PROCEDURE FormalType(VAR fsys: Symset);
   BEGIN
      Test(arids,fsys,51);
      IF sy = arraysy THEN
         PutGetSy;
         TestGet(ofsy, 36);
      END;
      QualIdent
   END FormalType;

   PROCEDURE ActualType(VAR fsys: Symset);
      VAR
         lsy: Symbol;

      PROCEDURE SimpleType(VAR fsys: Symset);
      (* type-identifier, enumerations, subranges *)
         VAR
            lfsys : Symset;

         PROCEDURE RangeType;
         BEGIN
            PutGetSy;
            GenSet2(lfsys,fsys,range,rbrack);
            Constant(lfsys);
            TestPutGet(range,59);
            GenSet1(lfsys,fsys,rbrack);
            Constant(lfsys);
            TestPutGet(rbrack,22);
         END RangeType;

      BEGIN
         IF sy = ident THEN
            QualIdent;
            IF modrev AND (sy = lbrack) THEN
               RangeType;
            END;
         ELSIF sy = lparent THEN        (* enumeration type *)
            PutGetSy;
            GenSet2(lfsys,fsys,rparent,comma);
            Test(idents,lfsys,20);
            IdentList;
            TestPutGet(rparent,41);
         ELSIF sy = lbrack THEN         (* range type *)
            RangeType;
         ELSE
            Error(49);
            PutDmId;
         END;
         Test(fsys,s0,49);
      END SimpleType;

      PROCEDURE RecordType;
         VAR
            lfsys: Symset;

         PROCEDURE FieldlistSeq(VAR fsys : Symset);
            VAR
               lfsys : Symset;
               ospix : Spellix;

            PROCEDURE VariantFld(VAR fsys : Symset);
               VAR
                  lfsys: Symset;

            BEGIN
               GenSet1(lfsys,fsys,colon);
               ConstElemList(lfsys,TRUE);
               TestPutGet(colon, 37);
               FieldlistSeq(fsys)
            END VariantFld;

         BEGIN
            WHILE (sy = ident) OR (sy = casesy) DO
               IF sy = ident THEN
                  IdentList;
                  TestPutGet(colon, 37);
                  ActualType(fsys)
               ELSE
                  PutGetSy;             (* sy = casesy *)
                  IF sy = ident THEN
                     ospix := spix;
                     GetSy;
                     IF sy = colon THEN
                        PutIdent(ospix);
                        PutGetSy;
                        QualIdent
                     ELSE
                        IF modrev THEN
                           Error(300);
                        END;
                        PutSy(colon);
                        PutIdent(ospix);
                        WHILE sy=period DO
                           PutGetSy;
                           TestIdent
                        END
                     END
                  ELSE
                     IF modrev AND (sy = colon) THEN
                        PutGetSy;
                     ELSE
                        PutSy(colon);
                     END;
                     QualIdent;
                  END;
                  TestGet(ofsy, 36);
                  GenSet2(lfsys,fsys,variant,elsesy);
                  (* in old version: at least one variant *)
                  (* in new version: list of variants may be empty *)
                  LOOP
                     IF modrev THEN
                        (* skip superfluous bars *)
                        WHILE sy = variant DO
                           GetSy;
                        END;
                        IF (sy = elsesy) OR (sy = endsy) THEN
                           EXIT
                        END;
                     END;
                     Test(constbegsys, lfsys, 136);
                     IF NOT InSet(constbegsys, sy) AND (sy <> variant) THEN
                        EXIT
                     END;
                     IF sy <> variant THEN (* else error message printed *)
                        PutSy(ofsy);
                        VariantFld(lfsys);
                     END;
                     IF (sy = elsesy) OR (sy = endsy) THEN
                        EXIT
                     END;
                     (* in old version: exact one bar between variants *)
                     IF sy = variant THEN GetSy END;
                  END;
                  IF sy = elsesy THEN
                     PutGetSy;
                     FieldlistSeq(fsys)
                  END;
                  TestEndsy;
               END;
               IF sy = semicolon THEN
                  GetSy
               ELSIF (sy<>variant) AND (sy<>endsy) AND (sy<>elsesy) THEN
                  GenSet2(lfsys,fsys,casesy,ident);
                  Skip(lfsys,23);
               END
            END
         END FieldlistSeq;

      BEGIN                             (* RecordType *)
         GenSet1(lfsys, fsys, endsy);
         FieldlistSeq(lfsys);
         TestEndsy;
      END RecordType;

      PROCEDURE ArrayType;
         VAR
            lfsys : Symset;

      BEGIN
         GenSet2(lfsys,fsys,comma,ofsy);
         LOOP
            SimpleType(lfsys);
            IF sy <> comma THEN
               EXIT
            END;
            (* revision in interpassfile: put comma *)
            PutGetSy;
         END;
         TestPutGet(ofsy, 36);
         ActualType(fsys);
      END ArrayType;

      PROCEDURE ProcedureType;
         VAR
            lfsys: Symset;

      BEGIN
         IF (sy = lparent) OR (sy = colon) THEN
            IF sy = lparent THEN
               PutGetSy;
               GenSet2(lfsys,fsys,comma,rparent);
               IF sy <> rparent THEN
                  LOOP
                     IF sy = varsy THEN
                        PutGetSy
                     END;
                     FormalType(lfsys);
                     IF sy <> comma THEN
                        EXIT
                     END;
                     GetSy;             (* skip comma *)
                  END;                  (* LOOP *)
               END;
               TestPutGet(rparent,41);
            ELSE
               Error(45);
               PutSy(lparent);
               PutSy(rparent);
            END;
            IF sy = colon THEN
               PutGetSy;
               QualIdent
            END
         END;
      END ProcedureType;

   BEGIN                                (* ActualType *)
      Test(actypbegsys,fsys,44);
      IF InSet(simplebegsys,sy) THEN
         SimpleType(fsys)
      ELSIF InSet(actypbegsys,sy) THEN
         lsy := sy;
         PutGetSy;
         IF lsy = arraysy THEN
            ArrayType
         ELSIF lsy = recordsy THEN
            RecordType
         ELSIF lsy = pointersy THEN
            TestGet(tosy, 56);
            ActualType(fsys);
         ELSIF lsy = setsy THEN
            TestGet(ofsy, 36);
            SimpleType(fsys);
         ELSIF lsy = proceduresy THEN
            ProcedureType
         END
      ELSE
         PutDmId
      END
   END ActualType;

   PROCEDURE Modul(VAR fsys : Symset);
      VAR
         lspix : Spellix;
         lfsys : Symset;

      PROCEDURE Block(VAR fsys : Symset);
         VAR
            lsy   : Symbol;
            lfsys : Symset;

         PROCEDURE ConstDecl;
            VAR
               lfsys : Symset;

         BEGIN
            WHILE sy = ident DO
               PutGetSy;
               TestGet(eql, 43);
               GenSet1(lfsys,fsys,semicolon);
               Constant(lfsys);
               TestEndSemi;
            END;
            GenSet1(lfsys,fsys,endsy);
            Test(lfsys,s0,25);
         END ConstDecl;

         PROCEDURE TypeDecl;
            VAR
               lfsys : Symset;

         BEGIN
            GenSet1(lfsys,fsys,semicolon);
            WHILE sy = ident DO
               PutGetSy;
               IF sy = eql THEN
                  GetSy;
                  ActualType(lfsys);
               ELSIF defmod THEN
                  PutSy(hidden)
               ELSE
                  Error(43);
                  ActualType(lfsys);
               END;
               TestEndSemi;
            END;
            GenSet1(lfsys,fsys,endsy);
            Test(lfsys,s0,25);
         END TypeDecl;

         PROCEDURE VarDecl;
            VAR
               lfsys : Symset;

            PROCEDURE Reference;
               VAR
                  lfsys : Symset;

            BEGIN
               PutGetSy;                (* lbrack *)
               GenSet3(lfsys,fsys,rbrack,comma,colon);
               Constant(lfsys);
               TestPutGet(rbrack, 22)
            END Reference;

         BEGIN
            WHILE sy = ident DO
               PutGetSy;
               IF sy = lbrack THEN
                  Reference
               END;
               WHILE sy = comma DO
                  GetSy;
                  IF sy = ident THEN
                     PutGetSy;
                     IF sy = lbrack THEN
                        Reference
                     END;
                  ELSE
                     Error(20)
                  END
               END;
               TestPutGet(colon, 37);
               GenSet1(lfsys,fsys,semicolon);
               ActualType(lfsys);
               TestEndSemi
            END;
            GenSet1(lfsys,fsys,endsy);
            Test(lfsys,s0,25);
         END VarDecl;

         PROCEDURE ModulDecl;
         BEGIN
            IF defmod THEN
               Error(57)
            END;
            Modul(fsys);
            TestEndSemi;
         END ModulDecl;

         PROCEDURE ProcFunDecl(VAR fsys : Symset);
            VAR
               lspix: Spellix;
               lfsys: Symset;

         BEGIN
            GenSet3(lfsys,fsys,lbrack,lparent,endsy);
            Test(idents,lfsys,20);
            IF sy <> ident THEN
               Error(20);
               PutDmId;
               lspix := spix
            ELSE
               lspix := spix;
               PutGetSy
            END;
            IF (sy = lparent) OR (sy = colon) THEN
               IF sy = lparent THEN
                  PutGetSy;
                  GenSet2(lfsys,fsys,rparent,semicolon);
                  IF sy <> rparent THEN
                     LOOP
                        IF sy = varsy THEN
                           PutGetSy
                        END;
                        IdentList;
                        TestPutGet(colon, 37);
                        FormalType(lfsys);
                        IF sy <> semicolon THEN
                           EXIT
                        END;
                        GetSy;
                     END;               (* LOOP *)
                  END;
                  TestPutGet(rparent, 41);
               ELSE
                  Error(45);
                  PutSy(lparent);
                  PutSy(rparent);
               END;
               IF sy=colon THEN
                  PutGetSy;
                  QualIdent
               END;
            END;
            TestEndSemi;
            IF NOT defmod THEN
               Block(fsys);
               IF sy = ident THEN
                  IF spix <> lspix THEN
                     Error(24)
                  END;
                  GetSy;
               ELSE
                  Error(20)
               END;
               TestEndSemi;
            END;
         END ProcFunDecl;

         PROCEDURE StatSeq( VAR fsys : Symset);
            VAR
               lfsys: Symset;

         BEGIN
            LOOP
               GenSet1(lfsys, fsys, semicolon);
               LOOP
                  Statement(lfsys);
                  IF sy <> semicolon THEN
                     EXIT
                  END;
                  GetSy
               END;                     (* LOOP *)
               IF InSet(stattersys,sy) THEN
                  EXIT
               END;
               Error(23);
               AddSet(lfsys,fsys,semistattersys);
               Skip(lfsys,52);
               WHILE sy = semicolon DO
                  GetSy
               END;
               IF NOT InSet(statbegids,sy) THEN
                  EXIT
               END;
            END;                        (* LOOP *)
         END StatSeq;

         PROCEDURE Statement(VAR fsys : Symset);
            VAR
               lfsys, l2fsys: Symset;
               lsy          : Symbol;

            PROCEDURE Selector(VAR fsys : Symset);
               VAR
                  lsy: Symbol;

            BEGIN
               WHILE (sy=lbrack) OR (sy=period) OR (sy=arrow) DO
                  lsy := sy;
                  PutGetSy;
                  IF lsy = lbrack THEN
                     LOOP
                        Expression(fsys,comrbracks);
                        IF sy <> comma THEN
                           EXIT
                        END;
                        GetSy;
                        PutSy(rbrack);
                        PutSy(lbrack)
                     END;
                     TestPutGet(rbrack, 22)
                  ELSIF lsy = period THEN
                     TestIdent
                  END
               END
            END Selector;

            PROCEDURE Expression(VAR fsys, fsys1: Symset);
               VAR
                  lfsys: Symset;

               PROCEDURE SimpleExpr(fsys: Symset);

                  PROCEDURE Term(fsys: Symset);

                     PROCEDURE Factor(VAR fsys: Symset);

                        PROCEDURE SetConstructor(fsys: Symset);
                           VAR
                              lfsys : Symset;

                           PROCEDURE ElemList(fsys: Symset; list: BOOLEAN);
                              VAR
                                 lfsys: Symset;

                           BEGIN
                              Set1(fsys, comma);
                              GenSet1(lfsys, fsys, range);
                              IF InSet(exprbegsys, sy) OR list THEN
                                 LOOP
                                    Expression(lfsys, lfsys);
                                    IF sy=range THEN
                                       PutGetSy;
                                       Constant(fsys)
                                    END;
                                    IF sy<>comma THEN
                                       EXIT
                                    END;
                                    GetSy
                                 END
                              END
                           END ElemList;

                        BEGIN
                           PutGetSy;
                           GenSet1(lfsys,fsys,rconbr);
                           ElemList(lfsys,FALSE);
                           TestPutGet(rconbr,58)
                        END SetConstructor;

                     BEGIN              (*Factor *)
                        Test(facbegsys,fsys,48);
                        IF sy = ident THEN
                           QualIdent;
                           IF sy=lconbr THEN
                              IF modrev THEN
                                 SetConstructor(fsys);
                              ELSE
                                 ConstSetConstructor(fsys);
                              END;
                           ELSE
                              Selector(fsys);
                              IF sy = lparent THEN
                                 PutGetSy;
                                 IF InSet(exprbegsys, sy) THEN
                                    LOOP
                                       Expression(fsys,rparcomma);
                                       IF sy<>comma THEN
                                          EXIT
                                       END;
                                       PutGetSy
                                    END
                                 END;
                                 TestPutGet(rparent, 41);
                              END
                           END
                        ELSIF sy = lconbr THEN
                           IF modrev THEN
                              SetConstructor(fsys);
                           ELSE
                              ConstSetConstructor(fsys);
                           END;
                        ELSIF sy = lparent THEN
                           PutGetSy;
                           Expression(fsys,rparset);
                           TestPutGet(rparent, 41);
                        ELSIF sy = notsy THEN
                           PutGetSy;
                           Factor(fsys);
                        ELSIF (sy>=intcon) AND (sy<=stringcon) THEN
                           PutGetSy;
                        ELSE
                           PutDummyConst;
                        END;
                     END Factor;

                  BEGIN                 (*Term*)
                     InclSet(fsys, mulopsys);
                     LOOP
                        Factor(fsys);
                        IF NOT InSet(mulopsys,sy) THEN
                           EXIT
                        END;
                        PutGetSy;
                     END;
                  END Term;

               BEGIN                    (*SimpleExpr*)
                  IF InSet(plmis,sy) THEN
                     PutGetSy
                  END;
                  InclSet(fsys, addopsys);
                  LOOP
                     Term(fsys);
                     IF NOT InSet(addopsys,sy) THEN
                        EXIT
                     END;
                     PutGetSy;
                  END;
               END SimpleExpr;

            BEGIN                       (*Expression*)
               AddSet(lfsys,fsys,relopsys);
               SimpleExpr(lfsys);
               IF InSet(relopsys,sy) THEN
                  PutGetSy;
                  SimpleExpr(fsys);
               END;
               Test(fsys1,fsys,27);
            END Expression;

         BEGIN                          (* Statement *)
            (* assignment or call *)
            IF sy = ident THEN
               InitSave;
               PutGetSy;
               Selector(fsys);
               StopSave;
               IF sy = becomes THEN
                  PutGetSy;
                  ReleaseSys;
                  PutSy(comma);      (* to separate variable from expression *)
                  Expression(fsys,semistattersys);
               ELSE
                  PutSy(call);
                  ReleaseSys;
                  IF sy = lparent THEN
                     PutGetSy;
                     GenSet1(lfsys,fsys,semicolon);
                     IF InSet(exprbegsys, sy) THEN
                        LOOP
                           Expression(lfsys, rparcomma);
                           IF sy <> comma THEN
                              EXIT
                           END;
                           PutGetSy
                        END
                     END;
                     TestPutGet(rparent, 41);
                  ELSE
                     PutSy(lparent);
                     PutSy(rparent)     (* TEMPORARY *)
                  END
               END
            ELSIF InSet(statbegsys, sy) THEN
               lsy := sy;
               PutGetSy;
               CASE lsy OF
                 ifsy:                  (* if statement *)
                     Test(exprbegsys,fsys,27);
                     LOOP
                        GenSet3(lfsys,fsys,elsifsy,elsesy,endsy);
                        Expression(lfsys,thenset);
                        TestGet(thensy, 28);
                        GenSet2(lfsys,fsys,elsifsy,elsesy);
                        StatSeq(lfsys);
                        IF sy <> elsifsy THEN
                           EXIT
                        END;
                        PutGetSy;
                     END;
                     IF sy = elsesy THEN
                        PutGetSy;
                        StatSeq(fsys)
                     END;
                     TestEndsy;
               | loopsy:                (* loop statement *)
                     GenSet2(lfsys,statbegids,endsy,semicolon);
                     Test(lfsys,fsys,29);
                     StatSeq(fsys);
                     TestEndsy;
               | repeatsy:              (* repeat statement *)
                     Test(iduntstatbegsys,fsys,31);
                     GenSet1(lfsys,fsys,untilsy);
                     StatSeq(lfsys);
                     TestPutGet(untilsy, 32);
                     Expression(fsys,semistattersys);
               | whilesy:               (* while statement *)
                     GenSet1(lfsys,fsys,dosy);
                     Test(exprbegsys,lfsys,33);
                     Expression(fsys,doset);
                     TestGet(dosy, 34);
                     StatSeq(fsys);
                     TestEndsy;
               | casesy:                (* case statement *)
                     GenSet1(lfsys,fsys,ofsy);
                     Test(exprbegsys,lfsys,35);
                     AddSet(lfsys,fsys,constbegsys);
                     Expression(lfsys,ofset);
                     TestGet(ofsy, 36);
                     GenSet3(lfsys,fsys,variant, elsesy, endsy);
                     GenSet1(l2fsys,fsys, colon);
                     LOOP
                        (* skip superfluous bars *)
                        IF modrev THEN
                           WHILE sy = variant DO
                              GetSy;
                           END;
                           IF (sy = elsesy) OR (sy = endsy) THEN
                              EXIT
                           END;
                        END;
                        PutSy(ofsy);
                        ConstElemList(l2fsys,TRUE);
                        TestPutGet(colon, 37);
                        StatSeq(lfsys);
                        IF sy <> variant THEN
                           EXIT
                        END;
                        GetSy;
                     END;
                     IF sy = elsesy THEN
                        PutGetSy;
                        GenSet1(lfsys, fsys, endsy);
                        StatSeq(lfsys)
                     END;
                     TestEndsy;
               | withsy:                (* with statement *)
                     GenSet1(lfsys,fsys,dosy);
                     Test(idents,lfsys,39);
                     IF sy = ident THEN
                        QualIdent;
                        Selector(fsys)
                     END;
                     TestGet(dosy, 34);
                     StatSeq(fsys);
                     TestEndsy;
               | forsy:                 (* for statement *)
                     TestIdent;
                     TestGet(becomes, 26);
                     PutSy(comma);
                     GenSet3(lfsys,fsys,tosy,bysy,dosy);
                     Set1(lfsys,endsy);
                     Test(exprbegsys, lfsys,60);
                     Expression(lfsys, toset);
                     TestPutGet(tosy, 56);
                     SubSet(lfsys, lfsys, toset);
                     Test(exprbegsys, lfsys, 60);
                     Expression(lfsys, bydoset);
                     IF sy=bysy THEN
                        PutGetSy;
                        GenSet2(lfsys, fsys, dosy, endsy);
                        Constant(lfsys)
                     END;
                     TestGet(dosy, 34);
                     StatSeq(fsys);
                     TestEndsy;
               | returnsy:              (* return statement *)
                     IF InSet(exprbegsys, sy) THEN
                        PutSy(lparent);
                        Expression(fsys, semistattersys);
                        PutSy(rparent)
                     END;
               | exitsy:                (* exit statement *)
               END;                     (* CASE *)
            END
         END Statement;

      BEGIN                             (* Block *)
         Test(blocksys,fsys,25);
         GenSet1(lfsys,fsys,endsy);
         REPEAT
            WHILE InSet(declsys,sy) DO
               lsy := sy;
               PutGetSy;
               IF lsy = varsy THEN
                  VarDecl;
               ELSIF lsy = proceduresy THEN
                  ProcFunDecl(fsys);
               ELSIF lsy = modulesy THEN
                  ModulDecl;
               ELSIF lsy = typesy THEN
                  TypeDecl;
               ELSIF lsy = constsy THEN
                  ConstDecl;
               END;
            END;
            (* consider dummy program *)
            IF NOT (InSet(bodysys,sy) OR (sy=eop)) THEN
               Test(bodysys,fsys,25);
               IF InSet(bodysys,sy) AND NOT defmod THEN
                  PutSy(proceduresy);
                  PutDmId;
                  Block(lfsys);
               END;
            END;
         UNTIL InSet(statbegbodysys,sy) OR (sy = eop);
         IF sy = beginsy THEN
            IF defmod THEN           (* no body allowed in definition module *)
               Error(55);
            END;
            PutGetSy;
            StatSeq(lfsys)
         END;
         IF InSet(statbegsys,sy) THEN
            Error(38);
            PutSy(beginsy);
            StatSeq(lfsys);
         END;
         TestGet(endsy, 40);
         PutSy(endblock);
      END Block;

      PROCEDURE InterfaceList(fsys: Symset);
      BEGIN
         Set1(fsys, semicolon);
         Test(idents, fsys, 20);
         IdentList;
         TestEndSemi
      END InterfaceList;

   BEGIN                                (*Modul*)
      GenSet3(lfsys,fsys,lbrack,semicolon,endsy);
      Set3(lfsys,exportsy,importsy,fromsy);
      Test(idents,lfsys,20);
      IF sy <> ident THEN
         PutDmId;
         lspix := spix;
      ELSE
         lspix := spix;
         PutGetSy;
      END;
      IF sy = lbrack THEN
         PutGetSy;                      (* lbrack *)
         GenSet3(lfsys,fsys,rbrack,semicolon,endsy);
         Set3(lfsys,exportsy,importsy,fromsy);
         Constant(lfsys);
         TestPutGet(rbrack,22);
      END;
      IF sy <> semicolon THEN
	 GenSet1(lfsys, fsys, endsy);
	 Set3(lfsys, exportsy, importsy, fromsy);
	 Skip(lfsys, 23); (* REV AFB 11/84 *)
      ELSE
	 GetSy;
      END;
      WHILE (sy = fromsy) OR (sy = importsy) DO
         copysepmod := globalmod;
         IF sy = fromsy THEN
            PutGetSy;
            TestIdent;
            copysepmod := FALSE;
            IF sy<>importsy THEN
               Error(61)
            ELSE
               GetSy
            END
         ELSE
            PutGetSy
         END;
         GenSet1(lfsys, fsys, endsy);
         InterfaceList(lfsys)
      END;
      IF globalmod THEN
         ReleaseSys;
         copysepmod := FALSE;
         IF SymFilesMissing THEN
            RETURN
         END;
         (* if symbolfiles incomplete then stop compilation *)
      END;
      IF sy=exportsy THEN
         IF globalmod THEN
            IF modrev AND defmod THEN
               Error(301);
            ELSIF NOT defmod THEN
               Error(54)
            END;
         END;
         GetSy;
         IF sy = qualifiedsy THEN
            PutGetSy
         ELSIF globalmod THEN           (* QUALIFIED expected *)
            Error(47);
            PutSy(qualifiedsy)
         ELSE
            PutSy(exportsy)
         END;
         GenSet3(lfsys, fsys, fromsy, importsy, endsy);
         InterfaceList(lfsys)
      END;
      globalmod := FALSE;
      Block(fsys);
      IF sy = ident THEN
         IF lspix <> spix THEN
            Error(24)
         END;
         GetSy;
      ELSE
         Error(20)
      END;
   END Modul;

   PROCEDURE Pass1;
   BEGIN
      InitIdTables;
      InitInOut;
      SymSetConstInit;
      globalmod := TRUE;
      copysepmod := FALSE;
      bigsetroot := NIL;
      GetSy;
      InitSave; (* leading options must not be saved *)
      defmod := MCBase.isdef;
	 (* depends on the suffix but not on the initial keywords *)
      IF defmod THEN
	 IF sy = definitionsy THEN
	    PutGetSy;
	 ELSE
	    PutSy(definitionsy);
	    Error(62);
	    IF sy # modulesy THEN
	       GetSy;
	    END;
	 END;
	 TestGet(modulesy, 46);
      ELSIF sy = implementationsy THEN
	 PutGetSy;
	 TestGet(modulesy,46);
	 IF sy = ident THEN
	    GetSeparateModule;
	 END;
      ELSE                                 (* expect modulesy *)
	 IF sy = definitionsy THEN
	    Error(63); GetSy;
	 END;
	 ismain := TRUE;                   (* always *)
	 TestPutGet(modulesy, 63);
      END;
      Modul(startset);
      IF sy <> period THEN
	 Error(53);
      ELSE
	 GetSy;
	 IF sy <> eop THEN
	    Error(9); (* end of file expected *) (* REV AFB 11/84 *)
	 END;
      END;
      TermInOut;
   END Pass1;

END MCPass1.
