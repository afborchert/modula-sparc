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
   $Id: MCP4CodeSys.m2,v 0.1 1997/02/21 18:40:30 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4CodeSys.m2,v $
   Revision 0.1  1997/02/21  18:40:30  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4CodeSys; (* AFB 3/96 *)

   (* the only assembly output generating module;
      implementation for ``as'' of Solaris 2.x/SPARCv8
   *)

   FROM ASCII IMPORT tab;
   FROM MCBase IMPORT Label, String, oneword, Ident, Idclass, stringmax;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Global IMPORT Assert;
   FROM SYSTEM IMPORT BYTE;
   IMPORT ASCII, Conversions, MCBase, MCMnemonics, MCP4Attributes, MCFatal,
      MCP4Global, MCP4Public, SYSTEM, StdIO, Memory, SysExit, SystemTypes,
      MCP4Scanner, MCP4Types;

   (* (* exported from definition module *)
   TYPE
      Segment = (text, data, rodata);
   *)

   VAR
      currentSegment: Segment;
      intendedSegment: Segment;
      bfline: CARDINAL; (* line of last BEGIN *)

   MODULE Output;

      FROM ASCII IMPORT nl;
      FROM Conversions IMPORT ConvertInteger, ConvertCardinal, ConvertHex,
	 ConvertOctal;
      FROM MCP4Public IMPORT Sflag, ErrorsFound, assName;
      FROM MCP4Global IMPORT Assert;
      FROM MCFatal IMPORT IOFault;
      FROM StdIO IMPORT Fopen, FILE, write, Fputc, Fclose, Fseek, Ftell; 
      FROM SysExit IMPORT EnterCleanup;
      FROM SystemTypes IMPORT OFF;

      EXPORT InitOutput, WriteString, WriteLn, Write, WriteCard, WriteInt,
	 WriteLongCard, WriteLongInt, WriteHex, WriteOct3,
	 SuppressOutput, ContinueOutput;

      VAR
	 out: FILE;
	 open: BOOLEAN; (* output file opened *)
	 suppressOutput: BOOLEAN;
	 outputcount: CARDINAL;

      PROCEDURE InitOutput;
      BEGIN
	 Assert(~open);
	 IF (assName[0] = 0C) OR ErrorsFound THEN
	    (* no output is to be generated *)
	 ELSIF ~Fopen(out, assName, write, (* buffered = *) TRUE) THEN
	    IOFault(assName);
	 ELSE
	    open := TRUE; suppressOutput := FALSE;
	 END;
      END InitOutput;

      PROCEDURE CloseOutput;
      BEGIN
	 IF open THEN
	    IF ~Fclose(out) THEN
	       IOFault(assName);
	    END;
	 END;
	 open := FALSE;
      END CloseOutput;

      PROCEDURE Write(ch: CHAR);
      BEGIN
	 IF open & ~ErrorsFound & ~suppressOutput THEN
	    IF ~Fputc(ch, out) THEN
	       IOFault(assName);
	    END;
	    IF ch = nl THEN
	       INC(outputcount);
	    END;
	 END;
      END Write;

      PROCEDURE SuppressOutput;
      BEGIN
	 Assert(~suppressOutput);
	 suppressOutput := TRUE;
      END SuppressOutput;

      PROCEDURE ContinueOutput;
      BEGIN
	 Assert(suppressOutput);
	 suppressOutput := FALSE;
      END ContinueOutput;

      PROCEDURE WriteLn;
      BEGIN
	 Assert(~suppressOutput);
	 Write(nl);
      END WriteLn;

      PROCEDURE WriteString(s: ARRAY OF CHAR);
	 VAR i: CARDINAL;
      BEGIN
	 i := 0;
	 WHILE (i <= HIGH(s)) & (s[i] # 0C) DO
	    Write(s[i]); INC(i);
	 END;
      END WriteString;

      PROCEDURE WriteCard(c: CARDINAL);
	 VAR str: ARRAY [0..11] OF CHAR;
      BEGIN
	 ConvertCardinal(c, 1, str);
	 WriteString(str);
      END WriteCard;

      PROCEDURE WriteLongCard(c: LONGCARD);
	 VAR str: ARRAY [0..11] OF CHAR;
      BEGIN
	 (* FIXME: TO BE REPLACED BY ConvertLongCardinal *)
	 ConvertCardinal(CARDINAL(c), 1, str);
	 WriteString(str);
      END WriteLongCard;

      PROCEDURE WriteHex(c: CARDINAL);
	 VAR
	    str: ARRAY [0..7] OF CHAR;
	    i: CARDINAL;
      BEGIN
	 ConvertHex(c, 8, str);
	 i := 0;
	 WHILE str[i] = ' ' DO
	    str[i] := '0';
	    INC(i);
	 END;
	 WriteString(str);
      END WriteHex;

      PROCEDURE WriteOct3(c: CARDINAL);
	 CONST
	    len = 3;
	 VAR
	    str: ARRAY [0..len-1] OF CHAR;
	    i: CARDINAL;
      BEGIN
	 ConvertOctal(c, len, str);
	 i := 0;
	 WHILE str[i] = ' ' DO
	    str[i] := '0';
	    INC(i);
	 END;
	 WriteString(str);
      END WriteOct3;

      PROCEDURE WriteInt(i: INTEGER);
	 VAR str: ARRAY [0..11] OF CHAR;
      BEGIN
	 ConvertInteger(i, 1, str);
	 WriteString(str);
      END WriteInt;

      PROCEDURE WriteLongInt(i: LONGINT);
	 VAR str: ARRAY [0..11] OF CHAR;
      BEGIN
	 (* FIXME: ConvertInteger is to be replaced by ConvertLongInteger *)
	 ConvertInteger(i, 1, str);
	 WriteString(str);
      END WriteLongInt;

   BEGIN
      open := FALSE;
      EnterCleanup(CloseOutput);
   END Output;

   MODULE Formats;

      IMPORT ASCII, MCP4Attributes, MCP4Public, MCP4Global, MCBase, Memory,
	 MCP4Scanner, MCMnemonics, Output, SYSTEM, CheckSegment, bfline,
	 MCP4Types;

      EXPORT PushArg, WriteMnem, Print, WriteLabel;

      MODULE Regs;

	 FROM MCP4Attributes IMPORT Reg;
	 FROM MCP4Global IMPORT Assert;
	 FROM Output IMPORT WriteString;

	 EXPORT WriteReg;

	 VAR
	    regname: ARRAY Reg OF ARRAY [0..7] OF CHAR;

	 PROCEDURE WriteReg(r: Reg);
	 BEGIN
	    Assert(r # noReg);
	    WriteString(regname[r]);
	 END WriteReg;

      BEGIN
	 regname[g0] := "%g0"; regname[g1] := "%g1"; regname[g2] := "%g2";
	 regname[g3] := "%g3"; regname[g4] := "%g4"; regname[g5] := "%g5";
	 regname[g6] := "%g6"; regname[g7] := "%g7";
	 regname[o0] := "%o0"; regname[o1] := "%o1"; regname[o2] := "%o2";
	 regname[o3] := "%o3"; regname[o4] := "%o4"; regname[o5] := "%o5";
	 regname[o6] := "%o6"; regname[o7] := "%o7";
	 regname[l0] := "%l0"; regname[l1] := "%l1"; regname[l2] := "%l2";
	 regname[l3] := "%l3"; regname[l4] := "%l4"; regname[l5] := "%l5";
	 regname[l6] := "%l6"; regname[l7] := "%l7";
	 regname[i0] := "%i0"; regname[i1] := "%i1"; regname[i2] := "%i2";
	 regname[i3] := "%i3"; regname[i4] := "%i4"; regname[i5] := "%i5";
	 regname[i6] := "%i6"; regname[i7] := "%i7";
	 regname[y] := "%y";
	 regname[f0] := "%f0"; regname[f1] := "%f1"; regname[f2] := "%f2";
	 regname[f3] := "%f3"; regname[f4] := "%f4"; regname[f5] := "%f5";
	 regname[f6] := "%f6"; regname[f7] := "%f7"; regname[f8] := "%f8";
	 regname[f9] := "%f9"; regname[f10] := "%f10"; regname[f11] := "%f11";
	 regname[f12] := "%f12"; regname[f13] := "%f13"; regname[f14] := "%f14";
	 regname[f15] := "%f15"; regname[f16] := "%f16"; regname[f17] := "%f17";
	 regname[f18] := "%f18"; regname[f19] := "%f19"; regname[f20] := "%f20";
	 regname[f21] := "%f21"; regname[f22] := "%f22"; regname[f23] := "%f23";
	 regname[f24] := "%f24"; regname[f25] := "%f25"; regname[f26] := "%f26";
	 regname[f27] := "%f27"; regname[f28] := "%f28"; regname[f29] := "%f29";
	 regname[f30] := "%f30"; regname[f31] := "%f31";
	 regname[fsr] := "%fsr";
      END Regs;

      MODULE Args;

	 FROM MCP4Global IMPORT Assert;
	 FROM SYSTEM IMPORT BYTE, ADDRESS, ADR;

	 EXPORT PushArg, NextArg, AllArgs, RewindArgs, SkipRestOfArgs;

	 CONST
	    maxarg = 10;
	 TYPE
	    Arg =
	       RECORD
		  addr: ADDRESS;
		  size: CARDINAL;
	       END;
	 VAR
	    args: ARRAY [0..maxarg-1] OF Arg;
	    argcnt: CARDINAL;
	    index: CARDINAL;

	 PROCEDURE PushArg(VAR arg: ARRAY OF BYTE);
	 BEGIN
	    Assert((index = 0) & (argcnt < maxarg));
	    WITH args[argcnt] DO
	       addr := ADR(arg);
	       size := SIZE(arg);
	    END;
	    INC(argcnt);
	 END PushArg;

	 PROCEDURE NextArg(VAR a: ADDRESS; VAR s: CARDINAL);
	 BEGIN
	    Assert(index < argcnt);
	    WITH args[index] DO
	       a := addr;
	       s := size;
	    END;
	    INC(index);
	 END NextArg;

	 PROCEDURE AllArgs;
	 BEGIN
	    Assert(index = argcnt);
	    index := 0; argcnt := 0;
	 END AllArgs;

	 PROCEDURE RewindArgs;
	 BEGIN
	    Assert(index = argcnt);
	    index := 0;
	 END RewindArgs;

	 PROCEDURE SkipRestOfArgs;
	 BEGIN
	    index := 0; argcnt := 0;
	 END SkipRestOfArgs;

      BEGIN
	 argcnt := 0; index := 0;
      END Args;

      MODULE Printing;

	 FROM Args IMPORT NextArg, AllArgs, RewindArgs, SkipRestOfArgs;
	 FROM MCP4Attributes IMPORT Attribute, Reg, RegSet, AtMode, Simm13,
	    ArithmeticType;
	 FROM MCBase IMPORT Label, String, Symbol, nilvalue, oneword,
	    Type, Size, Ident, Structform, Stset, Idclass, Idset, Kindvar;
	 FROM MCP4Global IMPORT Assert;
	 FROM MCP4Scanner IMPORT line;
	 FROM MCP4Types IMPORT IsReal, Signed, ByteSize;
	 FROM Memory IMPORT CheckPtr;
	 FROM MCMnemonics IMPORT Mnemonic, Mnem, genericMnemonics;
	 FROM Output IMPORT WriteString, WriteLn, Write, WriteCard, WriteInt,
	    WriteLongCard, WriteLongInt, WriteHex, WriteOct3,
	    SuppressOutput, ContinueOutput;
	 FROM Regs IMPORT WriteReg;
	 FROM SYSTEM IMPORT ADDRESS;
	 IMPORT ASCII, bfline, CheckSegment;

	 EXPORT WriteMnem, Print, WriteLabel;

	 VAR
	    start: BOOLEAN; (* at beginning of a new line? *)
	    comment: BOOLEAN; (* inside of a comment? *)
	    lastline: INTEGER;

	 PROCEDURE WriteLabel(l: Label);
	 BEGIN
	    WITH l DO
	       Assert(ok);
	       WriteString(".L");
	       IF head # 0C THEN
		  Write(head);
	       END;
	       Write('.');
	       IF n1 # 0 THEN
		  WriteCard(n1);
	       END;
	       IF n2 # 0 THEN
		  Write('.'); WriteCard(n2);
	       END;
	    END;
	 END WriteLabel;

	 PROCEDURE WriteTab;
	 BEGIN
	    Write(ASCII.tab); start := FALSE;
	 END WriteTab;

	 PROCEDURE WriteLine;
	 BEGIN
	    lastline := line;
	    WriteString("/* @ "); WriteInt(lastline);
	    WriteString(" */"); WriteLn;
	    IF bfline # 0 THEN
	       (*
	       WriteTab; WriteString(".stabn"); WriteTab;
	       WriteString("0104,0,"); WriteInt(lastline); WriteString(",.");
	       WriteLn;
	       *)
	       start := TRUE;
	    END;
	 END WriteLine;

	 PROCEDURE WriteMnem(m: Mnemonic);
	    VAR
	       typtr: POINTER TO Type;
	       type: Type;
	       size: CARDINAL;
	       tsize: Size;
	 BEGIN
	    Assert(start);
	    IF line > lastline THEN
	       WriteLine;
	    END;
	    WriteTab; (* start = FALSE *)

	    WriteString(Mnem[m]);
	    IF m IN genericMnemonics THEN
	       (* append appropriate suffix *)
	       NextArg(typtr, size); Assert(size = SIZE(Type)); type := typtr^;
	       tsize := type^.size;
	       CASE m OF
	       | LOAD, STORE:
		     IF type^.size < oneword THEN
			IF m = LOAD THEN
			   IF Signed(type) THEN
			      Write("s");
			   ELSE
			      Write("u");
			   END;
			END;
			IF ByteSize(type) THEN
			   Write("b");
			ELSE
			   Write("h");
			END;
		     ELSIF type^.size = 2 * oneword THEN
			Write("d");
		     END;
               | FiTO, FSQRT, FADD, FSUB, FMUL, FDIV, FCMP, FCMPE:
		     CASE type^.size OF
		     | 4:  Write("s");
		     | 8:  Write("d");
		     | 16: Write("q");
		     ELSE
			Assert(FALSE);
		     END;
	       ELSE
		  Assert(FALSE);
	       END;
	    END;
	 END WriteMnem;

	 PROCEDURE WriteComment;
	 BEGIN
	    IF ~comment THEN
	       IF start THEN
		  WriteString("/*   ");
		  start := FALSE;
	       ELSE
		  WriteTab; WriteString("/* ");
	       END;
	       comment := TRUE;
	    END;
	 END WriteComment;

	 PROCEDURE Print(format: ARRAY OF CHAR);

	    CONST
	       (* configuration parameters *)
	       symprefix = "M.";
	    TYPE
	       CharSet = SET OF CHAR;
	    CONST
	       Printable = CharSet{ASCII.tab, ' '..'~'};
	       Legal = CharSet{'a'..'z', 'A'..'Z', ',', '.', '_', '+', '-'};
	    VAR
	       ch: CHAR;	(* ch = format[index] *)
	       index: CARDINAL;	(* in format *)
	       mnem: BOOLEAN;   (* mnemonic written *)
	       tab: BOOLEAN;	(* tab after mnem written? *)
	       deflabel: BOOLEAN; (* %:l *)

	    PROCEDURE NextCh;
	    BEGIN
	       INC(index);
	       Assert((index <= HIGH(format)) & (format[index] # 0C));
	       ch := format[index];
	    END NextCh;

	    PROCEDURE Advance;
	       (* advance index but do not modify ch *)
	       VAR
		  keepch: CHAR;
	    BEGIN
	       INC(index);
	    END Advance;

	    PROCEDURE EndOfFormat() : BOOLEAN;
	    BEGIN
	       RETURN (index > HIGH(format)) OR (format[index] = 0C)
	    END EndOfFormat;

	    PROCEDURE CTab;
	    BEGIN
	       IF mnem & ~tab THEN
		  WriteTab;
		  tab := TRUE;
	       END;
	    END CTab;

	    PROCEDURE WriteIdent(ip: Ident);
	       (* write symbol name of modules, procedures and variables *)
	    BEGIN
	       WITH ip^ DO
		  WriteString(symprefix);
		  IF (klass = mods) & globalmodule THEN
		     WriteString(identifier);
		  ELSE
		     WriteString(globmodp^.identifier);
		     Assert(klass IN Idset{pures, funcs, vars, mods});
		     CASE klass OF
		     | pures, funcs, mods:
		              Write("_"); WriteCard(procnum);
		     | vars:  Assert((state = global) OR (state = separate));
			      WriteString(".V+"); WriteCard(vaddr);
		     ELSE
			Assert(FALSE);
		     END;
		  END;
	       END;
	    END WriteIdent;

	    PROCEDURE TakeIdent;
	       VAR
		  idptr: POINTER TO Ident;
		  ip: Ident;
		  size: CARDINAL;
	    BEGIN
	       NextArg(idptr, size); Assert(size = SIZE(Ident));
	       ip := idptr^;
	       WriteIdent(ip);
	    END TakeIdent;

	    PROCEDURE TakeAttribute;
	       VAR
		  atptr: POINTER TO Attribute;
		  size: CARDINAL;
		  at: Attribute;
		  newsize: Size;

	       PROCEDURE WriteIndex;
		  VAR
		     sthprinted: BOOLEAN;
	       BEGIN
		  Write("[");
		  sthprinted := FALSE;
		  WITH at^ DO (* indexMode *)
		     Assert(mode = indexMode);
		     IF reg # noReg THEN
		        Assert(reg IN RegSet{g1..i7});
			WriteReg(reg); sthprinted := TRUE;
		     END;
		     IF (labelip # NIL) OR rlabel.ok THEN
			IF reg # noReg THEN
			   Write("+");
			END;
			WriteString("%lo(");
			IF labelip # NIL THEN
			   WriteIdent(labelip);
			ELSE
			   WriteLabel(rlabel);
			END;
			Write(")");
			sthprinted := TRUE;
		     END;
		     Assert(sthprinted);
		     IF addr # 0 THEN
			IF addr < 0 THEN
			   WriteInt(addr);
			ELSE
			   Write("+");
			   WriteInt(addr);
			END;
			Assert(xreg = noReg);
		     ELSIF xreg # noReg THEN
			Write("+");
			WriteReg(xreg);
		     END;
		  END;
		  Write("]");
	       END WriteIndex;

	    BEGIN (* TakeAttribute *)
	       NextArg(atptr, size); at := atptr^;
	       Assert((size = SIZE(Attribute)) & CheckPtr(at));
	       WITH at^ DO
		  CASE mode OF
		  | procAt:         WriteIdent(atip);
		  | regMode,
		    floatRegMode:   WriteReg(reg);
		  | addrMode:       Assert(reg IN RegSet{g1..i7});
		                    Write("["); WriteReg(reg); Write("]");
		  | indexMode:      WriteIndex;
		  | simm13Mode:     WriteInt(simm13val);
		  ELSE
		     Assert(FALSE);
		  END;
	       END;
	    END TakeAttribute;

	    PROCEDURE TakeCardinal;
	       TYPE
		  LongCardPtr = POINTER TO LONGCARD;
	       VAR
		  cardptr: POINTER TO CARDINAL;
		  longcardptr: LongCardPtr;
		  size: CARDINAL;
	    BEGIN
	       NextArg(cardptr, size);
	       IF size = SIZE(CARDINAL) THEN
		  WriteCard(cardptr^);
	       ELSIF size = SIZE(LONGCARD) THEN
		  longcardptr := LongCardPtr(cardptr);
		  WriteLongCard(longcardptr^);
	       ELSE
		  Assert(FALSE);
	       END;
	    END TakeCardinal;

	    PROCEDURE TakeInteger;
	       TYPE
		  LongIntPtr = POINTER TO LONGINT;
	       VAR
		  intptr: POINTER TO INTEGER;
		  longintptr: LongIntPtr;
		  size: CARDINAL;
	    BEGIN
	       NextArg(intptr, size);
	       IF size = SIZE(INTEGER) THEN
		  WriteInt(intptr^);
	       ELSIF size = SIZE(LONGINT) THEN
		  longintptr := LongIntPtr(intptr);
		  WriteLongInt(longintptr^);
	       ELSE
		  Assert(FALSE);
	       END;
	    END TakeInteger;

	    PROCEDURE TakeLabel;
	       VAR
		  labptr: POINTER TO Label;
		  size: CARDINAL;
	    BEGIN
	       NextArg(labptr, size); Assert(size = SIZE(Label));
	       WriteLabel(labptr^);
	    END TakeLabel;

	    PROCEDURE TakeRegister;
	       VAR
		  regptr: POINTER TO Reg;
		  size: CARDINAL;
	    BEGIN
	       NextArg(regptr, size); Assert(size = SIZE(Reg));
	       WriteReg(regptr^);
	    END TakeRegister;

	    PROCEDURE TakeIndirect;
	       
	       (* %[]  inside [] one of
		       r
		       r,d
		       r,r
		       permitted
		       %-sequences instead of `d' permitted
		       `r' may be followed by 2, 4, or 8 (scale)
	       *)

	       VAR
		  opcnt: CARDINAL;
		  comma: BOOLEAN;
		  commaJustSeen: BOOLEAN;

	       PROCEDURE GetCh() : BOOLEAN;
	       BEGIN
		  Assert(~EndOfFormat());
		  ch := format[index];
		  IF ch # ']' THEN
		     Advance;
		  END;
		  RETURN ch # ']'
	       END GetCh;

	       PROCEDURE UngetCh;
	       BEGIN
		  Assert(index > 0);
		  DEC(index);
	       END UngetCh;

	       PROCEDURE Separator(neg: BOOLEAN);
	       BEGIN
		  IF neg THEN
		     Write("-");
		  ELSE
		     IF commaJustSeen THEN
			Write("+");
		     END;
		  END;
		  commaJustSeen := FALSE;
	       END Separator;

	       PROCEDURE TakeRegister;
		  VAR
		     regptr: POINTER TO Reg;
		     size: CARDINAL;
	       BEGIN
		  NextArg(regptr, size); Assert(size = SIZE(Reg));
		  IF regptr^ # noReg THEN
		     Assert(regptr^ IN RegSet{g1..i7});
		     Separator((* neg = *) FALSE);
		     WriteReg(regptr^);
		  END;
	       END TakeRegister;

	       PROCEDURE TakeSimm13;
		  TYPE
		     LongIntPtr = POINTER TO LONGINT;
		  VAR
		     intptr: POINTER TO INTEGER;
		     longintptr: LongIntPtr;
		     size: CARDINAL;
		     simm13: INTEGER;
	       BEGIN
		  NextArg(intptr, size);
		  IF size = SIZE(INTEGER) THEN
		     simm13 := intptr^;
		  ELSIF size = SIZE(LONGINT) THEN
		     longintptr := LongIntPtr(intptr);
		     simm13 := longintptr^;
		  ELSE
		     Assert(FALSE);
		  END;
		  Assert((simm13 >= MIN(Simm13)) & (simm13 <= MAX(Simm13)));
		  IF simm13 >= 0 THEN
		     Separator((* neg = *) FALSE);
		     WriteInt(simm13);
		  ELSE
		     Separator((* neg = *) TRUE);
		     WriteInt(- simm13);
		  END;
	       END TakeSimm13;

	    BEGIN (* TakeIndirect *)
	       opcnt := 0; commaJustSeen := FALSE; comma := FALSE;
	       Write("[");
	       WHILE GetCh() DO
		  IF ch # "," THEN INC(opcnt) END;
		  CASE ch OF
		  | 'd':   TakeSimm13;
		  | '%':   Separator((* neg = *) FALSE);
			   ch := format[index];
			   WorkupParameter;
			   Advance;
		  | 'r':   TakeRegister;
		  | ',':   Assert(~comma & (opcnt > 0));
			   commaJustSeen := TRUE; comma := TRUE;
		  ELSE
		     Assert(FALSE);
		  END;
	       END;
	       Write("]");
	    END TakeIndirect;

	    PROCEDURE TakeCharacterString;
	       TYPE
		  CharPtr = POINTER TO CHAR;
	       VAR
		  s: CharPtr;
		  p: ADDRESS;
		  len: CARDINAL;
	    BEGIN
	       NextArg(p, len);
	       LOOP
		  IF len = 0 THEN EXIT END;
		  s := CharPtr(p); INC(p, SIZE(CHAR)); DEC(len);
		  IF s^ = 0C THEN EXIT END;
		  Write(s^);
	       END;
	    END TakeCharacterString;

	    PROCEDURE DefineString;
	       TYPE
		  CharPtr = POINTER TO CHAR;
	       VAR
		  s: CharPtr;
		  p: ADDRESS;
		  len: CARDINAL;
		  cnt: CARDINAL;
	    BEGIN
	       Assert(~comment);
	       cnt := 0;
	       NextArg(p, len);
	       LOOP
		  IF len = 0 THEN EXIT END;
		  s := CharPtr(p); INC(p, SIZE(CHAR)); DEC(len);
		  IF s^ = 0C THEN EXIT END;
		  IF cnt MOD 30 = 0 THEN
		     IF cnt > 0 THEN
			Write('"'); WriteLn;
		     END;
		     WriteTab; WriteString(".ascii"); WriteTab;
		     Write('"');
		  END;
		  INC(cnt);
		  IF s^ IN (Printable - CharSet{ASCII.tab, '"', '\'}) THEN
		     Write(s^);
		  ELSE
		     Write('\');
		     CASE s^ OF
		     | ASCII.tab:   Write('t');
		     | '\':         Write('\');
		     | '"':         Write('"');
		     ELSE
			WriteOct3(ORD(s^));
		     END;
		  END;
	       END;
	       IF cnt > 0 THEN
		  (* pad string to next oneword-boundary *)
		  Write('\'); WriteOct3(0); INC(cnt);
		  WHILE cnt MOD oneword # 0 DO
		     Write('\'); WriteOct3(0); INC(cnt);
		  END;
		  Write('"');
	       END;
	    END DefineString;

	    PROCEDURE Import;
	    BEGIN
	       Export;
	    END Import;

	    PROCEDURE Export;
	    BEGIN
	       Assert(start);
	       WriteTab; WriteString(".globl"); WriteTab;
	    END Export;

	    PROCEDURE LabelDef;
	    BEGIN
	       Assert(start);
	       WriteTab; TakeLabel;
	       WriteString("="); WriteTab;
	    END LabelDef;

	    PROCEDURE TakeUnary(unaryop: ARRAY OF CHAR);

	       PROCEDURE GetCh() : BOOLEAN;
	       BEGIN
		  Assert(~EndOfFormat());
		  ch := format[index];
		  IF ch # ')' THEN
		     Advance;
		  END;
		  RETURN ch # ')'
	       END GetCh;

	    BEGIN
               WriteString(unaryop); Write('(');
               WHILE GetCh() DO
		  IF ch = "%" THEN
		     Assert(GetCh());
		     WorkupParameter;
		  ELSE
		     Assert(ch IN Legal);
		     Write(ch);
		  END;
               END;
               Write(')');
	    END TakeUnary;

	    PROCEDURE WorkupParameter;
	    BEGIN
	       CASE ch OF
	       | 'a': CTab; TakeAttribute;
	       | 'c': CTab; TakeCardinal;
	       | 'e': Import;
	       | 'g': Export;
	       | 'H': CTab; NextCh; Assert(ch = "("); NextCh; TakeUnary("%hi");
	       | 'i': CTab; TakeInteger;
	       | 'l': CTab; TakeLabel;
	       | 'L': CTab; NextCh; Assert(ch = "("); NextCh; TakeUnary("%lo");
	       | 'n': CTab; TakeIdent;
	       | 'r': CTab; TakeRegister;
	       | '[': CTab; NextCh; TakeIndirect;
	       | 's': CTab; TakeCharacterString;
	       | '*': WriteComment;
	       | '_': CTab; WriteString(symprefix);
	       | '=': LabelDef;
	       ELSE
		  Assert(FALSE);
	       END;
	    END WorkupParameter;

	    PROCEDURE BSSDefinition;

	       VAR
		  comma: BOOLEAN;

	       PROCEDURE GetCh() : BOOLEAN;
	       BEGIN
		  Assert(~EndOfFormat());
		  ch := format[index];
		  IF ch # ')' THEN
		     Advance;
		  END;
		  RETURN ch # ')'
	       END GetCh;

	    BEGIN
	       tab := TRUE; start := FALSE; comma := FALSE;
	       Assert(GetCh()); (* skip "(" *)

	       WriteTab;
	       WriteString(".reserve");
	       WriteTab;

               WHILE GetCh() DO
		  CASE ch OF
		  | "%":   Assert(GetCh()); WorkupParameter;
		  | ",":   Assert(~comma); comma := TRUE; Write(",");
		  ELSE
		     Assert(FALSE);
		  END;
               END;
	       Assert(comma);
	       Write(",");
	       WriteString('".bss"');
	       WriteString(",8"); (* alignment *)
	    END BSSDefinition;

	 BEGIN (* Print *)
	    IF start & (lastline < line) THEN
	       WriteLine;
	    END;
	    CheckSegment;
	    mnem := ~start; tab := FALSE;
	    deflabel := FALSE;
	    index := 0;
	    WHILE ~EndOfFormat() DO
	       ch := format[index];
	       IF ch = '%' THEN
		  NextCh;
		  IF ch = ':' THEN
		     NextCh;
		     Assert((ch = '=') OR start);
		     CASE ch OF
		     | 'b': NextCh; BSSDefinition;
		     | 'D': WriteTab;
		     | 'l': deflabel := TRUE; tab := TRUE; start := FALSE;
		     | 'L': WriteTab; WriteString(".word"); WriteTab;
		     | 's': DefineString;
		     | '=': Assert(~start); WriteTab; Write('='); WriteTab;
		     ELSE
			Assert(FALSE);
		     END;
		  ELSIF ch = 'A' THEN
		     Assert(mnem & ~tab); WriteString(",a");
		  ELSE
		     WorkupParameter;
		  END;
	       ELSE
		  Assert((ch IN Printable) & comment OR (ch IN Legal));
		  CTab;
		  Write(ch);
		  start := FALSE;
	       END;
	       Advance;
	    END;
	    IF deflabel THEN
	       Write(':');
	    END;
	    IF comment THEN
	       WriteString(" */");
	    END;
	    WriteLn;
	    AllArgs;
	    start := TRUE; comment := FALSE;
	 END Print;

      BEGIN
	 start := TRUE; comment := FALSE; lastline := 0;
      END Printing;

   END Formats;

   PROCEDURE Emit(m: Mnemonic; s: ARRAY OF CHAR);
   BEGIN
      WriteMnem(m); Print(s);
   END Emit;

   PROCEDURE Emit1(m: Mnemonic; s: ARRAY OF CHAR; p1: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); WriteMnem(m); Print(s);
   END Emit1;

   PROCEDURE Emit2(m: Mnemonic; s: ARRAY OF CHAR; p1, p2: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); WriteMnem(m); Print(s);
   END Emit2;

   PROCEDURE Emit3(m: Mnemonic; s: ARRAY OF CHAR; p1, p2, p3: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); WriteMnem(m); Print(s);
   END Emit3;

   PROCEDURE Emit4(m: Mnemonic; s: ARRAY OF CHAR;
		   p1, p2, p3, p4: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4);
      WriteMnem(m); Print(s);
   END Emit4;

   PROCEDURE Emit5(m: Mnemonic; s: ARRAY OF CHAR;
		   p1, p2, p3, p4, p5: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      WriteMnem(m); Print(s);
   END Emit5;

   PROCEDURE Emit6(m: Mnemonic; s: ARRAY OF CHAR;
		   p1, p2, p3, p4, p5, p6: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      PushArg(p6);
      WriteMnem(m); Print(s);
   END Emit6;

   PROCEDURE Emit7(m: Mnemonic; s: ARRAY OF CHAR;
                   p1, p2, p3, p4, p5, p6, p7: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      PushArg(p6); PushArg(p7);
      WriteMnem(m); Print(s);
   END Emit7;

   PROCEDURE Emit8(m: Mnemonic; s: ARRAY OF CHAR;
                   p1, p2, p3, p4, p5, p6, p7, p8: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      PushArg(p6); PushArg(p7); PushArg(p8);
      WriteMnem(m); Print(s);
   END Emit8;

   PROCEDURE Emit9(m: Mnemonic; s: ARRAY OF CHAR;
		   p1, p2, p3, p4, p5, p6, p7, p8, p9: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      PushArg(p6); PushArg(p7); PushArg(p8); PushArg(p9);
      WriteMnem(m); Print(s);
   END Emit9;

   PROCEDURE StrEmit(s: ARRAY OF CHAR);
   BEGIN
      Print(s);
   END StrEmit;

   PROCEDURE StrEmit1(s: ARRAY OF CHAR; p1: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); Print(s);
   END StrEmit1;

   PROCEDURE StrEmit2(s: ARRAY OF CHAR; p1, p2: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); Print(s);
   END StrEmit2;

   PROCEDURE StrEmit3(s: ARRAY OF CHAR; p1, p2, p3: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); Print(s);
   END StrEmit3;

   PROCEDURE StrEmit4(s: ARRAY OF CHAR; p1, p2, p3, p4: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); Print(s);
   END StrEmit4;

   PROCEDURE StrEmit5(s: ARRAY OF CHAR; p1, p2, p3, p4, p5: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      Print(s);
   END StrEmit5;

   PROCEDURE StrEmit6(s: ARRAY OF CHAR; p1, p2, p3, p4, p5, p6: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      PushArg(p6);
      Print(s);
   END StrEmit6;

   PROCEDURE StrEmit7(s: ARRAY OF CHAR;
                   p1, p2, p3, p4, p5, p6, p7: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      PushArg(p6); PushArg(p7);
      Print(s);
   END StrEmit7;

   PROCEDURE StrEmit8(s: ARRAY OF CHAR;
                   p1, p2, p3, p4, p5, p6, p7, p8: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      PushArg(p6); PushArg(p7); PushArg(p8);
      Print(s);
   END StrEmit8;

   PROCEDURE StrEmit9(s: ARRAY OF CHAR;
		   p1, p2, p3, p4, p5, p6, p7, p8, p9: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      PushArg(p6); PushArg(p7); PushArg(p8); PushArg(p9);
      Print(s);
   END StrEmit9;

   PROCEDURE StrEmit10(s: ARRAY OF CHAR;
		   p1, p2, p3, p4, p5, p6, p7, p8, p9, p10: ARRAY OF BYTE);
   BEGIN
      PushArg(p1); PushArg(p2); PushArg(p3); PushArg(p4); PushArg(p5);
      PushArg(p6); PushArg(p7); PushArg(p8); PushArg(p9); PushArg(p10);
      Print(s);
   END StrEmit10;

   (* pseudo operations *)

   PROCEDURE CheckSegment;
   BEGIN
      IF intendedSegment # currentSegment THEN
	 Write(tab);
	 WriteString(".section");
	 Write(tab);
	 Write('"');
	 CASE intendedSegment OF
	 | text:     WriteString(".text");
	 | data:     WriteString(".data");
	 | rodata:   WriteString(".rodata");
	 END;
	 Write('"');
	 WriteLn;
	 currentSegment := intendedSegment;
      END;
   END CheckSegment;

   PROCEDURE SetSegment(s: Segment);
   BEGIN
      intendedSegment := s;
   END SetSegment;

   PROCEDURE EmitAlign; (* word (= 2 bytes) alignment *)
   BEGIN
      Write(tab); WriteString(".align"); Write(tab); Write('2'); WriteLn;
   END EmitAlign;

   PROCEDURE EmitAlign4; (* 4-byte-alignment *)
   BEGIN
      Write(tab); WriteString(".align"); Write(tab); Write('4'); WriteLn;
   END EmitAlign4;

   PROCEDURE EmitAlign8; (* 8-byte-alignment *)
   BEGIN
      Write(tab); WriteString(".align"); Write(tab); Write('8'); WriteLn;
   END EmitAlign8;

   PROCEDURE EmitLabel(l: Label); (* define label *)
   BEGIN
      CheckSegment;
      WriteLabel(l); Write(':'); WriteLn;
   END EmitLabel;

   PROCEDURE EmitString(s: String); (* define label and string *)
      TYPE
	 CharSet = SET OF CHAR;
      CONST
	 Printable = CharSet{' '..'~'} - CharSet{'"', '\'};
      TYPE
	 CharPtr = POINTER TO ARRAY [0..stringmax] OF CHAR;
      VAR
	 cnt: CARDINAL; (* number of characters printed *)
	 ch: CHAR;	(* character of `s' *)
	 cp: CharPtr;
   BEGIN
      CheckSegment;
      EmitAlign4;
      WITH s^ DO
	 WriteLabel(label); Write(':');
	 cp := CharPtr(valentry);
      END;
      cnt := 0;
      LOOP
	 ch := cp^[cnt];
	 IF cnt MOD 30 = 0 THEN
	    IF cnt > 0 THEN Write('"'); WriteLn END;
	    Write(tab); WriteString(".ascii"); Write(tab);
	    Write('"');
	 END;
	 INC(cnt);
	 IF ch IN Printable THEN
	    Write(ch);
	 ELSE
	    Write('\');
	    CASE ch OF
	    | tab:   Write('t');
	    | '\':   Write('\');
	    | '"':   Write('"');
	    ELSE
	       WriteOct3(ORD(ch));
	    END;
	 END;
	 IF ch = 0C THEN EXIT END;
      END;
      IF cnt > 0 THEN
	 (* pad string to next oneword-boundary *)
	 WHILE cnt MOD oneword # 0 DO
	    Write('\'); WriteOct3(0); INC(cnt);
	 END;
	 Write('"');
      END;
      WriteLn;
   END EmitString;

   PROCEDURE EmitHeader(modp: Ident); (* module header and key *)
   BEGIN
      WITH modp^ DO
	 Assert(klass = mods);
	 StrEmit4("%g%n_%c_%c_%c",
	    modp, modulekey[0], modulekey[1], modulekey[2]);
	 StrEmit4("%:l%n_%c_%c_%c",
	    modp, modulekey[0], modulekey[1], modulekey[2]);
      END;
   END EmitHeader;

   PROCEDURE EmitKey(extmodp: Ident); (* key reference to imported module *)
   BEGIN
      WITH extmodp^ DO
	 Assert(klass = mods);
	 StrEmit4("%e%n_%c_%c_%c",
	    extmodp, modulekey[0], modulekey[1], modulekey[2]);
      END;
   END EmitKey;

   (* generations of stabs and line numbers *)

   PROCEDURE EmitFileName(filename: ARRAY OF CHAR);
      VAR
	 shortname: ARRAY [0..13] OF CHAR;
	 sindex, index: CARDINAL;
   BEGIN
      index := 0; sindex := 0;
      WHILE (index <= HIGH(filename)) & (filename[index] # 0C) DO
	 IF filename[index] = '/' THEN
	    sindex := 0;
	 ELSIF sindex <= HIGH(shortname) THEN
	    shortname[sindex] := filename[index];
	    INC(sindex);
	 END;
	 INC(index);
      END;
      IF sindex <= HIGH(shortname) THEN
	 shortname[sindex] := 0C;
      END;
      Write(tab);
      WriteString(".file");
      Write(tab);
      Write('"');
      WriteString(shortname);
      Write('"');
      WriteLn;
   END EmitFileName;

   PROCEDURE EmitBeginBlock1(blockp: Ident; line: CARDINAL);
   BEGIN
      WITH blockp^ DO
      END;
   END EmitBeginBlock1;

   PROCEDURE EmitBeginBlock2(blockp: Ident; line: CARDINAL);
   BEGIN
      bfline := line;
   END EmitBeginBlock2;

   PROCEDURE EmitEndBlock1(blockp: Ident; line: CARDINAL);
   BEGIN
   END EmitEndBlock1;

   PROCEDURE EmitEndBlock2(blockp: Ident; line: CARDINAL);
   BEGIN
      bfline := 0;
   END EmitEndBlock2;

   PROCEDURE EmitLineNumer(line: CARDINAL);
   BEGIN
      IF bfline # 0 THEN
      END;
   END EmitLineNumer;

   PROCEDURE InitEmitCode;
      (* to be called after ScanArgs *)
   BEGIN
      InitOutput;
   END InitEmitCode;

BEGIN
   currentSegment := text; (* default segment of as *)
   bfline := 0;
END MCP4CodeSys.
