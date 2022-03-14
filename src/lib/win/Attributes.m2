(* Ulm's Modula-2 Library
   Copyright (C) 1984-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm's Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: Attributes.m2,v 0.2 1997/02/28 15:59:14 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Attributes.m2,v $
   Revision 0.2  1997/02/28  15:59:14  borchert
   header fixed

   Revision 0.1  1997/02/21  19:43:14  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Attributes; (* AFB 5/88 *)

   FROM StdIO IMPORT FILE, Fputc;
   FROM TermInfo IMPORT Term, String, TputsDelay, Tparm9;
   FROM Delay IMPORT Delay, InitDelay;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM SYSTEM IMPORT ADR;

   (* (* exported from definition module *)
   TYPE
      Attribute = (standout, underline, reverse, blink, dim, bold, blank,
		   protect, alternate);
      AttributeSet = SET OF Attribute;
   VAR
      Done: BOOLEAN;
   *)
   TYPE
      TermRef = POINTER TO Term;
      AttrInfo = POINTER TO AttrRec;
      AttrRec =
	 RECORD
	    tinfo: TermRef;
	    out: FILE;
	    baudrate: CARDINAL;
	    atset, on: AttributeSet;
	 END;
   VAR
      outfp: FILE;

   PROCEDURE OpenAttrInfo(VAR atinfo: AttrInfo;
			  VAR terminfo: Term;
			  bdrate: CARDINAL;
			  fp: FILE);
      VAR
	 cap: String;
	 at: Attribute;
   BEGIN
      NEW(atinfo);
      WITH atinfo^ DO
	 tinfo := ADR(terminfo);
	 out := fp;
	 baudrate := bdrate;
	 atset := AttributeSet{};
	 on := AttributeSet{};
	 FOR at := MIN(Attribute) TO MAX(Attribute) DO
	    EnterCap(cap, at, tinfo);
	    IF cap[0] <> 0C THEN
	       INCL(atset, at);
	    END;
	 END;
      END;
      Done := TRUE;
   END OpenAttrInfo;

   PROCEDURE CloseAttrInfo(VAR atinfo: AttrInfo);
   BEGIN
      DISPOSE(atinfo);
      Done := TRUE;
   END CloseAttrInfo;

   PROCEDURE AvailableAttributes(atinfo: AttrInfo; VAR atset: AttributeSet);
   BEGIN
      atset := atinfo^.atset;
      Done := TRUE;
   END AvailableAttributes;

   PROCEDURE SetAttribute(atinfo: AttrInfo; at: Attribute);
      VAR cap: String;
   BEGIN
      WITH atinfo^ DO
	 InitOut(atinfo);
	 IF at IN atset THEN
	    EnterCap(cap, at, tinfo);
	    Out(cap);
	 END;
      END;
   END SetAttribute;

   PROCEDURE SetAttributes(atinfo: AttrInfo; attrset: AttributeSet);
      VAR
	 flags: ARRAY [MIN(Attribute)..MAX(Attribute)] OF BOOLEAN;
	 at: Attribute;
	 cap: String;
	 card: CARDINAL; (* cardinality of attrset *)
	 oneat: Attribute; (* if card = 1: this is the only one *)
   BEGIN
      WITH atinfo^ DO
	 InitOut(atinfo); Done := TRUE;
	 WITH tinfo^ DO
	    IF on <> AttributeSet{} THEN
	       ResetAttributes(atinfo);
	    END;
	    IF OK(SetAttributes) THEN
	       card := 0;
	       FOR at := MIN(Attribute) TO MAX(Attribute) DO
		  flags[at] := at IN attrset;
		  IF at IN attrset THEN
		     INC(card);
		     oneat := at;
		  END;
	       END;
	       (* not only optimization but also bug fix of terminfo entries *)
	       IF card = 0 THEN
		  (* ResetAttributes(atinfo); -- already done *)
	       ELSIF card = 1 THEN
		  SetAttribute(atinfo, oneat);
	       ELSE
		  Tparm9(cap, SetAttributes,
			flags[standout],
			flags[underline],
			flags[reverse],
			flags[blink],
			flags[dim],
			flags[bold],
			flags[blank],
			flags[protect],
			flags[alternate]);
		  Out(cap);
	       END;
	    ELSE
	       FOR at := MIN(Attribute) TO MAX(Attribute) DO
		  IF at IN attrset THEN
		     EnterCap(cap, at, tinfo);
		     Out(cap);
		  END;
	       END;
	    END;
	 END;
	 IF Done THEN
	    on := attrset;
	 END;
      END;
   END SetAttributes;

   PROCEDURE ResetAttribute(atinfo: AttrInfo; at: Attribute);
      VAR cap: String;
   BEGIN
      WITH atinfo^ DO
	 InitOut(atinfo);
	 ExitCap(cap, at, tinfo);
	 EXCL(on, at);
	 Out(cap);
      END;
   END ResetAttribute;

   PROCEDURE ResetAttributes(atinfo: AttrInfo);
      VAR
	 at: Attribute;
	 cap: String;
   BEGIN
      WITH atinfo^ DO
	 InitOut(atinfo);
	 WITH tinfo^ DO
	    IF OK(ExitAttributeMode) THEN
	       Out(ExitAttributeMode);
	       (* sometimes (e.g. dap4x) exitattributemode doesn't *)
	       (* switch off the alternate charset mode *)
	       IF alternate IN on THEN
		  ExitCap(cap, alternate, tinfo);
		  Out(cap);
	       END;
	       on := AttributeSet{};
	    ELSE
	       FOR at := MIN(Attribute) TO MAX(Attribute) DO
		  IF at IN on THEN
		     ExitCap(cap, at, tinfo);
		     EXCL(on, at);
		     Out(cap);
		  END;
	       END;
	    END;
	 END;
      END;
   END ResetAttributes;

   (* local procedures *)

   PROCEDURE InitOut(atinfo: AttrInfo);
   BEGIN
      WITH atinfo^ DO
	 outfp := out;
	 WITH tinfo^ DO
	    InitDelay(baudrate, PadChar[0], Write);
	 END;
      END;
   END InitOut;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      Done := Done AND Fputc(ch, outfp);
   END Write;

   PROCEDURE Out(VAR s: ARRAY OF CHAR);
   BEGIN
      Done := TRUE;
      TputsDelay(s, 1, Write, Delay);
   END Out;

   PROCEDURE OK(VAR s: ARRAY OF CHAR) : BOOLEAN;
   BEGIN
      RETURN s[0] <> 0C
   END OK;

   PROCEDURE EnterCap(VAR cap: String; at: Attribute; tref: TermRef);
   BEGIN
      WITH tref^ DO
	 CASE at OF
	 | standout: 	cap := EnterStandoutMode;
         | underline: 	cap := EnterUnderlineMode;
         | reverse: 	cap := EnterReverseMode;
         | blink: 	cap := EnterBlinkMode;
         | dim: 	cap := EnterDimMode;
         | bold: 	cap := EnterBoldMode;
         | blank: 	cap := EnterSecureMode;
         | protect: 	cap := EnterProtectedMode;
         | alternate:	cap := EnterAltCharsetMode;
	 END;
      END;
   END EnterCap;

   PROCEDURE ExitCap(VAR cap: String; at: Attribute; tref: TermRef);
   BEGIN
      WITH tref^ DO
	 CASE at OF
	 | standout: 	cap := ExitStandoutMode;
         | underline: 	cap := ExitUnderlineMode;
         | reverse: 	cap := ExitAttributeMode;
         | blink: 	cap := ExitAttributeMode;
         | dim: 	cap := ExitAttributeMode;
         | bold: 	cap := ExitAttributeMode;
         | blank: 	cap := ExitAttributeMode;
         | protect: 	cap := ExitAttributeMode;
         | alternate:	cap := ExitAltCharsetMode;
	 END;
      END;
   END ExitCap;

END Attributes.
