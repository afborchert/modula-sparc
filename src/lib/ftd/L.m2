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
   $Id: L.m2,v 0.2 1997/02/28 15:59:52 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: L.m2,v $
   Revision 0.2  1997/02/28  15:59:52  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:43  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE L; (* WS 6/88 *)

   FROM SYSTEM IMPORT ADDRESS, ADR, BYTE;
   FROM Printf IMPORT Printf, FmtExitCode, Default, Labort, ErrorReaction;
   FROM LongStrings IMPORT Long, StrSize, CutLong;
   IMPORT E;

   VAR 
      ErrorMode : BITSET;
      Done      : FmtExitCode;
      size      : CARDINAL;
      x         : CHAR;

   PROCEDURE setmode (mode : BITSET);

   BEGIN 
      ErrorMode := mode;
   END setmode;

   PROCEDURE getmode (VAR mode : BITSET);

   BEGIN 
      mode := ErrorMode;
   END getmode;

   PROCEDURE success() : FmtExitCode;

   BEGIN 
      RETURN Done;
   END success;

   PROCEDURE done() : BOOLEAN;

   BEGIN 
      RETURN Done = Success;
   END done;

   PROCEDURE Finish(long : Long; no : CARDINAL; VAR fmt : ARRAY OF CHAR);

   VAR
      fmt3 : ARRAY[0..255] OF CHAR;
      factor : INTEGER;

   BEGIN
      factor := Labort ;
      ErrorReaction(Done,ErrorMode,no,factor,fmt3);
      CutLong(long,size);
      E.rror2(factor,fmt3,no,fmt);
   END Finish;

   PROCEDURE printf0(long : Long; fmt : ARRAY OF CHAR);

   BEGIN 
      size := StrSize(long);
      Done := Printf(long,0,fmt,x,x,x,x,x,x,x,x);
      IF Done # Success THEN 
         Finish(long,0,fmt);
      END 
   END printf0;

   PROCEDURE printf1(long : Long; fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   BEGIN 
      size := StrSize(long);
      Done := Printf(long,1,fmt,i1,x,x,x,x,x,x,x);
      IF Done # Success THEN 
         Finish(long,1,fmt);
      END 
   END printf1;

   PROCEDURE printf2(long : Long; fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(long,2,fmt,i1,i2,x,x,x,x,x,x);
      IF Done # Success THEN 
         Finish(long,2,fmt);
      END 
   END printf2;

   PROCEDURE printf3(long : Long; fmt : ARRAY OF CHAR; i1,i2,i3 : ARRAY OF 
      BYTE);

   BEGIN 
      size := StrSize(long);
      Done := Printf(long,3,fmt,i1,i2,i3,x,x,x,x,x);
      IF Done # Success THEN 
         Finish(long,3,fmt);
      END 
   END printf3;

   PROCEDURE printf4(long : Long; fmt : ARRAY OF CHAR; i1,i2,i3,i4 : ARRAY OF 
      BYTE );

   BEGIN 
      size := StrSize(long);
      Done := Printf(long,4,fmt,i1,i2,i3,i4,x,x,x,x);
      IF Done # Success THEN 
         Finish(long,4,fmt);
      END 
   END printf4;

   PROCEDURE printf5(long : Long; fmt : ARRAY OF CHAR; i1,i2,i3,i4, i5 : 
      ARRAY OF BYTE);

   BEGIN 
      size := StrSize(long);
      Done := Printf(long,5,fmt,i1,i2,i3,i4,i5,x,x,x);
      IF Done # Success THEN 
         Finish(long,5,fmt);
      END 
   END printf5;

   PROCEDURE printf6(long : Long; fmt : ARRAY OF CHAR; i1,i2,i3,i4, i5,i6 : 
      ARRAY OF BYTE);

   BEGIN 
      size := StrSize(long);
      Done := Printf(long,6,fmt,i1,i2,i3,i4,i5,i6,x,x);
      IF Done # Success THEN 
         Finish(long,6,fmt);
      END 
   END printf6;

   PROCEDURE printf7(long : Long; fmt : ARRAY OF CHAR; i1,i2,i3,i4, i5,i6,i7 
      : ARRAY OF BYTE);

   BEGIN 
      size := StrSize(long);
      Done := Printf(long,7,fmt,i1,i2,i3,i4,i5,i6,i7,x);
      IF Done # Success THEN 
         Finish(long,7,fmt);
      END 
   END printf7;

   PROCEDURE printf8(long : Long; fmt : ARRAY OF CHAR; i1,i2,i3,i4, i5,i6,i7,
      i8 : ARRAY OF BYTE);

   BEGIN 
      size := StrSize(long);
      Done := Printf(long,8,fmt,i1,i2,i3,i4,i5,i6,i7,i8);
      IF Done # Success THEN 
         Finish(long,8,fmt);
      END 
   END printf8;

BEGIN 
   Done := Undefined;
   ErrorMode := Default;
END L. 
