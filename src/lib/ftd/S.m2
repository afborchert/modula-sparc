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
   $Id: S.m2,v 0.2 1997/02/28 16:00:08 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: S.m2,v $
   Revision 0.2  1997/02/28  16:00:08  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:46  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE S;

   FROM SYSTEM IMPORT ADDRESS, ADR, BYTE;
   FROM Printf IMPORT Printf, FmtExitCode, Default, Sabort, ErrorReaction; 
   FROM LongStrings IMPORT Long, Alloc, StrAdr, StrSize, ClearLong;
   FROM Bytes IMPORT ByteNCopy;
   IMPORT E;

   VAR 
      ErrorMode : BITSET;
      Done      : FmtExitCode;
      OutputText: Long;
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

   PROCEDURE printf0(VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR);

   BEGIN 
      Done := Printf(OutputText,0,fmt,x,x,x,x,x,x,x,x);
      Finish(0,s,fmt);
   END printf0;

   PROCEDURE printf1(VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,1,fmt,i1,x,x,x,x,x,x,x);
      Finish(1,s,fmt);
   END printf1;

   PROCEDURE printf2(VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1,i2 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,2,fmt,i1,i2,x,x,x,x,x,x);
      Finish(2,s,fmt);
   END printf2;

   PROCEDURE printf3(VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1,i2,i3 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,3,fmt,i1,i2,i3,x,x,x,x,x);
      Finish(3,s,fmt);
   END printf3;

   PROCEDURE printf4(VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1,i2,i3,i4 
      : ARRAY OF BYTE );

   BEGIN 
      Done := Printf(OutputText,4,fmt,i1,i2,i3,i4,x,x,x,x);
      Finish(4,s,fmt);
   END printf4;

   PROCEDURE printf5(VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1,i2,i3,i4,
      i5 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,5,fmt,i1,i2,i3,i4,i5,x,x,x);
      Finish(5,s,fmt);
   END printf5;

   PROCEDURE printf6(VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1,i2,i3,i4,
      i5,i6 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,6,fmt,i1,i2,i3,i4,i5,i6,x,x);
      Finish(6,s,fmt);
   END printf6;

   PROCEDURE printf7(VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1,i2,i3,i4,
      i5,i6,i7 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,7,fmt,i1,i2,i3,i4,i5,i6,i7,x);
      Finish(7,s,fmt);
   END printf7;

   PROCEDURE printf8(VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1,i2,i3,i4,
      i5,i6,i7,i8 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,8,fmt,i1,i2,i3,i4,i5,i6,i7,i8);
      Finish(8,s,fmt);
   END printf8;

   PROCEDURE Finish(no : CARDINAL; VAR s : ARRAY OF CHAR; VAR fmt : ARRAY OF 
      CHAR);

      VAR 
	 add  : ADDRESS;
         size : CARDINAL;
	 fmt3 : ARRAY[0..255] OF CHAR;
	 factor : INTEGER;

   BEGIN 
      IF Done = Success THEN 
         size := StrSize(OutputText);
	 IF size > (HIGH(s)+1) THEN
	    Done := StringTooSmall;
	 ELSE
	    IF size < (HIGH(s)+1) THEN (* copy null *)
	       INC(size);
	    END;
	    ByteNCopy(ADR(s),StrAdr(OutputText),size);
         END;
      END;
      IF Done # Success THEN 
	 factor := Sabort;
         ErrorReaction(Done,ErrorMode,no,factor,fmt3);
	 E.rror2(factor,fmt3,no,fmt);
      END;
      ClearLong(OutputText);
   END Finish;

BEGIN 
   Done := Undefined;
   ErrorMode := Default;
   Alloc(OutputText);
END S. 
