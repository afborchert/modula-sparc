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
   $Id: P.m2,v 0.2 1997/02/28 15:59:59 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: P.m2,v $
   Revision 0.2  1997/02/28  15:59:59  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:45  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE P; (* WS 6/88 *)

   FROM SYSTEM IMPORT ADDRESS, ADR, BYTE;
   FROM StdIO IMPORT stdout;
   FROM Printf IMPORT Printf, FmtExitCode, Default, Pabort, ErrorReaction;
   FROM LongStrings IMPORT Long, Alloc, StrAdr, StrSize, ClearLong,
      Lwrite;
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

   PROCEDURE rintf0(fmt : ARRAY OF CHAR);

   BEGIN 
      Done := Printf(OutputText,0,fmt,x,x,x,x,x,x,x,x);
      Finish(0,fmt);
   END rintf0;

   PROCEDURE rintf1(fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,1,fmt,i1,x,x,x,x,x,x,x);
      Finish(1,fmt);
   END rintf1;

   PROCEDURE rintf2(fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,2,fmt,i1,i2,x,x,x,x,x,x);
      Finish(2,fmt);
   END rintf2;

   PROCEDURE rintf3(fmt : ARRAY OF CHAR; i1,i2,i3 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,3,fmt,i1,i2,i3,x,x,x,x,x);
      Finish(3,fmt);
   END rintf3;

   PROCEDURE rintf4(fmt : ARRAY OF CHAR; i1,i2,i3,i4 : ARRAY OF 
      BYTE );

   BEGIN 
      Done := Printf(OutputText,4,fmt,i1,i2,i3,i4,x,x,x,x);
      Finish(4,fmt);
   END rintf4;

   PROCEDURE rintf5(fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5 : ARRAY OF 
      BYTE);

   BEGIN 
      Done := Printf(OutputText,5,fmt,i1,i2,i3,i4,i5,x,x,x);
      Finish(5,fmt);
   END rintf5;

   PROCEDURE rintf6(fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,6,fmt,i1,i2,i3,i4,i5,i6,x,x);
      Finish(6,fmt);
   END rintf6;

   PROCEDURE rintf7(fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6,i7 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,7,fmt,i1,i2,i3,i4,i5,i6,i7,x);
      Finish(7,fmt);
   END rintf7;

   PROCEDURE rintf8(fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6,i7,i8 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,8,fmt,i1,i2,i3,i4,i5,i6,i7,i8);
      Finish(8,fmt);
   END rintf8;

   PROCEDURE Finish(no : CARDINAL; VAR fmt : ARRAY OF CHAR);

   VAR
      fmt3 : ARRAY[0..255] OF CHAR;
      factor : INTEGER;
      
   BEGIN
      IF (Done = Success) AND NOT Lwrite(OutputText,stdout) THEN
	 Done := CannotWriteStdout;
      END;
      IF Done # Success THEN
	 factor := Pabort;
	 ErrorReaction(Done,ErrorMode,no,factor,fmt3);
	 E.rror2(factor,fmt3,no,fmt);
      END;
      ClearLong(OutputText);
   END Finish;

BEGIN 
   Done := Undefined;
   ErrorMode := Default;
   Alloc(OutputText);
END P. 
