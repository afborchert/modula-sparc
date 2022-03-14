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
   $Id: Stack.m2,v 0.2 1997/02/28 16:00:11 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Stack.m2,v $
   Revision 0.2  1997/02/28  16:00:11  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:47  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Stack;

   FROM SYSTEM IMPORT BYTE, ADDRESS, ADR, WORD;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM Bytes IMPORT ByteNCopy;
   
   TYPE 
      StackType = RECORD
	 maxno : CARDINAL; 
	 top   : CARDINAL;
	 elemsz: CARDINAL;
	 align : CARDINAL;
	 add   : ADDRESS;
      END;
      Stack = POINTER TO StackType;
	 
   PROCEDURE Create(VAR s : Stack; noelem : CARDINAL; elem : ARRAY OF BYTE);

   BEGIN
      NEW(s);
      WITH s^ DO
	 top := 0;
	 maxno := noelem;
	 elemsz := HIGH(elem)+1;
	 align := elemsz DIV SIZE(WORD) * SIZE(WORD);
	 IF align # elemsz THEN
	    INC(align,SIZE(WORD));
	 END;
	 ALLOCATE(add,align*maxno);
      END;
   END Create;

   PROCEDURE Dispose (VAR s : Stack);

   BEGIN
      IF s # NIL THEN
	 WITH s^ DO
	    DEALLOCATE(add,align*maxno);
	 END;
	 DISPOSE(s);
	 s := NIL;
      END;
   END Dispose;

   PROCEDURE Clear(s : Stack);

   BEGIN
      IF s # NIL THEN
	 s^.top := 0;
      END;
   END Clear;

   PROCEDURE Elems(s : Stack) : CARDINAL;

   BEGIN
      IF s # NIL THEN
	 RETURN s^.top;
      ELSE
	 RETURN 0;
      END;
   END Elems;

   PROCEDURE Push(s : Stack; elem : ARRAY OF BYTE) : BOOLEAN;

   BEGIN
      IF (s = NIL) OR (HIGH(elem)+1 # s^.elemsz) OR (s^.top = s^.maxno) THEN
	 RETURN FALSE;
      ELSE
	 WITH s^ DO
	    ByteNCopy(add+top*align,ADR(elem),elemsz);
	    INC(top);
	 END;
	 RETURN TRUE;
      END;
   END Push;

   PROCEDURE Pop(s : Stack; VAR elem : ARRAY OF BYTE) : BOOLEAN;

   BEGIN
      IF (s # NIL) AND (HIGH(elem)+1 = s^.elemsz) AND Top(s,0,elem) THEN
	 DEC(s^.top);
	 RETURN TRUE;
      ELSE
	 RETURN FALSE;
      END;
   END Pop;

   PROCEDURE Adr(s : Stack; pos : CARDINAL; VAR a : ADDRESS) : BOOLEAN;

   BEGIN
      IF (s = NIL) OR (s^.top <= pos) THEN
	 a := NIL;
	 RETURN FALSE;
      ELSE
	 WITH s^ DO
	    a := add +(top-1-pos)*align;
	 END;
	 RETURN TRUE;
      END;
   END Adr;

   PROCEDURE Top(s : Stack; pos : CARDINAL; VAR elem : ARRAY OF BYTE) : BOOLEAN;

   VAR
      a : ADDRESS;

   BEGIN
      IF (s # NIL) AND (s^.elemsz = HIGH(elem)+1) AND Adr(s,pos,a) THEN
	 ByteNCopy(ADR(elem),a,s^.elemsz);
	 RETURN TRUE;
      ELSE
         RETURN FALSE;
      END;
   END Top;

END Stack.
