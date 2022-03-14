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
   $Id: Storage.m2,v 0.2 1997/02/28 15:50:36 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Storage.m2,v $
   Revision 0.2  1997/02/28  15:50:36  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:36  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Storage;

   FROM SYSTEM IMPORT WORD, ADDRESS, ADR, TSIZE;
   FROM SysBreak IMPORT Sbreak;
   FROM SysPanic IMPORT Panic;

   (* see "The C Programming Language", Page 173 *)

   TYPE
      FreePtr = POINTER TO FreeNode;
      FreeNode =
         RECORD
            size: CARDINAL; (* in units *)
	    CASE : BOOLEAN OF
              TRUE:
		 next: FreePtr;
	    | FALSE:
		 cnext: CARDINAL;
            END;
         END;
   VAR
      FreeList: FreePtr; (* circular ordered list *)
      base: FreeNode;
      ign: BOOLEAN;
      Mode: (returnNIL, abort);

   (* ask system for memory *)

   PROCEDURE MoreCore(nu: CARDINAL) : FreePtr;
      CONST Nalloc = 1024; (* #units to allocate at once *)
      VAR rnu: CARDINAL; (* rounded number of units *)
	  adr: ADDRESS;
	  fp: FreePtr;
   BEGIN
      INC(nu); (* allocate an additional unit to support 8-byte-accesses
		  even on the last 4 bytes of an allocated area *)
      rnu := Nalloc * ((nu+Nalloc-1) DIV Nalloc);
      adr := Sbreak(stob(rnu));
      IF adr = NIL THEN
	 adr := Sbreak(stob(nu));
	 IF adr = NIL THEN
	    RETURN NIL;
	 ELSE
	    rnu := nu;
	 END;
      END;
      fp := FreePtr(adr);
      (* make the new allocated area available with the exception of
	 the last 8 bytes
      *)
      DEALLOCATE(fp, (* size in bytes = *) (rnu-1) * TSIZE(FreeNode));
      RETURN FreeList;
   END MoreCore;

   (* bytes to units *)

   PROCEDURE btou(nb: CARDINAL) : CARDINAL;
   BEGIN
      nb := TSIZE(FreeNode) * ((nb+TSIZE(FreeNode)-1) DIV TSIZE(FreeNode));
      RETURN nb DIV TSIZE(FreeNode);
   END btou;

   (* size to bytecount *)

   PROCEDURE stob(size: CARDINAL) : CARDINAL;
   BEGIN
      RETURN size * TSIZE(FreeNode);
   END stob;

   PROCEDURE DEALLOCATE(VAR ptr: ADDRESS; size: CARDINAL);
      (* size in words *)
      VAR free: FreePtr;
	  cfree: CARDINAL;
	  cptr: CARDINAL;
	  fptr: FreePtr;
   BEGIN
      size := btou(size); (* now size in units *)

      cptr := CARDINAL(ptr);
      free := FreeList;
      LOOP
	 cfree := CARDINAL(free);
	 IF (cptr > cfree) AND (cptr < free^.cnext) THEN
	    EXIT
	 END;
	 IF (cfree >= free^.cnext) AND
	    ((cptr > cfree) OR (cptr < free^.cnext)) THEN
	    EXIT (* at one end or other *)
	 END;
	 free := free^.next;
      END; (* LOOP *)

      fptr := FreePtr(ptr);
      fptr^.size := size;
      IF cptr + stob(fptr^.size) = free^.cnext THEN (* join to upper nbr *)
	 fptr^.size := size + free^.next^.size;
	 fptr^.next := free^.next^.next;
      ELSE
	 fptr^.next := free^.next;
      END;
      IF cfree + stob(free^.size) = cptr THEN (* join to lower nbr *)
	 free^.size := free^.size + fptr^.size;
	 free^.next := fptr^.next;
      ELSE
	 free^.next := fptr;
      END;
      FreeList := free;

      ptr := NIL;
   END DEALLOCATE;

   PROCEDURE ALLOCATE(VAR ptr: ADDRESS; size: CARDINAL);
      VAR free: FreePtr;
          dummy: ADDRESS;
          old: FreePtr;
   BEGIN
      size := btou(size); (* now size in units *)
      old := FreeList;
      free := FreeList^.next;
      LOOP
         IF free^.size >= size THEN

	    (* free block found *)

	    ptr := free;
            IF free^.size > size THEN
	       free^.size := free^.size - size;
	       INC(ptr, stob(free^.size));
	    ELSE
               IF old^.next = FreeList THEN
                  FreeList := old;
               END;
     	       old^.next := free^.next;
	    END;
            RETURN;
         END;
         old := free;
	 free := free^.next;
	 IF free = FreeList^.next THEN
	    free := MoreCore(size);
	    IF free = NIL THEN
	       EXIT
	    END;
	 END;
      END; (* LOOP *)

      IF Mode = returnNIL THEN
         ptr := NIL;
      ELSE
         Panic("No space available.");
      END;
   END ALLOCATE;

   PROCEDURE Setmode(m: CARDINAL);
   BEGIN
      CASE m OF
        1: Mode := abort;
      | 2: Mode := returnNIL;
      ELSE
         (* nothing *)
      END;
   END Setmode;

BEGIN
   FreeList := ADR(base);
   base.next := FreeList;
   base.size := 0;
   Mode := abort;
END Storage.
