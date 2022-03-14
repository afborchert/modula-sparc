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
   $Id: Memory.m2,v 0.1 1997/02/21 18:40:38 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Memory.m2,v $
   Revision 0.1  1997/02/21  18:40:38  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Memory;		(* Martin Hasch, Jan 1989 *)
(*
 *	dynamic core space management with error handling
 *)

   FROM SYSTEM IMPORT ADDRESS;
   FROM MCFatal IMPORT Fatal;
   FROM MCBase IMPORT BitsPerWord;
   IMPORT Storage;
   FROM SysLocations IMPORT Break;

   VAR
      minadr,
      maxadr:	CARDINAL;
      modulus:	BITSET;
      minbrk,
      maxbrk:	CARDINAL;

   PROCEDURE ALLOCATE(VAR adr: ADDRESS; siz: CARDINAL);
   BEGIN
      Storage.Setmode(2);
      Storage.ALLOCATE(adr,siz);
      Storage.Setmode(1);
      IF adr = NIL THEN
	 Fatal("core space overflow")
      END;
      IF CARDINAL(adr) < minadr THEN
	 minadr := adr;
      END;
      IF CARDINAL(adr) > maxadr THEN
	 maxadr := adr;
      END;
      INCL(modulus, CARDINAL(adr) MOD BitsPerWord);
      IF CARDINAL(Break) > maxbrk THEN
	 maxbrk := Break;
      END;
   END ALLOCATE;

   PROCEDURE DEALLOCATE(VAR adr: ADDRESS; siz: CARDINAL);
   BEGIN
      Storage.DEALLOCATE(adr, siz);
   END DEALLOCATE;

   PROCEDURE CheckPtr(p: ADDRESS): BOOLEAN;
      (* returns FALSE if p can obviously not be a result of ALLOCATE *)
      (* this test can be used to avoid memory fault while processing	 *)
      (* error messages containing possibly uninitialized pointers, e.g. *)
   BEGIN
      RETURN
	 (minadr <= CARDINAL(p)) & (CARDINAL(p) <= maxadr) &
	 (CARDINAL(p) MOD BitsPerWord IN modulus)
   END CheckPtr;

   PROCEDURE MaxSpace(): CARDINAL;
      (* returns maximum size of dynamically allocated core space area *)
   BEGIN
      RETURN maxbrk - minbrk
   END MaxSpace;

BEGIN
   Storage.Setmode(1);
   minadr := MAX(CARDINAL);
   maxadr := 0;
   modulus := {};
   minbrk := Break;
   maxbrk := minbrk;
END Memory.
