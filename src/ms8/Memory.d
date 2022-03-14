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
   $Id: Memory.d,v 0.1 1997/02/21 18:40:20 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Memory.d,v $
   Revision 0.1  1997/02/21  18:40:20  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Memory;		(* Martin Hasch, Jan 1989 *)
(*
 *	dynamic core space management with error handling
 *)

   FROM SYSTEM IMPORT ADDRESS;

   PROCEDURE ALLOCATE(VAR adr: ADDRESS; siz: CARDINAL);
      (* always successful *)

   PROCEDURE DEALLOCATE(VAR adr: ADDRESS; siz: CARDINAL);

   PROCEDURE CheckPtr(p: ADDRESS): BOOLEAN;
      (* returns FALSE if p can obviously not be a result of ALLOCATE *)
      (* this test can be used to avoid memory fault while processing	 *)
      (* error messages containing possibly uninitialized pointers, e.g. *)

   PROCEDURE MaxSpace(): CARDINAL;
      (* returns maximum size of dynamically allocated core space area *)

END Memory.
