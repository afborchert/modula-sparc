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
   $Id: RandomGener.d,v 0.2 1997/02/28 15:50:24 borchert Exp borchert $
   ----------------------------------------------------------------------------
   $Log: RandomGener.d,v $
   Revision 0.2  1997/02/28  15:50:24  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:09  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE RandomGenerator;

   (*    Anyone who considers arithmetical
	methods of producing random digits
	 is, of course, in a state of sin.
		 - John von Neumann (1951)
   *)

   PROCEDURE IntVal() : INTEGER;
      (* get random 32-bit value *)

   PROCEDURE RealVal() : REAL;
      (* get a uniformly distributed real value in [0..1) *)

   PROCEDURE Random(low, high: INTEGER) : INTEGER;
      (* get a uniformly distributed integer in [low..high] *)

   PROCEDURE Flip() : BOOLEAN;
      (* return TRUE or FALSE *)

   PROCEDURE Init(seed: INTEGER);

END RandomGenerator. 
