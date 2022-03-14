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
   $Id: ASCII.d,v 0.2 1997/02/28 15:49:44 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: ASCII.d,v $
   Revision 0.2  1997/02/28  15:49:44  borchert
   header fixed

   Revision 0.1  1997/02/21  19:17:55  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE ASCII;

   CONST

      (* control characters *)

      nul = 0C;   ack = 6C;   ff  = 14C;  dc2 = 22C;  can = 30C;  rs  = 36C;  
      soh = 1C;   bel = 7C;   cr  = 15C;  dc3 = 23C;  em  = 31C;  us  = 37C;  
      stx = 2C;   bs  = 10C;  so  = 16C;  dc4 = 24C;  sub = 32C;  sp  = 40C;
      etx = 3C;   ht  = 11C;  si  = 17C;  nak = 25C;  esc = 33C;  
      eot = 4C;   lf  = 12C;  dle = 20C;  syn = 26C;  fs  = 34C;  
      enq = 5C;   vt  = 13C;  dc1 = 21C;  etb = 27C;  gs  = 35C;

      (* other usual names *)

      null = nul;
      bell = bel;
      nl   = lf; (* new line *)
      tab  = ht;
      np   = ff; (* new page *)

      del  = 177C;

END ASCII.
