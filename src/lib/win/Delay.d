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
   $Id: Delay.d,v 0.2 1997/02/28 15:59:15 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Delay.d,v $
   Revision 0.2  1997/02/28  15:59:15  borchert
   header fixed

   Revision 0.1  1997/02/21  19:43:10  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Delay; (* AFB 6/88 *)

   FROM StdIO IMPORT FILE;

   TYPE
      OutProc = PROCEDURE (CHAR);

   PROCEDURE InitDelay(baudrate: CARDINAL; padch: CHAR; outc: OutProc);

   PROCEDURE InitDelayFile(baudrate: CARDINAL; padch: CHAR; fp: FILE);

   PROCEDURE Delay(tenthofmillisecs: CARDINAL);

END Delay.
