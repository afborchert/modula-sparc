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
   $Id: EtcGroup.d,v 0.2 1997/02/28 15:50:01 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: EtcGroup.d,v $
   Revision 0.2  1997/02/28  15:50:01  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:01  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE EtcGroup;
(*
 *	scanning and searching the etc/group file
 *
 *	Martin Hasch, University of Ulm, Dec-06-1988
 *)

   TYPE
      MemberList = POINTER TO Member;
      Member =
	 RECORD
	    logname:  ARRAY [0..7] OF CHAR;
	    nextmem:  MemberList;
	 END;

      Grent =
	 RECORD
	    grname:   ARRAY [0..7] OF CHAR;
	    password: ARRAY [0..15] OF CHAR;
	    gid:      CARDINAL;
	    members:  MemberList;		(* NIL-terminated *)
	 END;

   PROCEDURE OpenGr(filename: ARRAY OF CHAR): BOOLEAN;
   (* returns TRUE on success *)

   PROCEDURE GetGrent(VAR grent: Grent): BOOLEAN;

   PROCEDURE GetGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;

   PROCEDURE GetGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;

   PROCEDURE ReopenGr(): BOOLEAN;
   (* returns TRUE if group file is open and seekable *)

   PROCEDURE CloseGr(): BOOLEAN;
   (* returns TRUE if group file was open *)


   PROCEDURE FetchGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;
   (* implies OpenGr("/etc/group"), and CloseGr() *)

   PROCEDURE FetchGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;
   (* implies OpenGr("/etc/group"), and CloseGr() *)

END EtcGroup.
