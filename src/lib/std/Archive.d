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
   $Id: Archive.d,v 0.2 1997/02/28 15:49:46 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Archive.d,v $
   Revision 0.2  1997/02/28  15:49:46  borchert
   header fixed

   Revision 0.1  1997/02/21  19:17:56  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Archive; (* AFB 3/84 *)

   FROM SystemTypes IMPORT TIME, OFF;

   (* routines for reading an archive file *)

   CONST NameLength = 14;

   TYPE AFILE; (* hidden *)
      FileName = ARRAY[0..NameLength-1] OF CHAR;
      AStat =
         RECORD
            name: FileName;
            uid, gid: CARDINAL;
            date: TIME;
            size: OFF;
            mode: BITSET;
	    offset: OFF; (* absolute offset in archive file *)
         END;

   PROCEDURE ArchiveOpen(VAR a: AFILE; archive: ARRAY OF CHAR;
			 filename: ARRAY OF CHAR) : BOOLEAN;

   PROCEDURE ArchiveReopen(a: AFILE; filename: ARRAY OF CHAR) : BOOLEAN;
   (* in case of an error "a" will be closed *)

   PROCEDURE ArchiveClose(a: AFILE);

   PROCEDURE ArchiveRead(a: AFILE; VAR ch: CHAR) : BOOLEAN;

   PROCEDURE ArchiveStat(a: AFILE; VAR buf: AStat);

END Archive.
