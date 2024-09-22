(* Ulm Modula-2 Library
   Copyright (C) 1984-2024 by Andreas F. Borchert
   ----------------------------------------------------------------------------
   Ulm Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@andreas-borchert.de
   ----------------------------------------------------------------------------
*)

DEFINITION MODULE SysFcntl;

   FROM SYSTEM IMPORT WORD;

   TYPE
      (* for Solaris 11, see /usr/include/sys/fcntl.h, incomplete *)
      FcntlRequest = (
	 dupfd, (* 0 *)
	 getfd, (* 1 *)
	 setfd, (* 2 *)
	 getfl, (* 3 *)
	 setfl, (* 4 *)
	 fcntl5, fcntl6, fcntl7, fcntl8,
	 dup2fd (* 9 *)
      );

   PROCEDURE Fcntl(fd: CARDINAL; cmd: FcntlRequest; VAR arg: WORD) : BOOLEAN;

END SysFcntl.
