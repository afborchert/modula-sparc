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
   $Id: SysLseek.m2,v 0.2 1997/02/28 15:47:47 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysLseek.m2,v $
   Revision 0.2  1997/02/28  15:47:47  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:37  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysLseek;

   FROM Errno IMPORT errno;
   FROM Sys IMPORT lseek;
   FROM SystemTypes IMPORT OFF;
   FROM SYSTEM IMPORT UNIXCALL;

   PROCEDURE Lseek(fd: CARDINAL; offset: OFF;
                   whence: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(lseek, r0, r1, fd, offset, whence) THEN
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Lseek;

   PROCEDURE Tell(fd: CARDINAL; VAR offset: OFF) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(lseek, r0, r1, fd, 0, 1) THEN
         offset := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Tell;

END SysLseek.
