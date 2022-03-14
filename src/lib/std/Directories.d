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
   $Id: Directories.d,v 0.3 1997/02/28 15:49:58 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Directories.d,v $
   Revision 0.3  1997/02/28  15:49:58  borchert
   header fixed

   Revision 0.2  1997/02/21  19:29:13  borchert
   removal of old copyright

   Revision 0.1  1997/02/21  19:18:00  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Directories;

   FROM SystemTypes IMPORT DirSize, OFF;

   TYPE 
      DIR;

      FileName = ARRAY [0..DirSize-1] OF CHAR;
      Direct = 
         RECORD 
            ino: CARDINAL;
            name: FileName;
         END;

   PROCEDURE OpenDir(VAR dirp: DIR; filename: ARRAY OF CHAR) : BOOLEAN;

   PROCEDURE ReadDir(dirp: DIR; VAR direct: Direct) : BOOLEAN;

   PROCEDURE TellDir(dirp: DIR; VAR offset: OFF) : BOOLEAN;

   PROCEDURE SeekDir(dirp: DIR; offset: OFF) : BOOLEAN;

   PROCEDURE RewindDir(dirp: DIR) : BOOLEAN;

   PROCEDURE CloseDir(VAR dirp: DIR);

END Directories.
