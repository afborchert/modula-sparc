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
   $Id: Menus.d,v 0.2 1997/02/28 15:59:22 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Menus.d,v $
   Revision 0.2  1997/02/28  15:59:22  borchert
   header fixed

   Revision 0.1  1997/02/21  19:43:12  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Menus; (* AFB 6/88 *)

   FROM Windows IMPORT Window;

   TYPE
      Menu;
   VAR
      Done: BOOLEAN;

   PROCEDURE CreateMenu(VAR menu: Menu;
			title: ARRAY OF CHAR);

   PROCEDURE AddCommand(menu: Menu;
			cmd: ARRAY OF CHAR;
			cmdno: CARDINAL);

   PROCEDURE AddProcedure(menu: Menu;
			  cmd: ARRAY OF CHAR;
			  cmdproc: PROC);

   PROCEDURE AddSubMenu(menu: Menu;
			cmd: ARRAY OF CHAR;
			submenu: Menu);

   PROCEDURE ExecMenu(menu: Menu; background: Window;
		      VAR selected: CARDINAL);

   PROCEDURE DisposeMenu(VAR menu: Menu);

END Menus.
