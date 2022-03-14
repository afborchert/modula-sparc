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
   $Id: Screen.d,v 0.2 1997/02/28 15:59:25 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Screen.d,v $
   Revision 0.2  1997/02/28  15:59:25  borchert
   header fixed

   Revision 0.1  1997/02/21  19:43:13  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Screen; (* AFB 5/88 *)

   FROM TermInfo IMPORT Term;
   FROM StdIO IMPORT FILE;

   TYPE
      Screen;
      CursorVisibility = (normal, invisible, morevisible);
   VAR
      Done: BOOLEAN;

   PROCEDURE OpenScreen(VAR scr: Screen;
			outfp: FILE;
			baudrate: CARDINAL;
			lines, columns: CARDINAL;
			terminal: ARRAY OF CHAR) : BOOLEAN;

   PROCEDURE OpenScreenTI(VAR scr: Screen;
			  outfp: FILE;
			  baudrate: CARDINAL;
			  lines, columns: CARDINAL;
			  tinfo: Term);

   PROCEDURE CloseScreen(VAR scr: Screen);

   PROCEDURE Lines(scr: Screen) : CARDINAL;

   PROCEDURE Columns(scr: Screen) : CARDINAL;

   PROCEDURE ClearScreen(scr: Screen);

   PROCEDURE SetCursor(scr: Screen; line, column: CARDINAL);
      (* line in [0..Lines(scr)-1] and column in [0..Columns(scr)-1] *)

   PROCEDURE MoveCursor(scr: Screen;
	     (* from *) line1, column1,
	     (* to *)   line2, column2: CARDINAL);

   PROCEDURE InitScreen(scr: Screen; lines, columns: CARDINAL);

   PROCEDURE Scroll(scr: Screen; down: BOOLEAN;
		    line, column: CARDINAL; (* upper left corner *)
		    lines, columns: CARDINAL);
      (* scroll part of screen (downward or upward);
	 down: TRUE (downwards) or FALSE (upwards)
	 cursor position is undefined (if Done is set to TRUE)
	 Done set to FALSE if not supported by terminal/terminfo
      *)

   PROCEDURE ResetScrollRegions(scr: Screen);

   PROCEDURE SetCursorVisibility(scr: Screen; visibility: CursorVisibility);

END Screen.
