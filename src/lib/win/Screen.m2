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
   $Id: Screen.m2,v 0.2 1997/02/28 15:59:27 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Screen.m2,v $
   Revision 0.2  1997/02/28  15:59:27  borchert
   header fixed

   Revision 0.1  1997/02/21  19:43:18  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Screen; (* AFB 5/88 *)

   FROM CallShell IMPORT Shell;
   FROM Environment IMPORT GetEnv;
   FROM StdIO IMPORT FILE, stdout, Fputc, Fopen, Fclose, Fgetc, read, FileNo;
   FROM SysTermIO IMPORT GetWinsize, Winsize;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM TermInfo IMPORT Term, SetupTerm, TputsDelay, Tparm1, Tparm2, Tparm4;
   FROM Delay IMPORT InitDelay, Delay;

   (* (* exported from definition module *)
   TYPE
      CursorVisibility = (normal, invisible, morevisible);
   VAR
      Done: BOOLEAN;
   *)
   VAR
      outfp: FILE;	(* temporarily used for Write *)

   TYPE
      Screen = POINTER TO ScreenRec;
      ScreenRec =
	 RECORD
	    fp: FILE;
	    tinfo: Term;
	    baudrate: CARDINAL;
	    lines, columns: CARDINAL;
	 END;

   PROCEDURE OpenScreen(VAR scr: Screen;
			outfp: FILE;
			bdrate: CARDINAL;
			lines, columns: CARDINAL;
			terminal: ARRAY OF CHAR) : BOOLEAN;
   BEGIN
      NEW(scr);
      WITH scr^ DO
	 fp := outfp;
	 baudrate := bdrate;
	 IF NOT SetupTerm(terminal, tinfo) THEN
	    DISPOSE(scr); RETURN FALSE
	 END;
	 InitScreen(scr, lines, columns);
      END;
      RETURN TRUE
   END OpenScreen;

   PROCEDURE OpenScreenTI(VAR scr: Screen;
			  outfp: FILE;
			  bdrate: CARDINAL;
			  lines, columns: CARDINAL;
			  terminfo: Term);
   BEGIN
      NEW(scr);
      WITH scr^ DO
	 fp := outfp;
	 baudrate := bdrate;
	 tinfo := terminfo;
	 InitScreen(scr, lines, columns);
      END;
   END OpenScreenTI;

   PROCEDURE CloseScreen(VAR scr: Screen);
   BEGIN
      IF scr = NIL THEN Done := FALSE; RETURN END;
      DISPOSE(scr);
      scr := NIL;
   END CloseScreen;

   PROCEDURE Lines(scr: Screen) : CARDINAL;
   BEGIN
      IF scr = NIL THEN Done := FALSE; RETURN 0 END; Done := TRUE;
      RETURN scr^.lines
   END Lines;

   PROCEDURE Columns(scr: Screen) : CARDINAL;
   BEGIN
      IF scr = NIL THEN Done := FALSE; RETURN 0 END; Done := TRUE;
      RETURN scr^.columns
   END Columns;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      Done := Done AND Fputc(ch, outfp);
   END Write;

   PROCEDURE Out(VAR s: ARRAY OF CHAR);
   BEGIN
      TputsDelay(s, 1, Write, Delay);
   END Out;

   PROCEDURE OK(VAR s: ARRAY OF CHAR) : BOOLEAN;
   BEGIN
      RETURN s[0] <> 0C
   END OK;

   PROCEDURE InitOut(scr: Screen);
   BEGIN
      WITH scr^ DO
	 outfp := fp;
	 WITH tinfo DO
	    InitDelay(baudrate, PadChar[0], Write);
	 END;
      END;
   END InitOut;

   PROCEDURE ClearScreen(scr: Screen);
      VAR
	 line: CARDINAL;
   BEGIN
      IF scr = NIL THEN Done := FALSE; RETURN END; Done := TRUE;
      WITH scr^ DO
	 Done := TRUE;
	 InitOut(scr);
	 WITH tinfo DO
	    IF OK(ClearScreen) THEN
	       Out(ClearScreen);
	    ELSIF OK(CursorHome) AND OK(ClrEos) THEN
	       Out(CursorHome); Out(ClrEos);
	    ELSIF OK(ClrEos) THEN
	       SetCursor(scr, 0, 0); Out(ClrEos);
	    ELSIF OK(ClrEol) THEN
	       FOR line := 0 TO lines-1 DO
		  SetCursor(scr, line, 0); Out(ClrEol);
	       END;
	    ELSE
	       Done := FALSE;
	    END;
	 END;
      END;
   END ClearScreen;

   PROCEDURE SetCursor(scr: Screen; line, column: CARDINAL);
      (* line in [0..Lines(scr)-1] and column in [0..Columns(scr)-1] *)
      VAR
	 out: ARRAY [0..79] OF CHAR;
	 l, c: CARDINAL;
   BEGIN
      IF scr = NIL THEN Done := FALSE; RETURN END; Done := TRUE;
      WITH scr^ DO
	 IF (line >= lines) OR (column >= columns) THEN
	    Done := FALSE; RETURN
	 END;
	 InitOut(scr);
	 WITH tinfo DO
	    IF (line = 0) AND (column = 0) AND OK(CursorHome) THEN
	       Out(CursorHome);
	    ELSIF OK(CursorAddress) THEN
	       Tparm2(out, CursorAddress, line, column);
	       Out(out);
	    ELSIF OK(CursorHome) AND OK(ParmRightCursor) AND
		  OK(ParmDownCursor) THEN
	       Out(CursorHome);
	       Tparm1(out, ParmDownCursor, line); Out(out);
	       Tparm1(out, ParmRightCursor, column); Out(out);
	    ELSIF OK(CursorHome) AND OK(CursorRight) AND OK(CursorDown) THEN
	       Out(CursorHome);
	       FOR l := 1 TO line DO Out(CursorDown) END;
	       FOR c := 1 TO column DO Out(CursorRight) END;
	    ELSE
	       Done := FALSE; RETURN
	    END;
	 END;
      END;
   END SetCursor;

   PROCEDURE MoveCursor(scr: Screen;
	     (* from *) line1, column1,
	     (* to *)   line2, column2: CARDINAL);

      VAR
	 out: ARRAY [0..79] OF CHAR;

      PROCEDURE Repeat(ntimes: CARDINAL; VAR cap: ARRAY OF CHAR);
	 VAR i: CARDINAL;
      BEGIN
	 FOR i := 1 TO ntimes DO
	    Out(cap);
	 END;
      END Repeat;

      PROCEDURE Diff(c1, c2: INTEGER) : CARDINAL;
      BEGIN
	 RETURN ABS(c1 - c2)
      END Diff;

   BEGIN
      IF scr = NIL THEN Done := FALSE; RETURN END; Done := TRUE;
      WITH scr^ DO
	 InitOut(scr);
	 IF (line2 >= lines) OR (column2 >= columns) THEN
	    Done := FALSE; RETURN
	 END;
	 IF (line1 = line2) AND (column1 = column2) THEN
	    RETURN
	 END;
	 WITH tinfo DO
	    (* special cases *)
	    IF (column2 = 0) AND (line2 = line1+1) AND OK(Newline) THEN
	       Out(Newline); RETURN
	    END;
	    IF (line2 = 0) AND (column2 = 0) AND OK(CursorHome) THEN
	       Out(CursorHome); RETURN
	    END;
	    (* avoid bad cases *)
	    IF Diff(column1, column2) + Diff(line1, line2) > 6 THEN
	       SetCursor(scr, line2, column2); RETURN
	    END;

	    IF column1 <> column2 THEN
	       IF (Diff(column1, column2) <= 4) AND
		  OK(CursorRight) AND OK(CursorLeft) THEN
		  IF column2 > column1 THEN
		     Repeat(column2-column1, CursorRight);
		  ELSE
		     Repeat(column1-column2, CursorLeft);
		  END;
	       ELSIF OK(ParmLeftCursor) AND OK(ParmRightCursor) THEN
		  IF column2 > column1 THEN
		     Tparm1(out, ParmRightCursor, column2-column1);
		  ELSE
		     Tparm1(out, ParmLeftCursor, column1-column2);
		  END;
		  Out(out);
	       ELSE
		  (* give up *)
		  SetCursor(scr, line2, column2); RETURN
	       END;
	    END;
	    IF line1 <> line2 THEN
	       IF (Diff(line1, line2) <= 4) AND
		  OK(CursorDown) AND OK(CursorUp) THEN
		  IF line2 > line1 THEN
		     Repeat(line2-line1, CursorDown);
		  ELSE
		     Repeat(line1-line2, CursorUp);
		  END;
	       ELSIF OK(ParmUpCursor) AND OK(ParmDownCursor) THEN
		  IF line2 > line1 THEN
		     Tparm1(out, ParmDownCursor, line2-line1);
		  ELSE
		     Tparm1(out, ParmUpCursor, line1-line2);
		  END;
		  Out(out);
	       ELSE
		  (* give up *)
		  SetCursor(scr, line2, column2); RETURN
	       END;
	    END;
	 END;
      END;
   END MoveCursor;

   PROCEDURE InitScreen(scr: Screen; lns, cols: CARDINAL);
      VAR
	 status: CARDINAL; (* not used *)
	 winsize: Winsize;

      PROCEDURE PrintFile(VAR file: ARRAY OF CHAR);
	 VAR fp: FILE; ch: CHAR;
      BEGIN
	 IF Fopen(fp, file, read, (* buffered = *) TRUE) THEN
	    WHILE Fgetc(ch, fp) AND Fputc(ch, outfp) DO END;
	    IF NOT Fclose(fp) THEN END;
	 END;
      END PrintFile;

   BEGIN
      Done := TRUE;
      WITH scr^ DO
	 InitOut(scr);
	 WITH tinfo DO
	    IF (lns = 0) OR (cols = 0) THEN
	       IF GetWinsize(FileNo(outfp), winsize) AND
		  (winsize.rows # 0) AND (winsize.cols # 0) THEN
		  (* SUN version: dynamic window sizing *)
		  lines := winsize.rows;
		  columns := winsize.cols;
	       ELSE
		  (* take care of Lines or Columns = -1 *)
		  lines := ABS(Lines);
		  columns := ABS(Columns);
	       END;
	    END;
	    IF lns # 0 THEN
	       lines := lns;
	    END;
	    IF cols # 0 THEN
	       columns := cols;
	    END;
	    (* initializations *)
	    IF OK(Init1string) THEN Out(Init1string) END;
	    IF OK(Init2string) THEN Out(Init2string) END;
	    (* InitFile: file name containing further initializations *)
	    IF OK(InitFile) THEN
	       PrintFile(InitFile);
	    END;
	    IF OK(InitProg) AND (InitProg[0] > ' ') THEN
	       (* some bad terminfo descriptions contain iprog=nonsense *)
	       IF NOT Shell(InitProg, status) THEN (* ignore it *) END;
	    END;
	    IF OK(Init3string) THEN Out(Init3string) END;
	 END;
      END;
   END InitScreen;

   PROCEDURE Scroll(scr: Screen; down: BOOLEAN;
		    line, column: CARDINAL; (* upper left corner *)
		    lns, cols: CARDINAL);
      (* scroll part of screen (downward or upward);
	 down: TRUE (downwards) or FALSE (upwards)
	 cursor position is undefined (if Done is set to TRUE)
	 Done set to FALSE if not supported by terminal/terminfo
      *)
      VAR
	 bottom: BOOLEAN; (* region includes bottom line *)
	 out: ARRAY [0..79] OF CHAR;
   BEGIN
      Done := TRUE;
      WITH scr^ DO
	 WITH tinfo DO
	    IF (line + lns > lines) OR (column + cols > columns)
	       (* scroll region does not fit *) OR
	       down & NOT OK(ScrollReverse) OR
	       NOT down & NOT OK(ScrollForward) THEN
	       (* cannot scroll *)
	       Done := FALSE; RETURN
	    END;
	    InitOut(scr);
	    bottom := (cols = columns) & (line + lns = lines);
	    IF (line = 0) & (lns = lines) & (column = 0) & (cols = columns) THEN
	       (* scroll complete screen *)
	       IF down THEN
		  SetCursor(scr, 0, 0); Out(ScrollReverse);
	       ELSE
		  SetCursor(scr, lines-1, 0); Out(ScrollForward);
	       END;
	    ELSIF down & bottom & OK(InsertLine) THEN
	       SetCursor(scr, line, 0); Out(InsertLine);
	    ELSIF NOT down & bottom & OK(DeleteLine) THEN
	       SetCursor(scr, line, 0); Out(DeleteLine);
	    ELSIF (cols = columns) & OK(ChangeScrollRegion) THEN
	       Tparm2(out, ChangeScrollRegion, line, line+lns-1); Out(out);
	       IF down THEN
		  SetCursor(scr, line, 0); Out(ScrollReverse);
	       ELSE
		  SetCursor(scr, line+lns-1, 0); Out(ScrollForward);
	       END;
	       Tparm2(out, ChangeScrollRegion, 0, lines-1); Out(out);
	    ELSIF OK(SetWindow) THEN
	       Tparm4(out, SetWindow, line, line+lns-1, column, column+cols-1);
	       Out(out);
	       IF down THEN
		  SetCursor(scr, line, column); Out(ScrollReverse);
	       ELSE
		  SetCursor(scr, line+lns-1, column); Out(ScrollForward);
	       END;
	       Tparm4(out, SetWindow, 0, lines-1, 0, columns-1); Out(out);
	    ELSE
	       Done := FALSE;
	    END;
	 END;
      END;
   END Scroll;

   PROCEDURE ResetScrollRegions(scr: Screen);
      VAR
	 out: ARRAY [0..79] OF CHAR;
   BEGIN
      Done := TRUE;
      WITH scr^ DO
	 WITH tinfo DO
	    InitOut(scr);
	    IF OK(ChangeScrollRegion) THEN
	       Tparm2(out, ChangeScrollRegion, 0, lines-1); Out(out);
	    ELSIF OK(SetWindow) THEN
	       Tparm4(out, SetWindow, 0, lines-1, 0, columns-1); Out(out);
	    END;
	 END;
      END;
   END ResetScrollRegions;

   PROCEDURE SetCursorVisibility(scr: Screen; visibility: CursorVisibility);
   BEGIN
      Done := TRUE;
      WITH scr^ DO
	 WITH tinfo DO
	    InitOut(scr);
	    CASE visibility OF
	    | normal:      IF OK(CursorNormal) THEN
			      Out(CursorNormal);
			   ELSIF OK(CursorVisible) THEN
			      Out(CursorVisible);
			   ELSE
			      Done := FALSE;
			   END;
	    | invisible:   IF OK(CursorInvisible) THEN
			      Out(CursorInvisible);
			   ELSE
			      Done := FALSE;
			   END;
	    | morevisible: IF OK(CursorVisible) THEN
			      Out(CursorVisible);
			   ELSE
			      Done := FALSE;
			   END;
	    END;
	 END;
      END;
   END SetCursorVisibility;

END Screen.
