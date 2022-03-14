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
   $Id: OutLines.m2,v 0.2 1997/02/28 15:59:55 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: OutLines.m2,v $
   Revision 0.2  1997/02/28  15:59:55  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:44  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE OutLines; (* WS 7/88 *)

   FROM StdIO IMPORT FILE, stdout;
   FROM LongStrings IMPORT Long, Lwrite, LwritePart, FindChar, StrSize,
      GetChar, ValidLong, CountChar, LastChar;
   FROM Chars IMPORT lf;

   CONST 
      MaxLinesDefault = 23;

   VAR 
      Output : 
         RECORD 
            init : BOOLEAN;
            startln : INTEGER;
            startix : INTEGER;
            stopln : INTEGER;
            stopix : INTEGER;
         END;
   VAR 
      file    : FILE;
      setf    : BOOLEAN;
      (* --- *)
      long    : Long;
      size    : INTEGER;
      lines   : INTEGER;
      setl    : BOOLEAN;
      lastout : INTEGER;
      maxlines: INTEGER;

   PROCEDURE SetLong(l : Long);

   BEGIN 
      long := l;
      IF ValidLong(l) THEN 
         size := VAL(INTEGER,StrSize(long));
         lines := VAL(INTEGER,CountChar(long,lf));
         setl := (size*lines) > 0 ;
         lastout := 0;
	 maxlines := MaxLines;
         Output.init := FALSE;
      END;
   END SetLong;

   PROCEDURE SetFile(f : FILE);

   BEGIN 
      setf := TRUE;
      file := f;
   END SetFile;

   PROCEDURE Check() : BOOLEAN;

   BEGIN 
      RETURN setf AND setl AND ValidLong(long) AND (StrSize(long) = VAL 
         (CARDINAL,size)) AND (lf = GetChar(long,VAL(CARDINAL,size)-1))
	 AND (MaxLines = maxlines) AND (MaxLines > 0);
   END Check;

   PROCEDURE DefineLine(relative : BOOLEAN; ln : INTEGER) : BOOLEAN;

   BEGIN 
      IF NOT Check() THEN 
         RETURN FALSE;
      ELSIF NOT relative THEN 
         IF ln < LastChar THEN 
            RETURN FALSE;
         ELSIF ln = LastChar THEN 
            lastout := lines+1;
         ELSIF ln <= lines THEN 
            lastout := ln;
         ELSE 
            RETURN FALSE;
         END;
         RETURN TRUE;
      ELSE                              (*relative*)
         RETURN DefineLine(FALSE,lastout+ln);
      END;
   END DefineLine;

   PROCEDURE OutRange(start, stop : INTEGER) : BOOLEAN;

      VAR 
         from, to : INTEGER;

      PROCEDURE Ok(VAR line : INTEGER) : BOOLEAN;

      BEGIN 
         IF line < LastChar THEN 
            RETURN FALSE;
         ELSIF line = LastChar THEN 
            line := lines;
            RETURN TRUE;
         ELSIF line = 0 THEN 
            line := 1;
            RETURN TRUE;
         ELSIF line > lines THEN 
            RETURN FALSE;
	 ELSE 
	    RETURN TRUE;
         END;
      END Ok;

   BEGIN 
      IF NOT (Check() AND Ok(start) AND Ok(stop)) THEN 
         RETURN FALSE;
      ELSE 
         WITH Output DO 
            IF start <= 1 THEN 
               from := 0;
            ELSIF init AND (start = (stopln +1)) THEN 
               from := stopix+1;
	    ELSIF start <= (lines - start + 2 ) THEN
	       from := FindChar(long,lf,0,start-1)+1;
	    ELSE
	       from := FindChar(long,lf,LastChar,start-lines-2)+1;
	    END;
            IF stop = lines THEN 
               to := size-1;
            ELSIF init AND (stop = start-1) THEN
               to := startix-1 
            ELSIF stop <= (lines - stop) THEN
	       to := FindChar(long,lf,0,stop);
            ELSE 
	       to := FindChar(long,lf,LastChar,stop-lines-1);
            END;
            IF LwritePart(long,from,to,file) THEN 
               init := TRUE;
               startln := start;
               startix := from;
               stopln := stop;
               stopix := to;
               RETURN TRUE;
            ELSE 
               RETURN FALSE;
            END;
         END;
      END;
   END OutRange;

   PROCEDURE OutAll() : BOOLEAN;

   BEGIN 
      RETURN DefineLine(FALSE,LastChar) AND Lwrite(long,file);
   END OutAll;

   PROCEDURE OutNext() : BOOLEAN;

      VAR 
         start : INTEGER;
         stop  : INTEGER;
         over  : BOOLEAN;

   BEGIN 
      start := lastout+1;
      stop := start+MaxLines-1;
      over := stop > lines;
      IF over THEN 
         stop := lines 
      END;
      IF (start <= lines) AND OutRange(start,stop) THEN 
         IF over THEN 
            lastout := lines+1;
         ELSE 
            lastout := stop;
         END;
         RETURN TRUE;
      ELSE 
         RETURN FALSE;
      END;
   END OutNext;

   PROCEDURE OutPrev() : BOOLEAN;

      VAR 
         start : INTEGER;
         stop  : INTEGER;
         over  : BOOLEAN;

   BEGIN 
      stop := lastout-1;
      start := stop-MaxLines+1;
      over := start < 1;
      IF over THEN 
         start := 1;
      END;
      IF (stop >= 1) AND OutRange(start,stop) THEN 
         IF over THEN 
            lastout := 0;
         ELSE 
            lastout := start;
         END;
         RETURN TRUE;
      ELSE 
         RETURN FALSE;
      END;
   END OutPrev;

BEGIN
   MaxLines := MaxLinesDefault;
   SetFile(stdout);
END OutLines. 
