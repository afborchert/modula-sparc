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
   $Id: Plot.m2,v 0.2 1997/02/28 15:50:19 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Plot.m2,v $
   Revision 0.2  1997/02/28  15:50:19  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:30  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Plot;

   FROM StdIO IMPORT FILE, stdout, stderr, Fputc, Fflush;
   FROM SysPerror IMPORT Perror;
   FROM FtdIO IMPORT FwriteString, FwriteLn;

   CONST
      nl = 12C;
   VAR
      fp: FILE;

   (* device independent plotter interface; see plot(3) and plot(5) *)

   (* local routines *)

   PROCEDURE Error;
   BEGIN
      Perror("Plot");
      HALT;
   END Error;

   PROCEDURE PutChar(ch: CHAR);
   BEGIN
      IF NOT Fputc(ch, fp) THEN Error END;
   END PutChar;

   PROCEDURE PutInt(value: INTEGER);
      CONST
         MaxShortInt = 77777B;
      VAR
         cvalue: CARDINAL;
         minus: BOOLEAN;
   BEGIN
      IF value < 0 THEN minus := TRUE; value := -value;
      ELSE minus := FALSE END;
      cvalue := CARDINAL(value);
      IF cvalue > MaxShortInt THEN
         FwriteString(stderr, "Plot: value out of bounds");
         FwriteLn(stderr);
         HALT;
      ELSE
         IF minus THEN
            cvalue := CARDINAL(BITSET(-value) - { 0..15 } + { 16 });
         END;
	 (* nice greetings from PDP11: bytes must be swapped *)
         PutChar(CHR(cvalue MOD 400B));
         PutChar(CHR(cvalue DIV 400B));
      END;
   END PutInt;

   (* visible routines *)

   PROCEDURE OpenPlot(f: FILE);
   BEGIN
      fp := f;
   END OpenPlot;

   PROCEDURE ClosePlot;
   BEGIN
      IF Fflush(fp) THEN (* ignore result *) END;
   END ClosePlot;

   PROCEDURE Move(xto, yto: INTEGER);
   BEGIN
      PutChar("m"); PutInt(xto); PutInt(yto);
   END Move;

   PROCEDURE Cont(xto, yto: INTEGER);
   BEGIN
      PutChar("n"); PutInt(xto); PutInt(yto);
   END Cont;

   PROCEDURE Point(xpoint, ypoint: INTEGER);
   BEGIN
      PutChar("p"); PutInt(xpoint); PutInt(ypoint);
   END Point;

   PROCEDURE Line(xfrom, yfrom, xto, yto: INTEGER);
   BEGIN
      PutChar("l"); PutInt(xfrom); PutInt(yfrom); PutInt(xto); PutInt(yto);
   END Line;

   PROCEDURE String(str: ARRAY OF CHAR);
      VAR ix: CARDINAL;
   BEGIN
      PutChar("t");
      ix := 0;
      WHILE (ix <= HIGH(str)) AND (str[ix] <> nl) AND (str[ix] <> 0C) DO
         PutChar(str[ix]);
         INC(ix);
      END;
      PutChar(nl);
   END String;

   PROCEDURE Arc(xcenter, ycenter, xstart, ystart, xend, yend: INTEGER);
   BEGIN
      PutChar("a"); PutInt(xcenter); PutInt(ycenter);
      PutInt(xstart); PutInt(ystart); PutInt(xend); PutInt(yend);
   END Arc;

   PROCEDURE Circle(xcenter, ycenter, radius: INTEGER);
   BEGIN
      PutChar("c"); PutInt(xcenter); PutInt(ycenter); PutInt(radius);
   END Circle;

   PROCEDURE Erase;
   BEGIN
      PutChar("e");
   END Erase;

   PROCEDURE LineMod(style: ARRAY OF CHAR);
      VAR ix: CARDINAL;
   BEGIN
      PutChar("f");
      ix := 0;
      WHILE (ix <= HIGH(style)) AND (style[ix] <> nl) AND (style[ix] <> 0C) DO
         PutChar(style[ix]);
         INC(ix);
      END;
      PutChar(nl); PutChar(0C);
   END LineMod;

   PROCEDURE Space(xupleft, yupleft, xlowright, ylowright: INTEGER);
   BEGIN
      PutChar("s"); PutInt(xupleft); PutInt(yupleft);
      PutInt(xlowright); PutInt(ylowright);
   END Space;

   PROCEDURE Reverse(xupleft, yupleft, xlowright, ylowright: INTEGER);
   BEGIN
      PutChar("R");
      PutInt(xupleft); PutInt(yupleft); PutInt(xlowright); PutInt(ylowright);
   END Reverse;

   PROCEDURE Polygon(xcenter, ycenter, xstart, ystart, edges: INTEGER);
   BEGIN
      PutChar("P");
      PutInt(xcenter); PutInt(ycenter); PutInt(xstart); PutInt(ystart);
      PutInt(edges);
   END Polygon;

   PROCEDURE CharMod(plotchar: CHAR);
   BEGIN
      PutChar("C"); PutChar(plotchar);
   END CharMod;

BEGIN
   fp := stdout;
END Plot.
