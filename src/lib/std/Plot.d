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
   $Id: Plot.d,v 0.2 1997/02/28 15:50:18 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Plot.d,v $
   Revision 0.2  1997/02/28  15:50:18  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:07  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Plot;

   FROM StdIO IMPORT FILE;

   (* device independent plotter interface; see plot(3) and plot(5) *)

   PROCEDURE OpenPlot(f: FILE);

   PROCEDURE ClosePlot;

   PROCEDURE Move(xto, yto: INTEGER);

   PROCEDURE Cont(xto, yto: INTEGER);

   PROCEDURE Point(xpoint, ypoint: INTEGER);

   PROCEDURE Line(xfrom, yfrom, xto, yto: INTEGER);

   PROCEDURE String(str: ARRAY OF CHAR);

   PROCEDURE Arc(xcenter, ycenter, xstart, ystart, xend, yend: INTEGER);

   PROCEDURE Circle(xcenter, ycenter, radius: INTEGER);

   PROCEDURE Erase;

   PROCEDURE LineMod(style: ARRAY OF CHAR);

   PROCEDURE Space(xupleft, yupleft, xlowright, ylowright: INTEGER);

   PROCEDURE Reverse(xupleft, yupleft, xlowright, ylowright: INTEGER);

   PROCEDURE Polygon(xcenter, ycenter, xstart, ystart, edges: INTEGER);

   PROCEDURE CharMod(plotchar: CHAR);

END Plot.
