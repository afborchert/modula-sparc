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
   $Id: StdFuncs.m2,v 0.2 1997/02/28 15:50:33 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: StdFuncs.m2,v $
   Revision 0.2  1997/02/28  15:50:33  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:35  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE StdFuncs;

   FROM Functions IMPORT Real,
      InstallStdFunc1, InstallStdFunc2, InstallStdConst;
   FROM MathLib IMPORT arctan, exp, ln, sin, cos, sqrt;

   PROCEDURE Power(x, y: Real) : Real;
   BEGIN
      RETURN exp(y*ln(x))
   END Power;

   PROCEDURE Log(base, x: Real) : Real;
   BEGIN
      RETURN ln(x)/ln(base)
   END Log;

   PROCEDURE Log10(x: Real) : Real;
   BEGIN
      RETURN Log(10.0, x)
   END Log10;

   PROCEDURE Tan(x: Real) : Real;
   BEGIN
      RETURN sin(x)/cos(x)
   END Tan;

BEGIN
   InstallStdFunc1("arctan", arctan);
   InstallStdFunc1("exp", exp);
   InstallStdFunc1("ln", ln);
   InstallStdFunc1("sin", sin);
   InstallStdFunc1("cos", cos);
   InstallStdFunc1("sqrt", sqrt);
   InstallStdFunc1("log10", Log10);
   InstallStdFunc1("tan", Tan);

   InstallStdFunc2("log", Log);
   InstallStdFunc2("pow", Power);

   InstallStdConst("pi", 4.0*arctan(1.0));
   InstallStdConst("e", exp(1.0));
END StdFuncs.
