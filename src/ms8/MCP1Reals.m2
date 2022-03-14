(* Ulm's Modula-2 Compiler    Solaris 2.x/SPARCv8
   -- partially derived from ETH Zurichs Modula-2 Compiler for Lilith --
   Copyright (C) 1983-1996 Universitaet Ulm, SAI, 89069 Ulm, Germany
             (C) 1979-1981 Institut fuer Informatik, ETH Zuerich, Switzerland
   ----------------------------------------------------------------------------
   Modula-2 has been designed and developed by Niklaus Wirth
   at the Institut fuer Informatik, ETH Zuerich, Switzerland
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Compiler is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   Ulm's Modula-2 Compiler is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   $Id: MCP1Reals.m2,v 0.1 1997/02/21 18:40:25 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP1Reals.m2,v $
   Revision 0.1  1997/02/21  18:40:25  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP1Reals; (* LG / AFB *)

   FROM Memory IMPORT ALLOCATE;
   FROM MCBase IMPORT Constval, RealValuePtr, rvalroot;

  (*
   *	SPARCv8 floating point double format (ANSI/IEEE Standard 754-1985)
   *
   *	 0 1       11 12                                          63
   *	+-+----------+------------------------------- - - - --------+
   *	|S|    X     |		F                                   |
   *	+-+----------+------------------------------- - - - --------+
   *
   *	where
   *
   *	S:	sign bit
   *	X:	biased exponent (11 bits)
   *	F:	fraction (52 bits)
   *		
   *	most negative:	ffef ffff ffff ffff'	-1.797693134862314e+308
   *	least negative: 8010 0000 0000 0000'	-2.225073858507201e-308
   *	least positive:	0010 0000 0000 0000'	 2.225073858507201e-308
   *	most positive:	7fef ffff ffff ffff'	 1.797693134862314e+308
   *)

   CONST 
      maxexp = 309;
      minexp = -307;
      maxdignum = 16;

      (* last digit of maxfractdigits 4 (instead of 5) due to *)
      (* rounding errors *)
      maxfractdigits = "1797693134862314";
      minfractdigits = "2225073858507201";

   TYPE 
      DigitString = ARRAY[0..maxdignum-1] OF CHAR;

   VAR 
      minfract, maxfract: REAL;
      r0, r1, r10: REAL;
      rval: REAL;
      base: REAL;
      baseexp: CARDINAL;
      period: BOOLEAN;
      exp: INTEGER;
      dignum: INTEGER;
      eval: INTEGER;
      fok, eok: BOOLEAN;
      negexp: BOOLEAN;
      LeastPositive: ARRAY[0..1] OF CARDINAL;

   PROCEDURE InitRealConst;
      (* initialise the calculation of a constant real number *)
   BEGIN 
      rval := r0;
      base := r1;
      baseexp := 0;
      period := FALSE;
      exp := 0;
      dignum := 0;
      eval := 0;
      eok := TRUE;
      fok := TRUE;
      negexp := FALSE;
   END InitRealConst;

   PROCEDURE ConvertToFraction(ch: CHAR);
      (* convert a character to the fraction of a constant real number *)
   BEGIN 
      IF ch = '.' THEN 
         period := TRUE;
      ELSE 
         IF (dignum = 0) AND (ch = '0') THEN 
            IF period THEN 
               DEC(exp)
            END;
         ELSIF dignum < maxdignum THEN 
            rval := rval * r10 + FLOAT(ORD(ch) - ORD('0'));
            IF period THEN 
               DEC(exp)
            END;
            INC(dignum);
         ELSE 
            INC(baseexp);
            IF baseexp >= ORD(ABS(minexp)) THEN 
               fok := FALSE;
            ELSE 
               base := base / r10;
               rval := rval + base * FLOAT(ORD(ch) - ORD('0'));
            END;
            IF NOT period THEN 
               INC(exp)
            END;
         END;
      END;
   END ConvertToFraction;

   PROCEDURE ConvertToExponent(ch: CHAR);
      (* convert a character to the exponent of a constant real number *)
   BEGIN 
      IF ch = '-' THEN 
         negexp := TRUE;
      ELSE 
         IF eval < 1000 THEN 
            eval := eval * 10 + VAL(INTEGER,ORD(ch) - ORD('0'));
         ELSE 
            eok := FALSE;
         END;
      END;
   END ConvertToExponent;

   PROCEDURE TermRealConst(VAR cval: Constval; VAR long, err: BOOLEAN);
      (* terminate the calculation of a constant real number *)
      VAR 
         totexp : INTEGER;

      PROCEDURE TenTo(exp: INTEGER): REAL;
         VAR 
            r, res : REAL;
      BEGIN 
         res := r1;
         r := r10;
         LOOP 
            IF ODD(exp) THEN 
               res := res * r 
            END;
            exp := exp DIV 2;
            IF exp = 0 THEN 
               EXIT 
            END;
            r := r * r;
         END;
         RETURN res 
      END TenTo;

   BEGIN (* TermRealConst *)
      err := FALSE;
      long := FALSE;
      IF fok AND eok THEN 
         IF negexp THEN 
            DEC(exp,eval)
         ELSE 
            INC(exp,eval)
         END;
         totexp := dignum + exp;
         IF totexp > maxexp THEN 
            exp := 0;
            err := TRUE;
         ELSIF (totexp = maxexp) OR (totexp = minexp) THEN 
               (* compare with maxfract/minfract *) ;
            WHILE dignum < maxdignum DO 
               rval := rval * r10;
               INC(dignum);
               DEC(exp);
            END;
            IF (totexp = maxexp) AND (rval > maxfract) OR (totexp = minexp)
               AND (rval < minfract) THEN 
               exp := 0;
               rval := r0;
               err := totexp = maxexp;
            ELSIF (totexp = minexp) AND (rval = minfract) THEN 
            (* avoid underflow *)
               rval := REAL(LeastPositive);
               exp := 0;
            END;
         ELSIF totexp < minexp THEN 
            exp := 0;
            rval := r0;
         END;
         IF exp > 0 THEN                (* maximal value is maxexp - 1 *)
            rval := rval * TenTo(exp);
         ELSIF exp < 0 THEN           (* minimal value is minexp - maxdignum *)
            IF exp < -(maxexp-1) + maxdignum THEN 
               rval := rval / TenTo(-(maxexp-1) + maxdignum - exp);
               exp := -(maxexp-1) + maxdignum;
            END;
            rval := rval / TenTo(- exp);
         END;
      ELSE                              (* not (eok or fok) *)
         IF dignum <> 0 THEN 
            err := TRUE 
         END;
      END;
      IF err THEN 
         rval := r0 
      END;
      NEW(cval.rvalue);
      WITH cval.rvalue^ DO 
         label.ok := FALSE;
         rvalue := rval;
      END;
      IncludeRealConst(cval.rvalue);
   END TermRealConst;

   PROCEDURE IncludeRealConst(rval: RealValuePtr);
      (* include the given real value into the list of
	 to be generated real constants
      *)
   BEGIN
      rval^.rlink := rvalroot;
      rvalroot := rval;
   END IncludeRealConst;

   PROCEDURE InitFraction(str: DigitString; VAR fract: REAL);
      VAR 
         ix : CARDINAL;
   BEGIN 
      InitRealConst;
      ix := 0;
      WHILE (ix < maxdignum) AND (str[ix] <> 0C) DO 
         ConvertToFraction(str[ix]);
         INC(ix);
      END;
      fract := rval;
   END InitFraction;

BEGIN                                   (* MCP1Reals *)
   r0 := FLOAT(0);
   r1 := FLOAT(1);
   r10 := FLOAT(10);
   InitFraction(maxfractdigits, maxfract);
   InitFraction(minfractdigits, minfract);
   LeastPositive[0] := 00100000H;
   LeastPositive[1] := 0H;
   rvalroot := NIL;
END MCP1Reals. 
