(* Ulm's Modula-2 Library
   Copyright (C) 1983-1996 by University of Ulm, SAI, D-89069 Ulm, Germany,
             (C) 1992 Free Software Foundation, Inc.,
	     (C) 1985, 1993 The Regents of the University of California
   ----------------------------------------------------------------------------
   Note that parts of this module have been derived from the GNU C Library
   which itself has been partially derived from sources of
   the Regents of the University of California.
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
   Copyright (c) 1985, 1993
        The Regents of the University of California.  All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
   3. All advertising materials mentioning features or use of this software
      must display the following acknowledgement:
        This product includes software developed by the University of
        California, Berkeley and its contributors.
   4. Neither the name of the University nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
   OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
   SUCH DAMAGE.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: MathLib.m2,v 0.2 1997/02/28 15:50:14 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MathLib.m2,v $
   Revision 0.2  1997/02/28  15:50:14  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:27  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MathLib;

   (* implementation for the SPARCv8 platform *)

   FROM SYSTEM IMPORT SQRT;

   TYPE
      Error = (
	 plusInfinity,    (* range error: ERANGE *)
	 minusInfinity,   (* range error: -ERANGE *)
	 underflow,       (* range error: ERANGE *)
	 outOfDomain,     (* domain error: EDOM *)
	 endOfList
      );
   VAR
      error: Error;
   VAR
      hugeVal: REAL;
      nan: REAL;
      half: REAL;

   CONST
      maxexp = 1024;  (* DBL_MAX_EXP in /usr/include/float.h *)
      minexp = -1021; (* DBL_MIN_EXP *)
      mantdig = 53;   (* DBL_MANT_DIG *)
   CONST
      (* from support.c of GNU C Library *)
      prep1 = 54; bias = 1023;
      novf = 1.7E308; nunf = 3.0E-308;

   MODULE IEEE754Conversions;

      EXPORT IEEE754, IEEE754Real2ExpMan, IEEE754ExpMan2Real;

      TYPE
	 IEEE754 =
	    RECORD
	       negative: BOOLEAN;
	       exponent: CARDINAL;
	       mantissa0: CARDINAL;
	       mantissa1: CARDINAL;
	    END;

      CONST
	 realsize = 8;
	 signMask = 080000000H;
	 expMask =  07FF00000H;
	 expShift = 000100000H;
	 manMask =  0000FFFFFH;
      TYPE
	 Real = ARRAY [0..1] OF BITSET;

      PROCEDURE IEEE754Real2ExpMan(x: REAL; VAR ieee754: IEEE754);
	 VAR
	    real: Real;
      BEGIN
	 real := Real(x);
	 ieee754.negative := BITSET(signMask) * real[0] # {};
	 ieee754.exponent := CARDINAL(BITSET(expMask) * real[0]) DIV expShift;
	 ieee754.mantissa0 := CARDINAL(BITSET(manMask) * real[0]);
	 ieee754.mantissa1 := CARDINAL(real[1]);
      END IEEE754Real2ExpMan;

      PROCEDURE IEEE754ExpMan2Real(ieee754: IEEE754; VAR x: REAL);
	 VAR
	    real: Real;
      BEGIN
	 real[0] := BITSET(ieee754.mantissa0);
	 real[0] := real[0] + BITSET(ieee754.exponent * expShift);
	 IF ieee754.negative THEN
	    real[0] := real[0] + BITSET(signMask);
	 END;
	 real[1] := BITSET(ieee754.mantissa1);
	 x := REAL(real);
      END IEEE754ExpMan2Real;

   END IEEE754Conversions;

   PROCEDURE CopySign(x, y: REAL) : REAL;
      (* return x with its sign changed to that of y *)
   BEGIN
      x := ABS(x);
      IF y < 0.0 THEN
	 RETURN -x
      ELSE
	 RETURN x
      END;
   END CopySign;

   PROCEDURE IsNaN(x: REAL) : BOOLEAN;
      (* return TRUE if x is not a number *)
   BEGIN
      RETURN x # x
   END IsNaN;

   PROCEDURE IsInf(x: REAL) : BOOLEAN;
      VAR
	 ieee754: IEEE754;
   BEGIN
      IEEE754Real2ExpMan(x, ieee754);
      RETURN (ieee754.exponent = 07FFH) &
             (ieee754.mantissa0 = 0) & (ieee754.mantissa1 = 0)
   END IsInf;

   PROCEDURE Finite(x: REAL) : BOOLEAN;
   BEGIN
      RETURN ~IsNaN(x) & ~IsInf(x)
   END Finite;

   PROCEDURE InfNaN(error: Error) : REAL;
      (* Deal with an infinite or NaN result.
	 If error is plusInfinity, result is +Inf;
	 if error is minusInfinity, result is -Inf;
	 otherwise result is NaN.
	 May return an infinity or NaN, or may do something else.
      *)
   BEGIN
      CASE error OF
      | plusInfinity:   RETURN hugeVal
      | minusInfinity:  RETURN - hugeVal
      ELSE              RETURN nan
      END;
   END InfNaN;

   PROCEDURE Rint(x: REAL) : REAL;
      (* algorithm for rint(x) in pseudo-pascal form ...

	 real rint(x): real x;
	      ... delivers integer nearest x in direction of prevailing
              ... rounding mode
	 const        L = (last consecutive integer)/2
		= 2**52; for IEEE 754 Double
	 real s,t;
	 begin
	      if x != x then return x;       ... NaN
	      if |x| >= L then return x;     ... already an integer
	      s := copysign(L,x);
	      t := x + s;                    ... = (x+s) rounded to integer
	      return t - s
	 end;

	 Note: Inexact will be signaled if x is not an integer, as is
	      customary for IEEE 754.  No other signal can be emitted.
      *)
      CONST
	 L = 4503599627370496.0; (* 2^52 *)
      VAR
	 s, t: REAL;
   BEGIN
      IF x # x THEN
	 RETURN x (* NaN *)
      END;
      IF ABS(x) > L THEN (* already an integer *)
	 RETURN x
      END;
      s := CopySign(L, x);
      t := x + s; (* x+s rounded to integer *)
      RETURN t - s
   END Rint;

   PROCEDURE LogB(x: REAL) : REAL;
      (* return the base 2 signed integral exponent of X *)
      VAR
	 ieee754: IEEE754;
   BEGIN
      IF IsNaN(x) THEN
	 RETURN x
      ELSIF IsInf(x) THEN
	 RETURN hugeVal
      ELSIF x = 0.0 THEN
	 RETURN -hugeVal
      END;
      IEEE754Real2ExpMan(x, ieee754);
      IF ieee754.exponent = 0 THEN
	 (* A denormalized number.
            Multiplying by 2 ** DBL_MANT_DIG normalizes it;
	    we then subtract the DBL_MANT_DIG we added to the exponent.
	 *)
	 RETURN LogB(x * LdExp(1.0, mantdig)) - FLOAT(mantdig);
      ELSE
	 RETURN FLOAT(ieee754.exponent - (maxexp - 1))
      END;
   END LogB;

   PROCEDURE LdExp(x: REAL; exp: INTEGER) : REAL;
      VAR
	 ieee754: IEEE754;
	 exponent: INTEGER;
   BEGIN
      IEEE754Real2ExpMan(x, ieee754);
      exponent := ieee754.exponent;

      (* The order of the tests is carefully chosen to handle
         the usual case first, with no branches taken
      *)

      IF exponent # 0 THEN
	 (* X is nonzero and not denormalized *)
	 IF exponent <= maxexp - minexp + 1 THEN
	    (* X is finite.  When EXP < 0, overflow is actually underflow. *)
	    INC(exponent, exp);
	    IF exponent # 0 THEN
	       IF exponent <= maxexp - minexp + 1 THEN
		  (* in range *)
		  ieee754.exponent := exponent;
		  IEEE754ExpMan2Real(ieee754, x); RETURN x
	       END;
	       IF exp >= 0 THEN
		  (* overflow *)
		  IF x > 0.0 THEN
		     error := plusInfinity;
		     RETURN hugeVal
		  ELSE
		     error := minusInfinity;
		     RETURN - hugeVal
		  END;
	       END;
	       IF exponent <= mantdig + 1 THEN
		  (* underflow *)
		  IF x > 0.0 THEN
		     error := underflow;
		     RETURN 0.0
		  ELSE
		     error := underflow;
		     RETURN - 0.0
		  END;
	       END;
	    END;

	    (* gradual underflow *)
	    ieee754.exponent := 1;
	    IEEE754ExpMan2Real(ieee754, x);
	    x := x * LdExp(1.0, exponent - 1);
	    IEEE754Real2ExpMan(x, ieee754);
	    IF (ieee754.mantissa0 = 0) & (ieee754.mantissa1 = 0) THEN
	       error := underflow;
	    END;
	    RETURN x
	 END;

	 (* X is +-infinity or NaN *)
	 IF (ieee754.mantissa0 = 0) & (ieee754.mantissa1 = 0) THEN
	    (* X is +-infinity *)
	    IF exponent >= 0 THEN
	       (* overflow *)
	       IF x > 0.0 THEN
		  error := plusInfinity;
		  RETURN hugeVal
	       ELSE
		  error := minusInfinity;
		  RETURN - hugeVal
	       END;
	    ELSE
	       (* (infinity * number < 1).  With infinite precision,
	          (infinity / finite) would be infinity, but otherwise it's
		  safest to regard (infinity / 2) as indeterminate.  The
		  infinity might be (2 * finite).
	       *)
	       error := outOfDomain;
	       RETURN nan;
	    END;
	 END;

	 (* X is NaN *)
	 error := outOfDomain;
	 RETURN x
      END;

      (* X is zero or denormalized *)
      IF (ieee754.mantissa0 = 0) & (ieee754.mantissa1 = 0) THEN
	 (* X is +-0.0 *)
	 RETURN x
      END;

      (* X is denormalized.
         Multiplying by 2 ** DBL_MANT_DIG normalizes it;
	 we then subtract the DBL_MANT_DIG we added to the exponent.
      *)
      RETURN LdExp(x * LdExp(1.0, mantdig), exp - mantdig)
   END LdExp;

   PROCEDURE Frexp(x: REAL; VAR exp: INTEGER) : REAL;
      (* Break x into a normalized fraction and an integral power of 2 *)
      VAR
	 ieee754: IEEE754;
   BEGIN
      IF x = 0.0 THEN
	 exp := 0; RETURN 0.0
      ELSE
	 IEEE754Real2ExpMan(x, ieee754);
	 exp := ieee754.exponent - 1022;
	 ieee754.exponent := 1022;
	 IEEE754ExpMan2Real(ieee754, x);
	 RETURN x
      END;
   END Frexp;

   PROCEDURE Scalb(x: REAL; n: INTEGER) : REAL;
      (* returns  x * (2^n), for integer values n *)
      VAR
	 ieee754: IEEE754;
	 k: INTEGER;
   BEGIN
      IF x = 0.0 THEN RETURN x END;
      IEEE754Real2ExpMan(x, ieee754);
      k := ieee754.exponent;
      IF k # 07FFH THEN
	 IF n < - 2100 THEN
	    RETURN nunf * nunf
	 ELSIF n > 2100 THEN
	    RETURN novf + novf
	 END;
	 IF k = 0 THEN
	    x := x * Scalb(1.0, prep1);
	    RETURN Scalb(x, n - prep1)
	 END;
	 k := k + n;
	 IF k > 0 THEN
	    IF k < 07FFH THEN
	       ieee754.exponent := k;
	       IEEE754ExpMan2Real(ieee754, x);
	    ELSE
	       x := novf + novf;
	    END;
	 ELSE
	    IF k > - prep1 THEN
	       (* gradual underflow *)
	       ieee754.exponent := 1;
	       IEEE754ExpMan2Real(ieee754, x);
	       x := x * Scalb(1.0, k - 1);
	    ELSE
	       RETURN nunf * nunf
	    END;
	 END;
      END;
      RETURN x
   END Scalb;

   PROCEDURE Rem(x, y: REAL) : REAL;
      VAR
	 utmp, ua, ux, uy: IEEE754;
	 a, b, tmp: REAL;
	 negative: BOOLEAN;
	 k: INTEGER;
   BEGIN
      IEEE754Real2ExpMan(x, ux);
      y := ABS(y);
      IEEE754Real2ExpMan(y, uy);
      IF ~Finite(x) OR (y = 0.0) THEN
	 RETURN nan
      ELSIF IsNaN(y) THEN
	 RETURN y
      ELSIF IsInf(y) THEN
	 RETURN x
      ELSIF uy.exponent <= 1 THEN
	 (* Subnormal (or almost subnormal) Y value *)
	 b := Scalb(1.0, 54);
	 IEEE754ExpMan2Real(uy, y);
	 y := y * b;
	 x := Rem(x, y);
	 x := x * b;
	 RETURN Rem(x, y) / b
      ELSIF y >= 1.7e308 / 2.0 THEN
	 y := y / 2.0;
	 x := x / 2.0;
	 RETURN Rem(x, y) * 2.0
      ELSE
	 negative := ux.negative;
	 a := y + y; IEEE754Real2ExpMan(a, ua);
	 b := y / 2.0;
	 ux.negative := FALSE; IEEE754ExpMan2Real(ux, x);
	 WHILE x > a DO
	    k := ux.exponent - ua.exponent;
	    tmp := a; IEEE754Real2ExpMan(tmp, utmp);
	    INC(utmp.exponent, k); IEEE754ExpMan2Real(utmp, tmp);
	    IF x < tmp THEN
	       DEC(utmp.exponent);
	       IEEE754ExpMan2Real(utmp, tmp);
	    END;
	    x := x - tmp; IEEE754Real2ExpMan(x, ux);
	 END;
	 IF x > b THEN
	    x := x - y;
	    IF x >= b THEN
	       x := x - y;
	    END;
	 END;
	 IEEE754Real2ExpMan(x, ux);
	 ux.negative := ux.negative # negative;
	 IEEE754ExpMan2Real(ux, x);
	 RETURN x
      END;
   END Rem;

   PROCEDURE atan2(y, x: REAL) : REAL;
      CONST
	 athfhi = 4.6364760900080609352E-1;
	 athflo = 4.6249969567426939759E-18;
	 PIo4 = 7.8539816339744827900E-1;
	 at1fhi = 9.8279372324732905408E-1;
	 at1flo = -2.4407677060164810007E-17;
	 PIo2 = 1.5707963267948965580E0;
	 PI = 3.1415926535897931160E0;
	 a1 = 3.3333333333333942106E-1;
	 a2 = -1.9999999999979536924E-1;
	 a3 = 1.4285714278004377209E-1;
	 a4 = -1.1111110579344973814E-1;
	 a5 = 9.0908906105474668324E-2;
	 a6 = -7.6919217767468239799E-2;
	 a7 = 6.6614695906082474486E-2;
	 a8 = -5.8358371008508623523E-2;
	 a9 = 4.9850617156082015213E-2;
	 a10 = -3.6700606902093604877E-2;
	 a11 = 1.6438029044759730479E-2;
      CONST
	 small = 1.0E-9;
	 big = 1.0E18;
      VAR
	 t, z, signy, signx, hi, lo: REAL;
	 k, m: INTEGER;

      PROCEDURE DoIt() : REAL;
	 VAR
	    tmp: REAL;
      BEGIN
	 (* begin argument reduction *)
	 IF t < 2.4375 THEN
	    (* truncate 4(t+1/16) to integer for branching *)
	    k := TRUNC(4.0 * (t + 0.0625));
	    CASE k OF
	    | 0, 1: (* t is in [0, 7/16] *)
		  IF t < small THEN
		     tmp := big + small; (* raise inexact flag *)
		     IF signx > 0.0 THEN
			RETURN CopySign(t, signy)
		     ELSE
			RETURN CopySign(PI - t, signy)
		     END;
		  END;
		  hi := 0.0; lo := 0.0;
	    | 2:    (* t is in [7/16, 11/16] *)
		  hi := athfhi; lo := athflo;
		  z := x + x;
		  t := ((y + y) - x) / (z + y);
	    | 3, 4: (* t is in [11/16, 19/16] *)
		  hi := PIo4; lo := 0.0;
		  t := (y - x) / (x + y);
	    ELSE    (* t is in [19/16, 39/16] *)
		  hi := at1fhi; lo := at1flo;
		  z := y - x; y := y + y + y; t := x + x;
		  t := ((z + z) - x) / (t + y);
	    END;
	 ELSE
	    hi := PIo2; lo := 0.0;

	    IF t <= big THEN
	       (* t is in [2.4375, big] *)
	       t := - x / y;
	    ELSE
	       (* t is in [big, INF] *)
	       tmp := big + small; (* raise inexact flag *)
	       t := 0.0;
	    END;
	 END;

	 (* compute atan(t) for t in [-.4375, .4375] *)
	 z := t * t;
	 z := t*(z*(a1+z*(a2+z*(a3+z*(a4+z*(a5+z*(a6+z*(a7+z*(a8+
	         z*(a9+z*(a10+z*a11)))))))))));
	 z := lo - z; z := z + t; z := z + hi;

	 IF signx > 0.0 THEN
	    RETURN CopySign(z, signy)
	 ELSE
	    RETURN CopySign(PI - z, signy)
	 END;
      END DoIt;

   BEGIN (* atan2 *)
      (* if x or y is NaN *)
      IF x # x THEN
	 RETURN x
      ELSIF y # y THEN
	 RETURN y
      END;

      (* copy down the sign of y and x *)
      signy := CopySign(1.0, y);
      signx := CopySign(1.0, x);

      IF x = 1.0 THEN
	 y := CopySign(y, 1.0); t := y;
	 IF Finite(t) THEN
	    RETURN DoIt()
	 END;
      END;

      IF y = 0.0 THEN
	 IF signx = 1.0 THEN
	    RETURN y
	 ELSE
	    RETURN CopySign(PI, signy)
	 END;
      END;

      IF x = 0.0 THEN
	 RETURN CopySign(PIo2, signy)
      END;

      IF ~Finite(x) THEN
	 IF ~Finite(y) THEN
	    IF signx = 1.0 THEN
	       RETURN CopySign(PIo4, signy)
	    ELSE
	       RETURN CopySign(3.0 * PIo4, signy)
	    END;
	 ELSE
	    IF signx = 1.0 THEN
	       RETURN CopySign(0.0, signy)
	    ELSE
	       RETURN CopySign(PI, signy)
	    END;
	 END;
      END;

      IF ~Finite(y) THEN
	 RETURN CopySign(PIo2, signy)
      END;

      x := CopySign(x, 1.0);
      y := CopySign(y, 1.0);

      k := TRUNC(LogB(y));
      m := k - TRUNC(LogB(x));
      IF m > 60 THEN
	 t := big + big;
      ELSIF m < -80 THEN
	 t := y / x;
      ELSE
	 t := y / x; y := Scalb(y, -k); x := Scalb(x, -k);
      END;

      RETURN DoIt()
   END atan2;

   PROCEDURE Round(x: REAL) : INTEGER;
   BEGIN
      RETURN TRUNC(x + 0.5)
   END Round;

   MODULE LogTables;
      
      (* tables which are needed by ln *)

      EXPORT logFHead, logFTail;

      CONST
	 n = 128;

      VAR
	 logFHead, logFTail: ARRAY [0..n] OF REAL;
	 i: [0..n+1];
   BEGIN
      i := 0;
      logFHead[i] := 0.0; INC(i);
      logFHead[i] := 0.007782140442060381246; INC(i);
      logFHead[i] := 0.015504186535963526694; INC(i);
      logFHead[i] := 0.023167059281547608406; INC(i);
      logFHead[i] := 0.030771658666765233647; INC(i);
      logFHead[i] := 0.038318864302141264488; INC(i);
      logFHead[i] := 0.045809536031242714670; INC(i);
      logFHead[i] := 0.053244514518837604555; INC(i);
      logFHead[i] := 0.060624621816486978786; INC(i);
      logFHead[i] := 0.067950661908525944454; INC(i);
      logFHead[i] := 0.075223421237524235039; INC(i);
      logFHead[i] := 0.082443669210988446138; INC(i);
      logFHead[i] := 0.089612158689760690322; INC(i);
      logFHead[i] := 0.096729626458454731618; INC(i);
      logFHead[i] := 0.103796793681567578460; INC(i);
      logFHead[i] := 0.110814366340264314203; INC(i);
      logFHead[i] := 0.117783035656430001836; INC(i);
      logFHead[i] := 0.124703478501032805070; INC(i);
      logFHead[i] := 0.131576357788617315236; INC(i);
      logFHead[i] := 0.138402322859292326029; INC(i);
      logFHead[i] := 0.145182009844575077295; INC(i);
      logFHead[i] := 0.151916042025732167530; INC(i);
      logFHead[i] := 0.158605030176659056451; INC(i);
      logFHead[i] := 0.165249572895390883786; INC(i);
      logFHead[i] := 0.171850256926518341060; INC(i);
      logFHead[i] := 0.178407657472689606947; INC(i);
      logFHead[i] := 0.184922338493834104156; INC(i);
      logFHead[i] := 0.191394852999565046047; INC(i);
      logFHead[i] := 0.197825743329758552135; INC(i);
      logFHead[i] := 0.204215541428766300668; INC(i);
      logFHead[i] := 0.210564769107350002741; INC(i);
      logFHead[i] := 0.216873938300523150246; INC(i);
      logFHead[i] := 0.223143551314024080056; INC(i);
      logFHead[i] := 0.229374101064877322642; INC(i);
      logFHead[i] := 0.235566071312860003672; INC(i);
      logFHead[i] := 0.241719936886966024758; INC(i);
      logFHead[i] := 0.247836163904594286577; INC(i);
      logFHead[i] := 0.253915209980732470285; INC(i);
      logFHead[i] := 0.259957524436686071567; INC(i);
      logFHead[i] := 0.265963548496984003577; INC(i);
      logFHead[i] := 0.271933715484010463114; INC(i);
      logFHead[i] := 0.277868451003087102435; INC(i);
      logFHead[i] := 0.283768173130738432519; INC(i);
      logFHead[i] := 0.289633292582948342896; INC(i);
      logFHead[i] := 0.295464212893421063199; INC(i);
      logFHead[i] := 0.301261330578199704177; INC(i);
      logFHead[i] := 0.307025035294827830512; INC(i);
      logFHead[i] := 0.312755710004239517729; INC(i);
      logFHead[i] := 0.318453731118097493890; INC(i);
      logFHead[i] := 0.324119468654316733591; INC(i);
      logFHead[i] := 0.329753286372579168528; INC(i);
      logFHead[i] := 0.335355541920762334484; INC(i);
      logFHead[i] := 0.340926586970454081892; INC(i);
      logFHead[i] := 0.346466767346100823488; INC(i);
      logFHead[i] := 0.351976423156884266063; INC(i);
      logFHead[i] := 0.357455888922231679316; INC(i);
      logFHead[i] := 0.362905493689140712376; INC(i);
      logFHead[i] := 0.368325561158599157352; INC(i);
      logFHead[i] := 0.373716409793814818840; INC(i);
      logFHead[i] := 0.379078352934811846353; INC(i);
      logFHead[i] := 0.384411698910298582632; INC(i);
      logFHead[i] := 0.389716751140440464951; INC(i);
      logFHead[i] := 0.394993808240542421117; INC(i);
      logFHead[i] := 0.400243164127459749579; INC(i);
      logFHead[i] := 0.405465108107819105498; INC(i);
      logFHead[i] := 0.410659924985338875558; INC(i);
      logFHead[i] := 0.415827895143593195825; INC(i);
      logFHead[i] := 0.420969294644237379543; INC(i);
      logFHead[i] := 0.426084395310681429691; INC(i);
      logFHead[i] := 0.431173464818130014464; INC(i);
      logFHead[i] := 0.436236766774527495726; INC(i);
      logFHead[i] := 0.441274560805140936281; INC(i);
      logFHead[i] := 0.446287102628048160113; INC(i);
      logFHead[i] := 0.451274644139630254358; INC(i);
      logFHead[i] := 0.456237433481874177232; INC(i);
      logFHead[i] := 0.461175715122408291790; INC(i);
      logFHead[i] := 0.466089729924533457960; INC(i);
      logFHead[i] := 0.470979715219073113985; INC(i);
      logFHead[i] := 0.475845904869856894947; INC(i);
      logFHead[i] := 0.480688529345570714212; INC(i);
      logFHead[i] := 0.485507815781602403149; INC(i);
      logFHead[i] := 0.490303988045525329653; INC(i);
      logFHead[i] := 0.495077266798034543171; INC(i);
      logFHead[i] := 0.499827869556611403822; INC(i);
      logFHead[i] := 0.504556010751912253908; INC(i);
      logFHead[i] := 0.509261901790523552335; INC(i);
      logFHead[i] := 0.513945751101346104405; INC(i);
      logFHead[i] := 0.518607764208354637958; INC(i);
      logFHead[i] := 0.523248143765158602036; INC(i);
      logFHead[i] := 0.527867089620485785417; INC(i);
      logFHead[i] := 0.532464798869114019908; INC(i);
      logFHead[i] := 0.537041465897345915436; INC(i);
      logFHead[i] := 0.541597282432121573947; INC(i);
      logFHead[i] := 0.546132437597407260909; INC(i);
      logFHead[i] := 0.550647117952394182793; INC(i);
      logFHead[i] := 0.555141507540611200965; INC(i);
      logFHead[i] := 0.559615787935399566777; INC(i);
      logFHead[i] := 0.564070138285387656651; INC(i);
      logFHead[i] := 0.568504735352689749561; INC(i);
      logFHead[i] := 0.572919753562018740922; INC(i);
      logFHead[i] := 0.577315365035246941260; INC(i);
      logFHead[i] := 0.581691739635061821900; INC(i);
      logFHead[i] := 0.586049045003164792433; INC(i);
      logFHead[i] := 0.590387446602107957005; INC(i);
      logFHead[i] := 0.594707107746216934174; INC(i);
      logFHead[i] := 0.599008189645246602594; INC(i);
      logFHead[i] := 0.603290851438941899687; INC(i);
      logFHead[i] := 0.607555250224322662688; INC(i);
      logFHead[i] := 0.611801541106615331955; INC(i);
      logFHead[i] := 0.616029877215623855590; INC(i);
      logFHead[i] := 0.620240409751204424537; INC(i);
      logFHead[i] := 0.624433288012369303032; INC(i);
      logFHead[i] := 0.628608659422752680256; INC(i);
      logFHead[i] := 0.632766669570628437213; INC(i);
      logFHead[i] := 0.636907462236194987781; INC(i);
      logFHead[i] := 0.641031179420679109171; INC(i);
      logFHead[i] := 0.645137961373620782978; INC(i);
      logFHead[i] := 0.649227946625615004450; INC(i);
      logFHead[i] := 0.653301272011958644725; INC(i);
      logFHead[i] := 0.657358072709030238911; INC(i);
      logFHead[i] := 0.661398482245203922502; INC(i);
      logFHead[i] := 0.665422632544505177065; INC(i);
      logFHead[i] := 0.669430653942981734871; INC(i);
      logFHead[i] := 0.673422675212350441142; INC(i);
      logFHead[i] := 0.677398823590920073911; INC(i);
      logFHead[i] := 0.681359224807238206267; INC(i);
      logFHead[i] := 0.685304003098281100392; INC(i);
      logFHead[i] := 0.689233281238557538017; INC(i);
      logFHead[i] := 0.693147180560117703862; INC(i);

      i := 0;
      logFTail[i] := 0.; INC(i);
      logFTail[i] := -0.00000000000000543229938420049; INC(i);
      logFTail[i] :=  0.00000000000000172745674997061; INC(i);
      logFTail[i] := -0.00000000000001323017818229233; INC(i);
      logFTail[i] := -0.00000000000001154527628289872; INC(i);
      logFTail[i] := -0.00000000000000466529469958300; INC(i);
      logFTail[i] :=  0.00000000000005148849572685810; INC(i);
      logFTail[i] := -0.00000000000002532168943117445; INC(i);
      logFTail[i] := -0.00000000000005213620639136504; INC(i);
      logFTail[i] := -0.00000000000001819506003016881; INC(i);
      logFTail[i] :=  0.00000000000006329065958724544; INC(i);
      logFTail[i] :=  0.00000000000008614512936087814; INC(i);
      logFTail[i] := -0.00000000000007355770219435028; INC(i);
      logFTail[i] :=  0.00000000000009638067658552277; INC(i);
      logFTail[i] :=  0.00000000000007598636597194141; INC(i);
      logFTail[i] :=  0.00000000000002579999128306990; INC(i);
      logFTail[i] := -0.00000000000004654729747598444; INC(i);
      logFTail[i] := -0.00000000000007556920687451336; INC(i);
      logFTail[i] :=  0.00000000000010195735223708472; INC(i);
      logFTail[i] := -0.00000000000017319034406422306; INC(i);
      logFTail[i] := -0.00000000000007718001336828098; INC(i);
      logFTail[i] :=  0.00000000000010980754099855238; INC(i);
      logFTail[i] := -0.00000000000002047235780046195; INC(i);
      logFTail[i] := -0.00000000000008372091099235912; INC(i);
      logFTail[i] :=  0.00000000000014088127937111135; INC(i);
      logFTail[i] :=  0.00000000000012869017157588257; INC(i);
      logFTail[i] :=  0.00000000000017788850778198106; INC(i);
      logFTail[i] :=  0.00000000000006440856150696891; INC(i);
      logFTail[i] :=  0.00000000000016132822667240822; INC(i);
      logFTail[i] := -0.00000000000007540916511956188; INC(i);
      logFTail[i] := -0.00000000000000036507188831790; INC(i);
      logFTail[i] :=  0.00000000000009120937249914984; INC(i);
      logFTail[i] :=  0.00000000000018567570959796010; INC(i);
      logFTail[i] := -0.00000000000003149265065191483; INC(i);
      logFTail[i] := -0.00000000000009309459495196889; INC(i);
      logFTail[i] :=  0.00000000000017914338601329117; INC(i);
      logFTail[i] := -0.00000000000001302979717330866; INC(i);
      logFTail[i] :=  0.00000000000023097385217586939; INC(i);
      logFTail[i] :=  0.00000000000023999540484211737; INC(i);
      logFTail[i] :=  0.00000000000015393776174455408; INC(i);
      logFTail[i] := -0.00000000000036870428315837678; INC(i);
      logFTail[i] :=  0.00000000000036920375082080089; INC(i);
      logFTail[i] := -0.00000000000009383417223663699; INC(i);
      logFTail[i] :=  0.00000000000009433398189512690; INC(i);
      logFTail[i] :=  0.00000000000041481318704258568; INC(i);
      logFTail[i] := -0.00000000000003792316480209314; INC(i);
      logFTail[i] :=  0.00000000000008403156304792424; INC(i);
      logFTail[i] := -0.00000000000034262934348285429; INC(i);
      logFTail[i] :=  0.00000000000043712191957429145; INC(i);
      logFTail[i] := -0.00000000000010475750058776541; INC(i);
      logFTail[i] := -0.00000000000011118671389559323; INC(i);
      logFTail[i] :=  0.00000000000037549577257259853; INC(i);
      logFTail[i] :=  0.00000000000013912841212197565; INC(i);
      logFTail[i] :=  0.00000000000010775743037572640; INC(i);
      logFTail[i] :=  0.00000000000029391859187648000; INC(i);
      logFTail[i] := -0.00000000000042790509060060774; INC(i);
      logFTail[i] :=  0.00000000000022774076114039555; INC(i);
      logFTail[i] :=  0.00000000000010849569622967912; INC(i);
      logFTail[i] := -0.00000000000023073801945705758; INC(i);
      logFTail[i] :=  0.00000000000015761203773969435; INC(i);
      logFTail[i] :=  0.00000000000003345710269544082; INC(i);
      logFTail[i] := -0.00000000000041525158063436123; INC(i);
      logFTail[i] :=  0.00000000000032655698896907146; INC(i);
      logFTail[i] := -0.00000000000044704265010452446; INC(i);
      logFTail[i] :=  0.00000000000034527647952039772; INC(i);
      logFTail[i] := -0.00000000000007048962392109746; INC(i);
      logFTail[i] :=  0.00000000000011776978751369214; INC(i);
      logFTail[i] := -0.00000000000010774341461609578; INC(i);
      logFTail[i] :=  0.00000000000021863343293215910; INC(i);
      logFTail[i] :=  0.00000000000024132639491333131; INC(i);
      logFTail[i] :=  0.00000000000039057462209830700; INC(i);
      logFTail[i] := -0.00000000000026570679203560751; INC(i);
      logFTail[i] :=  0.00000000000037135141919592021; INC(i);
      logFTail[i] := -0.00000000000017166921336082431; INC(i);
      logFTail[i] := -0.00000000000028658285157914353; INC(i);
      logFTail[i] := -0.00000000000023812542263446809; INC(i);
      logFTail[i] :=  0.00000000000006576659768580062; INC(i);
      logFTail[i] := -0.00000000000028210143846181267; INC(i);
      logFTail[i] :=  0.00000000000010701931762114254; INC(i);
      logFTail[i] :=  0.00000000000018119346366441110; INC(i);
      logFTail[i] :=  0.00000000000009840465278232627; INC(i);
      logFTail[i] := -0.00000000000033149150282752542; INC(i);
      logFTail[i] := -0.00000000000018302857356041668; INC(i);
      logFTail[i] := -0.00000000000016207400156744949; INC(i);
      logFTail[i] :=  0.00000000000048303314949553201; INC(i);
      logFTail[i] := -0.00000000000071560553172382115; INC(i);
      logFTail[i] :=  0.00000000000088821239518571855; INC(i);
      logFTail[i] := -0.00000000000030900580513238244; INC(i);
      logFTail[i] := -0.00000000000061076551972851496; INC(i);
      logFTail[i] :=  0.00000000000035659969663347830; INC(i);
      logFTail[i] :=  0.00000000000035782396591276383; INC(i);
      logFTail[i] := -0.00000000000046226087001544578; INC(i);
      logFTail[i] :=  0.00000000000062279762917225156; INC(i);
      logFTail[i] :=  0.00000000000072838947272065741; INC(i);
      logFTail[i] :=  0.00000000000026809646615211673; INC(i);
      logFTail[i] := -0.00000000000010960825046059278; INC(i);
      logFTail[i] :=  0.00000000000002311949383800537; INC(i);
      logFTail[i] := -0.00000000000058469058005299247; INC(i);
      logFTail[i] := -0.00000000000002103748251144494; INC(i);
      logFTail[i] := -0.00000000000023323182945587408; INC(i);
      logFTail[i] := -0.00000000000042333694288141916; INC(i);
      logFTail[i] := -0.00000000000043933937969737844; INC(i);
      logFTail[i] :=  0.00000000000041341647073835565; INC(i);
      logFTail[i] :=  0.00000000000006841763641591466; INC(i);
      logFTail[i] :=  0.00000000000047585534004430641; INC(i);
      logFTail[i] :=  0.00000000000083679678674757695; INC(i);
      logFTail[i] := -0.00000000000085763734646658640; INC(i);
      logFTail[i] :=  0.00000000000021913281229340092; INC(i);
      logFTail[i] := -0.00000000000062242842536431148; INC(i);
      logFTail[i] := -0.00000000000010983594325438430; INC(i);
      logFTail[i] :=  0.00000000000065310431377633651; INC(i);
      logFTail[i] := -0.00000000000047580199021710769; INC(i);
      logFTail[i] := -0.00000000000037854251265457040; INC(i);
      logFTail[i] :=  0.00000000000040939233218678664; INC(i);
      logFTail[i] :=  0.00000000000087424383914858291; INC(i);
      logFTail[i] :=  0.00000000000025218188456842882; INC(i);
      logFTail[i] := -0.00000000000003608131360422557; INC(i);
      logFTail[i] := -0.00000000000050518555924280902; INC(i);
      logFTail[i] :=  0.00000000000078699403323355317; INC(i);
      logFTail[i] := -0.00000000000067020876961949060; INC(i);
      logFTail[i] :=  0.00000000000016108575753932458; INC(i);
      logFTail[i] :=  0.00000000000058527188436251509; INC(i);
      logFTail[i] := -0.00000000000035246757297904791; INC(i);
      logFTail[i] := -0.00000000000018372084495629058; INC(i);
      logFTail[i] :=  0.00000000000088606689813494916; INC(i);
      logFTail[i] :=  0.00000000000066486268071468700; INC(i);
      logFTail[i] :=  0.00000000000063831615170646519; INC(i);
      logFTail[i] :=  0.00000000000025144230728376072; INC(i);
      logFTail[i] := -0.00000000000017239444525614834; INC(i);
   END LogTables;

   (* === exported procedures ========================================== *)

   PROCEDURE arctan(x: REAL) : REAL;
   BEGIN
      RETURN atan2(x, 1.0)
   END arctan;

   PROCEDURE exp(x: REAL) : REAL;
      (* EXP(X)
       * RETURN THE EXPONENTIAL OF X
       * DOUBLE PRECISION (IEEE 53 bits, VAX D FORMAT 56 BITS)
       * CODED IN C BY K.C. NG, 1/19/85; 
       * REVISED BY K.C. NG on 2/6/85, 2/15/85, 3/7/85, 3/24/85, 4/16/85, 6/14/86.
       *
       * Required system supported functions:
       *      scalb(x,n)      
       *      copysign(x,y)   
       *      finite(x)
       *
       * Method:
       *      1. Argument Reduction: given the input x, find r and integer k such 
       *         that
       *                         x = k*ln2 + r,  |r| <= 0.5*ln2 .  
       *         r will be represented as r := z+c for better accuracy.
       *
       *      2. Compute exp(r) by 
       *
       *              exp(r) = 1 + r + r*R1/(2-R1),
       *         where
       *              R1 = x - x^2*(p1+x^2*(p2+x^2*(p3+x^2*(p4+p5*x^2)))).
       *
       *      3. exp(x) = 2^k * exp(r) .
       *
       * Special cases:
       *      exp(INF) is INF, exp(NaN) is NaN;
       *      exp(-INF)=  0;
       *      for finite argument, only exp(0)=1 is exact.
       *
       * Accuracy:
       *      exp(x) returns the exponential of x nearly rounded. In a test run
       *      with 1,156,000 random arguments on a VAX, the maximum observed
       *      error was 0.869 ulps (units in the last place).
       *
       * Constants:
       * The hexadecimal values are the intended ones for the following constants.
       * The decimal values may be used, provided that the compiler will convert
       * from decimal to binary accurately enough to produce the hexadecimal values
       * shown.
       *)
      CONST
	 p1 = 1.6666666666666601904E-1;
	 p2 = -2.7777777777015593384E-3;
	 p3 = 6.6137563214379343612E-5;
	 p4 = -1.6533902205465251539E-6;
	 p5 = 4.1381367970572384604E-8;
	 ln2hi = 6.9314718036912381649E-1;
	 ln2lo = 1.9082149292705877000E-10;
	 lnhuge = 7.1602103751842355450E2;
	 lntiny = -7.5137154372698068983E2;
	 invln2 = 1.4426950408889633870E0;
      VAR
	 z, hi, lo, c: REAL;
	 k: INTEGER;
   BEGIN (* exp *)
      IF x # x THEN
	 RETURN x (* x is NaN *)
      END;
      IF x <= lnhuge THEN
	 IF x >= lntiny THEN

	    (* argument reduction: x --> x - k * ln2 *)
	    k := TRUNC(invln2 * x + CopySign(0.5, x));

	    (* express x-k*ln2 as hi-lo and let x=hi-lo rounded *)
	    hi := x - FLOAT(k) * ln2hi;
	    lo := FLOAT(k) * ln2lo;
	    x := hi - lo;

	    (* return 2^k*[1+x+x*c/(2+c)] *)
	    z := x * x;
	    c := x - z*(p1+z*(p2+z*(p3+z*(p4+z*p5))));
	    RETURN Scalb(1.0+(hi-(lo-(x*c)/(2.0-c))),k);
	 ELSE
	    IF Finite(x) THEN
	       (* exp(-big#) underflows to zero *)
	       RETURN Scalb(1.0, -5000)
	    ELSE
	       (* exp(-INF) is zero *)
	       RETURN 0.0
	    END;
	 END;
      ELSE
	 (* exp(INF) is INF, exp(+big#) overflows to INF *)
	 IF Finite(x) THEN
	    RETURN Scalb(1.0, 5000)
	 ELSE
	    RETURN x
	 END;
      END;
   END exp;

   PROCEDURE ln(x: REAL) : REAL;
      (* Table-driven natural logarithm.
       *
       * This code was derived, with minor modifications, from:
       *      Peter Tang, "Table-Driven Implementation of the
       *      Logarithm in IEEE Floating-Point arithmetic." ACM Trans.
       *      Math Software, vol 16. no 4, pp 378-400, Dec 1990).
       *
       * Calculates log(2^m*F*(1+f/F)), |f/j| <= 1/256,
       * where F = j/128 for j an integer in [0, 128].
       *
       * log(2^m) = log2_hi*m + log2_tail*m
       * since m is an integer, the dominant term is exact.
       * m has at most 10 digits (for subnormal numbers),
       * and log2_hi has 11 trailing zero bits.
       *
       * log(F) = logF_hi[j] + logF_lo[j] is in tabular form in log_table.h
       * logF_hi[] + 512 is exact.
       *
       * log(1+f/F) = 2*f/(2*F + f) + 1/12 * (2*f/(2*F + f))**3 + ...
       * the leading term is calculated to extra precision in two
       * parts, the larger of which adds exactly to the dominant
       * m and F terms.
       * There are two cases:
       *      1. when m, j are non-zero (m | j), use absolute
       *         precision for the leading term.
       *      2. when m = j = 0, |1-x| < 1/256, and log(x) ~= (x-1).
       *         In this case, use a relative precision of 24 bits.
       * (This is done differently in the original paper)
       *
       * Special cases:
       *      0       return signalling -Inf
       *      neg     return signalling NaN
       *      +Inf    return +Inf
      *)
      CONST
	 n = 128;

	 (* Table of log(Fj) = logF_head[j] + logF_tail[j], for Fj = 1+j/128.
	  * Used for generation of extend precision logarithms.
	  * The constant 35184372088832 is 2^45, so the divide is exact.
	  * It ensures correct reading of logF_head, even for inaccurate
	  * decimal-to-binary conversion routines.  (Everybody gets the
	  * right answer for integers less than 2^53.)
	  * Values for log(F) were generated using error < 10^-57 absolute
	  * with the bc -l package.
	 *)
         A1 =      0.08333333333333178827;
         A2 =      0.01250000000377174923;
         A3 =     0.002232139987919447809;
         A4 =    0.0004348877777076145742;
      VAR
	 m, j: INTEGER;
	 F, f, g, q, u, u1, u2, v: REAL;

      PROCEDURE Truncate(x: REAL) : REAL;
	 VAR
	    ieee754: IEEE754;
      BEGIN
	 IEEE754Real2ExpMan(x, ieee754);
	 ieee754.mantissa1 :=
	    INTEGER(BITSET(ieee754.mantissa1) * BITSET(0F8000000H));
	 IEEE754ExpMan2Real(ieee754, x);
	 RETURN x
      END Truncate;

   BEGIN (* ln *)
      (* catch special cases *)
      IF x <= 0.0 THEN
	 IF x = 0.0 THEN
	    RETURN - hugeVal (* log(0) = -Inf *)
	 ELSE
	    RETURN nan       (* log(neg) = NaN *)
	 END;
      ELSIF ~Finite(x) THEN
	 RETURN x + x        (* x = NaN, Inf *)
      END;

      (* Argument reduction: 1 <= g < 2; x/2^m = g; *)
      (* y = F*(1 + f/F) for |f| <= 2^-8            *)
      m := TRUNC(LogB(x));
      g := LdExp(x, -m);
      IF m = - 1022 THEN
	 j := TRUNC(LogB(g)); INC(m, j);
	 g := LdExp(g, -j);
      END;
      j := TRUNC(FLOAT(n)*(g-1.0) + 0.5);
      F := (1.0 / FLOAT(n)) *
	       FLOAT(j) + 1.0; (* F*128 is an integer in [128, 512] *)
      f := g - F;

      (* Approximate expansion for log(1+f/F) ~= u + q *)
      g := 1.0 / (2.0 * F + f);
      u := 2.0 * f * g;
      v := u * u;
      q := u * v * (A1 + v*(A2 + v*(A3 + v*A4)));

      IF (m # 0) OR (j # 0) THEN
	 (* case 1: u1 = u rounded to 2^-43 absolute.  Since u < 2^-8,
		    u1 has at most 35 bits, and F*u1 is exact,
		    as F has < 8 bits.
		    It also adds exactly to |m*log2_hi + log_F_head[j] | < 750
	 *)
	 u1 := u + 513.0; u1 := u1 - 513.0;
      ELSE
	 (* case 2:  |1-x| < 1/256. The m- and j- dependent terms are zero;
	             u1 = u to 24 bits.
	 *)
	 u1 := u; u1 := Truncate(u1);
      END;
      u2 := (2.0*(f - F*u1) - u1*f) * g;

      u1 := u1 + FLOAT(m) * logFHead[n] + logFHead[j]; (* exact *)
      u2 := (u2 + logFTail[j]) + q;           (* tiny *)
      u2 := u2 + logFTail[n] * FLOAT(m);
      RETURN u1 + u2
   END ln;

   MODULE SinCos;

      IMPORT Rem, CopySign, Finite, half;
      EXPORT sin, cos;

      CONST
	 thresh = 2.6117239648121182150E-1;
	 PIo4 = 7.8539816339744827900E-1;
	 PIo2 = 1.5707963267948965580E0;
	 PI3o4 = 2.3561944901923448370E0;
	 PI = 3.1415926535897931160E0;
	 PI2 = 6.2831853071795862320E0;

	 S0 = -1.6666666666666463126E-1;
	 S1 = 8.3333333332992771264E-3;
	 S2 = -1.9841269816180999116E-4;
	 S3 = 2.7557309793219876880E-6;
	 S4 = -2.5050225177523807003E-8;
	 S5 = 1.5868926979889205164E-10;

	 C0 = 4.1666666666666504759E-2;
	 C1 = -1.3888888888865301516E-3;
	 C2 = 2.4801587269650015769E-5;
	 C3 = -2.7557304623183959811E-7;
	 C4 = 2.0873958177697780076E-9;
	 C5 = -1.1250289076471311557E-11;

	 small = 1.2E-8;
	 big = 1.0E20;

      PROCEDURE SinS(x: REAL) : REAL;
	 (* STATIC KERNEL FUNCTION OF SIN(X), COS(X), AND TAN(X)
	  * CODED IN C BY K.C. NG, 1/21/85;
	  * REVISED BY K.C. NG on 8/13/85.
	  *
	  *          sin(x*k) - x
	  * RETURN  --------------- on [-PI/4,PI/4] ,
	  *                  x
	  *
	  * where k=pi/PI, PI is the rounded
	  * value of pi in machine precision:
	  *
	  *      Decimal:
	  *              pi = 3.141592653589793 23846264338327 .....
	  *    53 bits   PI = 3.141592653589793 115997963 ..... ,
	  *    56 bits   PI = 3.141592653589793 227020265 ..... ,
	  *
	  *      Hexadecimal:
	  *              pi = 3.243F6A8885A308D313198A2E....
	  *    53 bits   PI = 3.243F6A8885A30  =  2 * 1.921FB54442D18
	  *    56 bits   PI = 3.243F6A8885A308 =  4 * .C90FDAA22168C2
	  *
	  * Method:
	  *      1. Let z=x*x. Create a polynomial approximation to
	  *          (sin(k*x)-x)/x  =  z*(S0 + S1*z^1 + ... + S5*z^5).
	  *      Then
	  *      sin__S(x*x) = z*(S0 + S1*z^1 + ... + S5*z^5)
	  *
	  *      The coefficient S's are obtained by a special Remez algorithm.
	  *
	  * Accuracy:
	  *      In the absence of rounding error, the approximation has absolute error
	  *      less than 2**(-61.11) for VAX D FORMAT, 2**(-57.45) for IEEE DOUBLE.
          *)
      BEGIN
	 RETURN x*(S0+x*(S1+x*(S2+x*(S3+x*(S4+x*S5)))))
      END SinS;

      PROCEDURE CosC(x: REAL) : REAL;
	 (*
	  * STATIC KERNEL FUNCTION OF SIN(X), COS(X), AND TAN(X)
	  * CODED IN C BY K.C. NG, 1/21/85;
	  * REVISED BY K.C. NG on 8/13/85.
	  *
	  *                          x*x
	  * RETURN   cos(k*x) - 1 + ----- on [-PI/4,PI/4],  where k = pi/PI,
	  *                           2
	  * PI is the rounded value of pi in machine precision :
	  *
	  *      Decimal:
	  *              pi = 3.141592653589793 23846264338327 .....
	  *    53 bits   PI = 3.141592653589793 115997963 ..... ,
	  *    56 bits   PI = 3.141592653589793 227020265 ..... ,
	  *
	  *      Hexadecimal:
	  *              pi = 3.243F6A8885A308D313198A2E....
	  *    53 bits   PI = 3.243F6A8885A30  =  2 * 1.921FB54442D18
	  *    56 bits   PI = 3.243F6A8885A308 =  4 * .C90FDAA22168C2
	  *
	  *
	  * Method:
	  *      1. Let z=x*x. Create a polynomial approximation to
	  *          cos(k*x)-1+z/2  =  z*z*(C0 + C1*z^1 + ... + C5*z^5)
	  *      then
	  *      cos__C(z) =  z*z*(C0 + C1*z^1 + ... + C5*z^5)
	  *
	  *      The coefficient C's are obtained by a special Remez algorithm.
	  *
	  * Accuracy:
	  *      In the absence of rounding error, the approximation has absolute error
	  *      less than 2**(-64) for VAX D FORMAT, 2**(-58.3) for IEEE DOUBLE.
	  *)
      BEGIN
	 RETURN x*x*(C0+x*(C1+x*(C2+x*(C3+x*(C4+x*C5)))))
      END CosC;

      PROCEDURE sin(x: REAL) : REAL;
	 VAR
	    a, c, z, tmp: REAL;
      BEGIN
	 IF ~Finite(x) THEN (* sin(NaN) and sin(INF) must be NaN *)
	    RETURN x - x
	 END;
	 x := Rem(x, PI2); (* reduce x into [-PI,PI] *)
	 a := CopySign(x, 1.0);
	 IF a >= PIo4 THEN
	    IF a >= PI3o4 THEN (* ... in [3PI/4,PI] *)
	       a := PI - a;
	       x := CopySign(a, x);
	    ELSE (* ... in [PI/4,3PI/4] *)
	       a := PIo2 - a; (* rtn. sign(x)*C(PI/2-|x|) *)
	       z := a * a;
	       c := CosC(z);
	       z := z * half;
	       IF z >= thresh THEN
		  a := half - ((z - half) - c);
	       ELSE
		  a := 1.0 - (z - c);
	       END;
	       RETURN CopySign(a, x)
	    END;
	 END;

	 IF a < small THEN (* return S(x) *)
	    tmp := big + a;
	    RETURN x
	 END;

	 RETURN x + x * SinS(x * x)
      END sin;

      PROCEDURE cos(x: REAL) : REAL;
	 VAR
	    a, c, z, s: REAL;
	    tmp: REAL;
      BEGIN
	 s := 1.0;
	 IF ~Finite(x) THEN  (* cos(NaN) and cos(INF) must be NaN *)
	    RETURN x - x
	 END;
	 x := Rem(x, PI2); (* reduce x into [-PI,PI] *)
	 a := CopySign(x, 1.0);
	 IF a >= PIo4 THEN
	    IF a >= PI3o4 THEN (* ... in [3PI/4,PI] *)
	       a := PI - a;
	       s := - 1.0;
	    ELSE               (* ... in [PI/4,3PI/4] *)
	       a := PIo2 - a;
	       RETURN a + a * SinS(a * a); (* rtn. S(PI/2-|x|) *)
	    END;
	 END;
	 IF a < small THEN
	    tmp := big + a;
	    RETURN s (* rtn. s*C(a) *)
	 END;
	 z := a * a;
	 c := CosC(z);
	 z := z * half;
	 IF z >= thresh THEN
	    a := half - ((z - half) - c);
	 ELSE
	    a := 1.0 - (z - c);
	 END;
	 RETURN CopySign(a, s)
      END cos;

   END SinCos;

   PROCEDURE sqrt(x: REAL) : REAL;
   BEGIN
      RETURN SQRT(x)
   END sqrt;

BEGIN
   hugeVal := MAX(REAL);
   hugeVal := hugeVal * 2.0; (* now +infinity on IEEE 754 machines *)
   half := 1.0 / 2.0;
   nan := ABS(SQRT(-1.0));
   IF ~IsInf(hugeVal) THEN HALT END;
   IF ~IsNaN(nan) THEN HALT END;
END MathLib.
