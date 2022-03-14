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
   $Id: Functions.d,v 0.2 1997/02/28 15:50:06 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Functions.d,v $
   Revision 0.2  1997/02/28  15:50:06  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:03  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Functions; (* AFB 12/88 *)

(*
   (C) Andreas Borchert, Universitaet Ulm, 1988

   The expression grammar is Modula-2 oriented with following exceptions:

   (1)	The ?: operator has been added (semantic close to C).
	?: has lowest priority and right-to-left associativity
   (2)	Integer constants are real constants, too.
   (3)  Operator-keywords are not supported, so use
	   &  instead of AND,
	   |  instead of OR, and
	   ~  instead of NOT.
   (4)	TRUE is represented as 1.0 and FALSE as 0.0
	Anything but 0.0 is taken to be TRUE (in conditions).

   The grammar (in EBNF):

      CondExpression = Expression [ "?" CondExpression ":" CondExpression ] .
      Expression = SimpleExpression [ RelOp SimpleExpression ] .
      SimpleExpression = ["+"|"-"] Term { AddOp Term } .
      Term = Factor { MulOp Factor } .
      Factor = Constant | IDENT | FunctionCall | "~" Factor |
	       "(" CondExpression ")" .
      FunctionCall = IDENT "(" CondExpression [ "," CondExpression ] ")" .
      RelOp = "=" | "#" | "<" | ">" | "<=" | ">=" .
      AddOp = "+" | "-" | "|" .
      MulOp = "*" | "/" | "&" .

   The start symbol is CondExpression.
   Identifiers are sequences of letters and digits. The first character
   must be a letter.
   Syntax of constants:

      Digit { Digit } [ "." { Digit } ] [ ("E"|"e") Digit { Digit } ]

   On errors `ParseFunction' returns FALSE after `errpos' has been set 
   to the error position.
   Errors can result from

      (1) Syntax errors.
      (2) Bad constants, e.g. exponent is too large
      (3) Bad number of arguments to a function

   Each identifier not declared as a constant or a function is
   considered to be a parameter. The value of a parameter is
   predefined to 0.0. `FirstParam' and `NextParam' give the parameter names
   in alphabetical order. Parameter values can be changed using
   `SetFuncParam'. `EvalFunction' evaluates the function with the
   parameters set previously.

   Example:

      WriteString("func: "); ReadString(functxt);
      IF ParseFunction(func, functxt) THEN
	 FirstParam;
	 WHILE NextParam(func, parname) DO
	    WriteString(parname); WriteString(" = ");
	    ReadReal(val);
	    SetFuncParam(func, parname, val);
	 END;
	 WriteReal(EvalFunction(func), 1); WriteLn;
      ELSE
	 (* error at errpos *)
      END;

   Warning:

   EvalFunction does not check for division by zero or any other
   operations which can result in a floating point exception.

   Hint:

   Import `StdFuncs' for having a standard set of functions and
   constants.

*)

   TYPE
      Function;
      Real = REAL;
      StdFunc1 = PROCEDURE (Real) : Real;
      StdFunc2 = PROCEDURE (Real, Real) : Real;

   VAR
      errpos: CARDINAL; (* error position in expr of ParseFunction *)

   PROCEDURE InstallStdFunc1(funcname: ARRAY OF CHAR; stdfunc: StdFunc1);

   PROCEDURE InstallStdFunc2(funcname: ARRAY OF CHAR; stdfunc: StdFunc2);

   PROCEDURE InstallStdConst(constname: ARRAY OF CHAR; constval: Real);

   PROCEDURE ParseFunction(expr: ARRAY OF CHAR; VAR func: Function) : BOOLEAN;

   PROCEDURE FirstParam(func: Function);

   PROCEDURE NextParam(func: Function; VAR parname: ARRAY OF CHAR) : BOOLEAN;

   PROCEDURE SetFuncParam(func: Function; parname: ARRAY OF CHAR;
			  value: Real);

   PROCEDURE EvalFunction(func: Function) : Real;

   PROCEDURE DisposeFunction(VAR func: Function);
      (* release storage associated with `func' *)

END Functions.
