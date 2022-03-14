(* Ulm's Modula-2 Compiler    Solaris 2.x/SPARCv8
   Copyright (C) 1983-1996 Universitaet Ulm, SAI, 89069 Ulm, Germany
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
   $Id: MCP4Expr.d,v 0.1 1997/02/21 18:40:13 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Expr.d,v $
   Revision 0.1  1997/02/21  18:40:13  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP4Expr; (* AFB 3/96 *)

   FROM MCBase IMPORT Label;
   FROM MCP4Attributes IMPORT Attribute, Reg;

   PROCEDURE ParseDesignator(VAR at: Attribute);
   PROCEDURE ParseExpression(VAR at: Attribute);

   PROCEDURE ParseAndGenDesignator(VAR at: Attribute);
   PROCEDURE ParseAndGenExpression(VAR at: Attribute; targetReg: Reg);
      (* targetReg may be noReg *)

   PROCEDURE GenExpr(VAR at: Attribute);
      (* generate code for the given attribute tree which has been
	 returned by ParseDesignator or ParseExpression
      *)

   PROCEDURE GenLogExpr(VAR at: Attribute; VAR trueLab, falseLab: Label);
      (* like GenExpr; result is in condMode;
	 if tlabel or flabel are set they equal trueLab and falseLab
      *)

END MCP4Expr.
