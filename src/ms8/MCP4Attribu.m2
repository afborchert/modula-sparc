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
   $Id: MCP4Attribu.m2,v 0.1 1997/02/21 18:40:28 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Attribu.m2,v $
   Revision 0.1  1997/02/21  18:40:28  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4Attributes; (* AFB 2/89 *)

   FROM MCBase IMPORT Offset, Label, oneword, doubleword;
   FROM MCP4Global IMPORT Assert, Error;
   FROM Memory IMPORT ALLOCATE, DEALLOCATE;

   VAR
      unique: CARDINAL;

   PROCEDURE GetLabel(VAR l: Label);
      (* get label with unique `n1'; head = 0C and n2 = 0 *)
   BEGIN
      WITH l DO
	 ok := TRUE;
	 head := 0C;
	 n1 := unique; INC(unique);
	 n2 := 0;
      END;
   END GetLabel;

   PROCEDURE NewAttribute(VAR at: Attribute);
      (* NEW(at) *)
   BEGIN
      NEW(at);
      WITH at^ DO
	 link := NIL; attype := NIL; atip := NIL; isaddr := FALSE;
      END;
   END NewAttribute;

   PROCEDURE DisposeAttribute(VAR at: Attribute);
      (* DISPOSE(at); at := NIL; *)
   BEGIN
      DISPOSE(at);
      at := NIL;
   END DisposeAttribute;

   PROCEDURE Align(VAR offset: Offset);
      CONST
	 align = doubleword;
	 maxint = MAX(Offset);
	 minint = MIN(Offset);
   BEGIN
      IF ABS(offset) MOD align # 0 THEN
	 IF offset >= 0 THEN
	    IF offset > maxint-align THEN
	       Error(100);
	    ELSE
	       INC(offset, align - offset MOD align);
	    END;
	 ELSE
	    IF offset < minint+align THEN
	       Error(100);
	    ELSE
	       DEC(offset, align - ABS(offset) MOD align);
	    END;
	 END;
      END;
   END Align;

BEGIN
   unique := 1;
END MCP4Attributes.
