(* Ulm's Modula-2 System: Makefile Generator
   Copyright (C) 1987-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Makefile Generator for Modula-2 is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   Ulm's Makefile Generator for Modula-2 is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY; without even the
   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: Macros.m2,v 0.1 1997/02/24 08:36:15 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Macros.m2,v $
   Revision 0.1  1997/02/24  08:36:15  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Macros; (* 3/87 *)

   FROM Environment IMPORT GetEnv;
   FROM Strings IMPORT StrCpy, StrCmp;
   FROM Out IMPORT WriteString, WriteLn, WriteTab;
   FROM Options IMPORT lookforenv;
   FROM Storage IMPORT ALLOCATE;

   TYPE
      Text = ARRAY [0..63] OF CHAR;
      MacroRef = POINTER TO Macro;
      Macro =
	 RECORD
	    name, val: Text;
	    link: MacroRef;
	 END;
   VAR
      macros: MacroRef;

   PROCEDURE EnterMacro(mname, mval: ARRAY OF CHAR);
      VAR new: MacroRef;
   BEGIN
      IF mval[0] <> 0C THEN
	 NEW(new);
	 WITH new^ DO
	    link := macros;
	    StrCpy(name, mname);
	    StrCpy(val, mval);
	 END;
	 macros := new;
      END;
   END EnterMacro;

   PROCEDURE WriteMacro(mname, default: ARRAY OF CHAR);
      VAR
	 ok: BOOLEAN;
	 text: Text;
	 mp: MacroRef;
   BEGIN
      WriteString(mname); WriteString(" ="); WriteTab;
      mp := macros;
      WHILE mp <> NIL DO
	 WITH mp^ DO
	    IF StrCmp(name, mname) = 0 THEN
	       WriteString(val); WriteLn;
	       RETURN
	    END;
	    mp := link;
	 END;
      END;
      IF lookforenv THEN
	 GetEnv(mname, text, ok);
      ELSE
	 ok := FALSE;
      END;
      IF ok THEN
	 WriteString(text);
      ELSE
	 WriteString(default);
      END;
      WriteLn;
   END WriteMacro;

BEGIN
   macros := NIL;
END Macros.
