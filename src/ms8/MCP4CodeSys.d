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
   $Id: MCP4CodeSys.d,v 0.1 1997/02/21 18:40:12 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4CodeSys.d,v $
   Revision 0.1  1997/02/21  18:40:12  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP4CodeSys; (* AFB 3/86 *)

   (* the only assembly output generating module;
      this interface does not depend on a particular assembler
   *)

   FROM MCBase IMPORT Ident, String, Label;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM SYSTEM IMPORT BYTE;

   PROCEDURE Emit(m: Mnemonic; s: ARRAY OF CHAR);
   PROCEDURE Emit1(m: Mnemonic; s: ARRAY OF CHAR; p1: ARRAY OF BYTE);
   PROCEDURE Emit2(m: Mnemonic; s: ARRAY OF CHAR; p1, p2: ARRAY OF BYTE);
   PROCEDURE Emit3(m: Mnemonic; s: ARRAY OF CHAR; p1, p2, p3: ARRAY OF BYTE);
   PROCEDURE Emit4(m: Mnemonic; s: ARRAY OF CHAR;
		   p1, p2, p3, p4: ARRAY OF BYTE);
   PROCEDURE Emit5(m: Mnemonic; s: ARRAY OF CHAR;
		   p1, p2, p3, p4, p5: ARRAY OF BYTE);
   PROCEDURE Emit6(m: Mnemonic; s: ARRAY OF CHAR;
		   p1, p2, p3, p4, p5, p6: ARRAY OF BYTE);
   PROCEDURE Emit7(m: Mnemonic; s: ARRAY OF CHAR;
                   p1, p2, p3, p4, p5, p6, p7: ARRAY OF BYTE);
   PROCEDURE Emit8(m: Mnemonic; s: ARRAY OF CHAR;
                   p1, p2, p3, p4, p5, p6, p7, p8: ARRAY OF BYTE);
   PROCEDURE Emit9(m: Mnemonic; s: ARRAY OF CHAR;
		   p1, p2, p3, p4, p5, p6, p7, p8, p9: ARRAY OF BYTE);
   PROCEDURE StrEmit(s: ARRAY OF CHAR);
   PROCEDURE StrEmit1(s: ARRAY OF CHAR; p1: ARRAY OF BYTE);
   PROCEDURE StrEmit2(s: ARRAY OF CHAR; p1, p2: ARRAY OF BYTE);
   PROCEDURE StrEmit3(s: ARRAY OF CHAR; p1, p2, p3: ARRAY OF BYTE);
   PROCEDURE StrEmit4(s: ARRAY OF CHAR; p1, p2, p3, p4: ARRAY OF BYTE);
   PROCEDURE StrEmit5(s: ARRAY OF CHAR; p1, p2, p3, p4, p5: ARRAY OF BYTE);
   PROCEDURE StrEmit6(s: ARRAY OF CHAR; p1, p2, p3, p4, p5, p6: ARRAY OF BYTE);
   PROCEDURE StrEmit7(s: ARRAY OF CHAR;
                   p1, p2, p3, p4, p5, p6, p7: ARRAY OF BYTE);
   PROCEDURE StrEmit8(s: ARRAY OF CHAR;
                   p1, p2, p3, p4, p5, p6, p7, p8: ARRAY OF BYTE);
   PROCEDURE StrEmit9(s: ARRAY OF CHAR;
		   p1, p2, p3, p4, p5, p6, p7, p8, p9: ARRAY OF BYTE);
      (* each Emit generates one line of output
	    exceptions: line numbers (in text segment only)
			definitions of real constants
	 permitted characters in strings and outside of %'s:
		     a-zA-Z0-9_,.
		     , separates operands
		     . is possibly translated to another character
		     (e.g. ~ or % on Nixdorf Targon/31)
	 inside comments (%*...) any printable character (except newline)
	 is permitted
	 composed/"selfmade" labels must be preceded by %_

	 control formats of `s':
	 %%   % (should not be used)
	 %a   attribute (Attributes.Attribute)
	 %A   set annull flag for this branch instruction
	 %:b(label,size)
	      define bss segment with the given label and size in bytes;
	      label and size may be arbitrary %-sequences
	 %c   CARDINAL value
	 %e   import (entry) definition (StrEmit); label follows
	 %g   export (global) definition (StrEmit); label follows
	 %H() high value; arbitrary %-sequences inside of () permitted
	 %i   INTEGER value
	 %l   label
	 %L() low value; arbitrary %-sequences inside of () permitted
	 %:l  define label (like EmitLabel) (followed by %'s for the label)
	 %:L  define long constant (followed by %'s for the value)
	 %m   module name (identifier component of MCBase.Ident)
	 %n   procedure label or global variable
	 %r   register (register direct)
	 %[]  inside [] one of
	      d
	      r
	      d,r
	      r,r		register indirect with index
	      %-sequences instead of `d' permitted
	 %s   string (ARRAY OF CHAR)
	 %S   label of string (MCBase.String)
	 %:s  define string (like EmitString); parameter: ARRAY OF CHAR
	 %*   treat rest of line as comment
	 %_   prefix for all labels (except MCBase.Label and string labels)
	 %=   define label (MCBase.Label); followed by anything (value)
	 %:D  define label; followed by anything for the label and ...
	 %:=  which should be followed by anything for the value
      *)

   (* pseudo operations *)

   TYPE
      Segment = (text, data, rodata);

   PROCEDURE SetSegment(s: Segment);

   PROCEDURE EmitAlign; (* 2-byte-alignment *)

   PROCEDURE EmitAlign4; (* 4-byte-alignment *)

   PROCEDURE EmitAlign8; (* 8-byte-alignment *)

   PROCEDURE EmitLabel(l: Label); (* define label *)

   PROCEDURE EmitString(s: String); (* define label and string *)

   PROCEDURE EmitHeader(modp: Ident); (* module header and key *)

   PROCEDURE EmitKey(extmodp: Ident); (* key reference to imported module *)

   (* generations of stabs and line numbers *)
   PROCEDURE EmitFileName(filename: ARRAY OF CHAR);
   PROCEDURE EmitBeginBlock1(blockp: Ident; line: CARDINAL);
   PROCEDURE EmitBeginBlock2(blockp: Ident; line: CARDINAL);
   PROCEDURE EmitEndBlock1(blockp: Ident; line: CARDINAL);
   PROCEDURE EmitEndBlock2(blockp: Ident; line: CARDINAL);

   PROCEDURE InitEmitCode;
      (* to be called at the beginning of the 4th pass *)

END MCP4CodeSys.
