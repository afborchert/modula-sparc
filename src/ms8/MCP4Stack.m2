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
   $Id: MCP4Stack.m2,v 0.2 1998/04/23 18:08:37 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Stack.m2,v $
   Revision 0.2  1998/04/23  18:08:37  borchert
   debugging code added (StrEmit) that notes stack allocations
   and releases; this allows to check that stack areas are
   not released too early

   Revision 0.1  1997/02/21  18:40:33  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4Stack; (* AFB 4/89 *)

   (* allocation and deallocation of temporary stack space *)

   FROM MCP4Attributes IMPORT Align;
   FROM MCP4CodeSys IMPORT StrEmit2;
   FROM MCP4Global IMPORT Assert;
   FROM MCBase IMPORT Size, Offset, oneword, Direction, stackdir;
   FROM Memory IMPORT ALLOCATE;

   CONST
      blocksize = 32;
   TYPE
      Bitmap = SET OF Offset [0..blocksize-1];
      StackList = POINTER TO StackBlock;
      StackBlock =
	 RECORD
	    used: Bitmap;	(* used 4-byte-words *)
	    link: StackList;
	 END;
   VAR
      inblock: BOOLEAN;		(* check calling order *)
      startoffset: Offset;	(* set by StackOffset *)
      topoffset: Offset;	(* next free position *)
      head, tail: StackList;	(* list of bitmaps *)
      maplen: Offset;		(* length of bitmap in words *)

   PROCEDURE Set(offset, count: Offset; used: BOOLEAN);
      VAR
	 sp: StackList;
	 base: Offset;
	 mask, range: Bitmap;
	 low, high: Offset;
	 invmask: Bitmap;
   BEGIN
      IF used THEN
	 mask := Bitmap{0..blocksize-1};
	 invmask := Bitmap{};
      ELSE
	 mask := Bitmap{};
	 invmask := Bitmap{0..blocksize-1};
      END;
      sp := head; base := 0;
      WHILE base+blocksize <= offset DO
	 sp := sp^.link; INC(base, blocksize);
      END;
      WHILE count > 0 DO
	 IF offset > 0 THEN
	    low := offset MOD blocksize; offset := 0;
	 ELSE
	    low := 0;
	 END;
	 IF low+count >= blocksize THEN
	    high := blocksize - 1;
	 ELSE
	    high := low + count - 1;
	 END;
	 DEC(count, high + 1 - low);
	 range := Bitmap{low..high};
	 Assert(sp^.used * range = invmask * range);
	 sp^.used := sp^.used - range + mask * range;
	 sp := sp^.link;
      END;
   END Set;

   PROCEDURE StackAlloc(VAR offset: Offset; size: Size);
      (* `offset' is aligned *)
      VAR
	 sp: StackList;		(* from head to tail *)
	 base: Offset;		(* corresponds to `sp' *)
	 count: Offset;		(* of consecutive free words *)
	 offs: Offset;		(* current offset in bitmap-list *)
	 isize: Offset;         (* size argument converted to Offset *)

      PROCEDURE New;
      BEGIN
	 IF head = NIL THEN
	    NEW(head);
	    tail := head;
	 ELSE
	    NEW(tail^.link);
	    tail := tail^.link;
	 END;
	 WITH tail^ DO
	    used := Bitmap{};
	    link := NIL;
	 END;
	 INC(maplen, blocksize);
      END New;

   BEGIN (* StackAlloc *)
      isize := size;
      Align(isize); isize := isize DIV oneword;

      (* make sure that we have at least one element in list *)
      WHILE isize > maplen DO
	 New;
      END;

      (* look for `isize' consecutive free words *)
      sp := head; base := 0;
      offs := 0; count := 0;
      WHILE count < isize DO
	 IF base+blocksize <= offs THEN
	    IF sp^.link = NIL THEN New END;
	    sp := sp^.link; INC(base, blocksize);
	 END;
	 IF offs MOD blocksize IN sp^.used THEN
	    count := 0;
	 ELSE
	    INC(count);
	 END;
	 INC(offs);
      END;

      (* `offs' points now to the first word after the free area of
	 `isize' words
      *)
      DEC(offs, isize);
      Set(offs, isize, (* used = *) TRUE);
      IF stackdir = forward THEN
	 offset := startoffset + offs * oneword;
	 IF offset + isize > topoffset THEN
	    topoffset := offset + isize * oneword;
	 END;
      ELSE
	 offset := startoffset - (isize+offs) * oneword;
	 IF offset < topoffset THEN
	    topoffset := offset;
	 END;
      END;
      StrEmit2("%* StackAlloc: needed %c bytes, returned offset %i", size, offset);
   END StackAlloc;

   PROCEDURE StackFree(offset: Offset; size: Size);
      VAR
	 offs: Offset;
	 isize: Offset;
   BEGIN
      StrEmit2("%* StackFree: returned offset %i (size %c)", offset, size);
      isize := size;
      Align(isize); isize := isize DIV oneword;
      IF stackdir = forward THEN
	 offs := (offset - startoffset) DIV oneword;
      ELSE
	 offs := (startoffset - offset) DIV oneword - isize;
      END;
      Set(offs, isize, (* used = *) FALSE);
   END StackFree;

   PROCEDURE StackOffset(offset: Offset);
      (* called at block entry; `offset' is the offset for temporaries *)
   BEGIN
      Assert(~inblock); inblock := TRUE;
      startoffset := offset; topoffset := startoffset;
   END StackOffset;

   PROCEDURE StackUse(VAR size: Size);
      (* called at block exit; *)
      (* returns the (aligned) number of bytes used for temporaries *)
      VAR
	 sp: StackList;
   BEGIN
      Assert(inblock); inblock := FALSE;
      sp := head;
      WHILE sp # NIL DO
	 Assert(sp^.used = Bitmap{});
	 sp := sp^.link;
      END;
      size := ABS(topoffset-startoffset);
   END StackUse;

BEGIN
   inblock := FALSE;
   head := NIL; tail := NIL; maplen := 0;
END MCP4Stack.
