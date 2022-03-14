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
   $Id: MCP4Types.m2,v 0.1 1997/02/21 18:40:34 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Types.m2,v $
   Revision 0.1  1997/02/21  18:40:34  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCP4Types;        (* AFB 9/83 *)
                                        (* REV AFB 5/84: PROCEDURE ByteSize *)

   FROM MCBase IMPORT Stptr, Stset, charptr, boolptr, intptr, cardptr, realptr,
      bitsetptr, addrptr, Structform, maxint, onebyte, oneword, doubleword,
      longintptr, longcardptr, longrealptr, Size;
   FROM MCP4Attributes IMPORT Attribute, ArithmeticType;
   FROM MCP4Global IMPORT Assert;
   FROM Storage IMPORT ALLOCATE;

   PROCEDURE ByteSize(fsp: Stptr): BOOLEAN;
      CONST 
         byteSize = Stset{bools, bytes, chars};
   BEGIN 
      WITH fsp^ DO 
         RETURN (form IN byteSize) OR (form = subranges) AND ByteSize(scalp)
      END;
   END ByteSize;

   PROCEDURE IsSetType(fsp:Stptr): BOOLEAN;
   BEGIN 
      RETURN fsp^.form IN Stset{sets, bigsets}
   END IsSetType;

   PROCEDURE IsArrayType(fsp:Stptr): BOOLEAN;
   BEGIN 
      RETURN fsp^.form=arrays 
   END IsArrayType;

   PROCEDURE IsDyn(ft: Stptr) : BOOLEAN;
   BEGIN
      WITH ft^ DO
	 RETURN (form = arrays) & dyn
      END;
   END IsDyn;

   PROCEDURE SizeType(at: Attribute): Size;
   (* Return the size of the entity given by at *)
   BEGIN 
      WITH at^ DO 
         RETURN attype^.size 
      END;
   END SizeType;

   PROCEDURE BaseType(ft: Stptr): Stptr;
      VAR 
         int, car, real, longreal: BOOLEAN;
   BEGIN 
      WITH ft^ DO 
         CASE ft^.form OF 
         | subranges: 
               RETURN BaseType(scalp);
         | longints: 
               RETURN intptr;
         | longcards: 
               RETURN cardptr;
         | setoftypes: 
               int := Stset{longints, ints} * typeset # Stset{};
               car := Stset{longcards, cards} * typeset # Stset{};
               real := reals IN typeset;
               longreal := longreals IN typeset;
	       IF longreal THEN
		  RETURN longrealptr
	       END;
               IF real THEN 
                  RETURN realptr 
               END;
               IF int AND car THEN 
		  IF intcarptr = NIL THEN Init END;
                  RETURN intcarptr 
               END;
               IF int THEN 
                  RETURN intptr 
               END;
               IF car THEN 
                  RETURN cardptr 
               END;
               Assert(FALSE);
         ELSE 
            RETURN ft
         END;
      END;
   END BaseType;

   PROCEDURE TestBaseType(ft: Stptr): Stptr;
   BEGIN 
      RETURN BaseType(ft)
   END TestBaseType;

   PROCEDURE Signed(ft: Stptr) : BOOLEAN;
      VAR
	 btype: Stptr;
   BEGIN
      btype := BaseType(ft);
      IF intcarptr = NIL THEN Init END;
      RETURN (btype = intptr) OR (btype = intcarptr)
   END Signed;

   PROCEDURE Unsigned(ft: Stptr) : BOOLEAN;
      VAR
	 btype: Stptr;
   BEGIN
      btype := BaseType(ft);
      IF intcarptr = NIL THEN Init END;
      RETURN (btype = charptr) OR (btype = boolptr) OR
             (btype = cardptr) OR (btype = intcarptr) OR (btype = addrptr)
   END Unsigned;

   PROCEDURE IsNumeric(ft: Stptr) : BOOLEAN;
      VAR
	 baseType: Stptr;
   BEGIN
      baseType := BaseType(ft);
      RETURN ft^.form IN Stset{
	 setoftypes,
	 ints, longints, cards, longcards,
	 reals, longreals
	 };
   END IsNumeric;

   PROCEDURE IsReal(ft: Stptr) : BOOLEAN;
   BEGIN
      WITH ft^ DO
	 RETURN (form IN Stset{reals, longreals}) OR
		(form = setoftypes) &
		(Stset{reals, longreals} * typeset # Stset{})
      END;
   END IsReal;

   PROCEDURE ResultType(at1, at2: Attribute): Stptr;
      VAR 
         ft1, ft2: Stptr;
   BEGIN 
      ft1 := BaseType(at1^.attype);
      ft2 := BaseType(at2^.attype);
      IF (ft1=intptr) OR (ft2=intptr) THEN RETURN intptr END;
      IF (ft1=cardptr) OR (ft2=cardptr) THEN RETURN cardptr END;
      IF ft1=boolptr THEN RETURN boolptr END;
      IF ft1^.form=enums THEN RETURN intptr END;
      IF (ft1=addrptr) OR (ft2=addrptr) THEN RETURN cardptr END;
      IF ft1^.form = bigsets THEN RETURN ft1 END;
      IF IsSetType(ft1) THEN RETURN bitsetptr END;
      IF (ft1 = realptr) OR (ft2 = realptr) THEN RETURN realptr END;
      IF (ft1 = longrealptr) OR (ft2 = longrealptr) THEN RETURN longrealptr END;
      (* IF ft1^.form = hides THEN RETURN intptr END; *)
      IF intcarptr = NIL THEN Init END;
      RETURN intcarptr 
   END ResultType;

   PROCEDURE ArithType(type: Stptr) : ArithmeticType;
   BEGIN
      IF intcarptr = NIL THEN Init END;
      type := BaseType(type);
      IF type = cardptr THEN RETURN cardAT END;
      IF (type = intptr) OR (type = longintptr) THEN RETURN intAT END;
      IF type = intcarptr THEN RETURN cardAT END;
      (*IF type = charptr THEN RETURN cardAT END;*)
      IF type = boolptr THEN RETURN logAT END;
      IF type = bitsetptr THEN RETURN bitAT END;
      IF type^.form = bigsets THEN RETURN bitAT END;
      IF type = realptr THEN RETURN float64AT END;
      IF type = longrealptr THEN RETURN float128AT END;
      RETURN cardAT 
   END ArithType;

   PROCEDURE Arithmetic(at1, at2: Attribute): ArithmeticType;
      VAR 
         tptr: Stptr;
   BEGIN 
      tptr := ResultType(at1, at2);
      RETURN ArithType(tptr)
   END Arithmetic;

   PROCEDURE SimpleType(at: Attribute): BOOLEAN;
   (* Returns at describes an simple typed attribut *)
   BEGIN 
      WITH at^.attype^ DO 
         RETURN (size <= doubleword) AND (form # arrays) AND (form # records)
      END 
   END SimpleType;

   PROCEDURE Init;
   BEGIN
      NEW(intcarptr);
      intcarptr^ := cardptr^;
   END Init;

BEGIN
   intcarptr := NIL;
END MCP4Types. 
