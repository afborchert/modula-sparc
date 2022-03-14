/* Ulm's Modula-2 System: Modula-2 Beautifier
   Copyright (C) 1983-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Beautifier is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   Ulm's Modula-2 Beautifier is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: m2b_cmtstr.h,v 0.1 1997/02/24 15:51:31 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_cmtstr.h,v $
   Revision 0.1  1997/02/24  15:51:31  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Struktur fuer Kommentare
 */

typedef	struct {

	char	*cs_start;	/* zeigt auf '(' */
	char	*cs_end;	/* zeigt auf ')' */
	char	cs_type;	/* Typ : 'l' = linksbuendig 
				         'a' = aligned
					 'c' = in die Kommentarspalte
					 'C' = analog, jedoch allein in d. Zeile
					 'o' = in den Kontext hinein
					 'm' = mehrzeiliger Kommentar */
	int	cs_column;	/* wo er in der Quelle stand */
	} CMTSTR;
