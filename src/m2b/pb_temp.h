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
   $Id: pb_temp.h,v 0.1 1997/02/24 15:51:47 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: pb_temp.h,v $
   Revision 0.1  1997/02/24  15:51:47  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Struktur des Tempfiles
 */

/*
 * Es ist unbedingt notwendig, dass ts_line die letzte Komponente der
 * Struktur ist. (Da aus Platzgruenden nicht der komplette Buffer in
 * die Tempfile geschrieben wird).
 * Entsprechend muss unbedingt ts_recsize die erste Komponente sein !
 * WICHTIG : ts_prelines  m u s s  die zweite Komponente sein !
 */

typedef struct {

	int	ts_recsize;		/* dyn. Groesse eines Records */
	int	ts_prelines;		/* Anz. Leerzeilen zuvor */
	int	ts_postlines;		/* Anz. Leerzeilen danach */
	int	ts_linelength;		/* entspricht akt. linelength */
	int	ts_mark;		/* Spalte der Markierung   	*/
	int	ts_free;		/* kleinstmoegl. Kommentarspalte */
	int	ts_comment;		/* Kommentarspalte */
	char	ts_line[BUFSIZ];	/* die Textzeile		*/

	} TMPSTR;
