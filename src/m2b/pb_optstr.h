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
   $Id: pb_optstr.h,v 0.1 1997/02/24 15:51:45 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: pb_optstr.h,v $
   Revision 0.1  1997/02/24  15:51:45  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Struktur fuer die Optionen, mit deren Hilfe unabhaengig vom
 * speziellen Kommando die Argumente bearbeitet werden koennen.
 */

typedef struct {

	char	*opt_name;	/* der Name der Option */
	char	opt_type;	/* Typ der Option */
	int	opt_used;	/* Counter */
	int	opt_repl;	/* regelt mehrfache Benutzung */
	union {
		struct  {
			int	*addr;		/* bei Typ 'f' */
			int	dflt;
			} f;
		struct	{
			int	*arg_addr;	/* bei Typ 'i' */
			int	arg_dflt;
			int	arg_low;	/* untere Grenze */
			int	arg_high;	/* obere Grenze */
			} i;
		struct	{
			char	**str_addr;	/* bei Typ 's' */
			char	*str_dflt;
			} s;
		} opt_spez;
	} OPTSTR;
