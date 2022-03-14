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
   $Id: m2b_optinit.c,v 0.1 1997/02/24 15:51:38 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_optinit.c,v $
   Revision 0.1  1997/02/24  15:51:38  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Initialisierung der Optionstabelle
 */
#include	<assert.h>
#include	<stdio.h>
#include	"pb_mac.h"
#include	"pb_optstr.h"
#include	"pb_opttab.h"

#define DIM(vec)  (sizeof(vec)/sizeof(vec[0]))

/*
 * die Flags/Variablen, die von den Optionen beeinflusst werden
 */

extern	char	*pb_name;
extern	int	comcolumn;
extern	int	hflag;
extern	int	linelength;
extern	int	oflag;
extern	int	pflag;
extern	int	step;
extern	int	vflag;
extern	char	*in_file;
extern	char	*out_file;

/*
 * der Init des `union'-Teils der OPTSTR ( geht n i c h t per
 * Initialisizer ! )
 */

int optinit ()
{	int i = 0;
	opttab[i].opt_spez.s.str_addr = &pb_name;
	opttab[i].opt_spez.s.str_dflt = "pb";
	++ i;
	opttab[i].opt_spez.i.arg_addr = &comcolumn;
	opttab[i].opt_spez.i.arg_dflt = 40;
	opttab[i].opt_spez.i.arg_low  = 10;
	opttab[i].opt_spez.i.arg_high = 140;
	++ i;
	opttab[i].opt_spez.f.addr     = &hflag;
	opttab[i].opt_spez.f.dflt     = FALSE;
	++ i;
	opttab[i].opt_spez.i.arg_addr = &linelength;
	opttab[i].opt_spez.i.arg_dflt = 79;
	opttab[i].opt_spez.i.arg_low  = 0;
	opttab[i].opt_spez.i.arg_high = 140;
	++ i;
	opttab[i].opt_spez.f.addr     = &oflag;
	opttab[i].opt_spez.f.dflt     = FALSE;
	++ i;
	opttab[i].opt_spez.f.addr     = &pflag;
	opttab[i].opt_spez.f.dflt     = FALSE;
	++ i;
	opttab[i].opt_spez.i.arg_addr = &step;
	opttab[i].opt_spez.i.arg_dflt = 3;
	opttab[i].opt_spez.i.arg_low  = 0;
	opttab[i].opt_spez.i.arg_high = 20;
	++ i;
	opttab[i].opt_spez.f.addr     = &vflag;
	opttab[i].opt_spez.f.dflt     = FALSE;
	++ i;
	opttab[i].opt_spez.s.str_addr= &in_file;
	opttab[i].opt_spez.s.str_dflt= NULL;
	++ i;
	opttab[i].opt_spez.s.str_addr= &out_file;
	opttab[i].opt_spez.s.str_dflt= NULL;
	++ i;
	assert(i == DIM(opttab));
	return i;
}
