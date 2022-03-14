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
   $Id: m2b_main.c,v 0.1 1997/02/24 15:51:37 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_main.c,v $
   Revision 0.1  1997/02/24  15:51:37  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/*
 *   pb - PASCAL beautifier
 *
 *   (c) Andreas Borchert 1983, University of Ulm
 *
 *   Version 4.00
 *
 *   Sorry, the comments have been written in german.
 *
 */

#include	<stdio.h>
#include	<ctype.h>
#include	<sys/types.h>
#include	<sys/stat.h>

#include	"pb_optstr.h"
#include	"pb_symbols.h"
#include	"pb_mac.h"

extern int anz_errors();
extern void errnr_init();
extern void error_start();
extern void in_start();
extern void match_init();
extern int optinit();
extern void options();
extern void pass2_start();
extern void putstart();
extern void quit();
extern void read_to_eof();
extern void shut_down();
extern void ssym_init();
extern void unit();

char version[] =	{ "Version 4.00" };
char *usage_str =	{ "Usage: m2b [-c column] [-h] [-k] [-l length] [-o] [-p] [-s step] [-u] [-v] [infile [outfile] ]" };
char nulldev[] =	{ "/dev/null" };
char pb_name[] =	{ "m2b" };
extern	char *in_file;
extern	char *out_file;
extern	char	*word[];
extern	int	wsym[];
struct	stat	instat_buffer,outstat_buffer;

extern	OPTSTR	opttab[];

void main ( argc,argv ) 
int	argc;
char	**argv;
{

	int	tablen;
	FILE	*fp;
	

	error_start();
	
	/*
	 * die Options/Flags bearbeiten
	 */
	
	tablen = optinit();
	options ( argc , argv , opttab , tablen );

	/*
	 * nun die Dateien eroeffnen
	 */

	if ( in_file == NULL )
		fp = stdin;
	else if ( (fp = fopen(in_file,"r")) == NULL )
		quit("Can't open : %s ",in_file);
	if ( out_file && strcmp ( out_file , "" ) == 0 ) 
		out_file = nulldev;
	else if ( out_file && strcmp ( in_file , out_file ) == 0 && argc > 2 )
			quit("Don't destroy your source !!!");

	/* bei pb x > x ... */

	fstat ( fileno(fp) , &instat_buffer );
	fstat ( fileno(stdout) , &outstat_buffer );
	
	if ( ! ( instat_buffer.st_mode & S_IFCHR ) ) /* Terminal ? */
		if ( instat_buffer.st_dev == outstat_buffer.st_dev &&
		     instat_buffer.st_ino == outstat_buffer.st_ino ) {
			fclose ( fp );
			quit("Sorry...Please try it again ( retyping your source ). ");
			}
	
	/*
	 * initialisieren
	 */

	ssym_init();
	match_init(word,wsym,NORW);
	errnr_init();

	/*
	 * den Formatierer starten
	 */

	if ( fp != stdin )
		fclose(fp);
	in_start ( in_file );
	pass2_start( out_file );
	putstart();
	unit();
	read_to_eof();

	/*
	 * Schluss
	 */

	shut_down();
	if ( anz_errors() )
		exit(1);
}
