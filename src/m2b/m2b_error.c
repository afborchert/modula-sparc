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
   $Id: m2b_error.c,v 0.1 1997/02/24 15:51:33 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_error.c,v $
   Revision 0.1  1997/02/24  15:51:33  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* Andreas Borchert 1983, Univrsity of Ulm */

/*
 * Diverse Fehlerausgaben/ausgaenge
 */

#include	<stdio.h>
#include	<signal.h>
#include	"pb_mac.h"

extern void quit();
extern void shut_down();
#ifdef DEBUG
extern void dump_all();
#endif

#define	MAXERROR	20	/* maximale Anzahl von `error'-Aufrufen */

	int	counter;	/* zaehlt die Fehler */

extern	int	lines_in;
extern	int	lines_out;
extern	int	zeilenanfang;
extern	char	*in_file;
extern	char	*usage_str;
	int	fatalerror = 0;

	char	buferr[] = { "Buffer overflow." };
	char	pusherr[] = { "Modularisize your program !!!" };

/* VARARGS1 */
void error ( controlstring , par1 , par2 )
char	*controlstring;
int	par1;
int	par2;
{
	if ( fatalerror )
		return;
	fprintf(stderr,"%s " , in_file);
	if ( lines_in ) {
		if ( zeilenanfang )/* ist das wirklich die Fehlerzeile ??? */
			fprintf(stderr,"(%4d) ",lines_in-1);
		else
			fprintf(stderr,"(%4d) ",lines_in);
		}
	fprintf(stderr,":");
	fprintf(stderr,controlstring,par1,par2);
	fprintf(stderr,"\n");
	++counter;
	if ( counter > MAXERROR )
		quit("Too many errors.");
}

/* VARARGS1 */
void fatal ( controlstring , par )
char	*controlstring;
int	par;
{
	if ( fatalerror )
		return;
	++fatalerror;
	fprintf(stderr,"Fatal error in m2b : ");
	fprintf(stderr,controlstring,par);
	fprintf(stderr,"\n");
	shut_down();
#ifdef DEBUG
	dump_all();
#endif
	abort();
}

/* VARARGS1 */
void quit ( controlstring , par1 , par2 )
char	*controlstring;
int	par1;
int	par2;
{
	if ( fatalerror )
		return;
	++fatalerror;
	fprintf(stderr,controlstring,par1,par2);
	fprintf(stderr," - QUIT\n");
	shut_down();
#ifdef	DEBUG
	dump_all();
	abort();
#endif
	exit(1);
}

/* VARARGS1 */
void warning ( controlstring , par1 , par2 )
char	*controlstring;
int	par1;
int	par2;
{
	if ( fatalerror )
		return;
	fprintf(stderr,"Warning : ");
	--counter;
	error(controlstring,par1,par2);
}

void usage ()
{
	if ( fatalerror )
		return;
	fprintf(stderr,"%s\n",usage_str);
	exit(1);		/* ohne shut_down, da usage nur am Anf. vork. */
}

void interrupt ()
{
	ignorf(signal ( SIGINT , SIG_IGN ));
	quit("m2b has been interrupted.");
}

void quitinterrupt ()
{
	ignorf(signal ( SIGQUIT , SIG_IGN ));
	fprintf(stderr,"Attention: Remove tempfile after debugging !!!");
	fprintf(stderr,"m2b: QUIT\n");
#ifdef DEBUG
	dump_all();
#endif
	abort();
}

void sigill ()
{
	ignorf(signal ( SIGILL , SIG_IGN ));
	fatal("Illegal instruction.");
}

void sigfpe()
{
	ignorf(signal ( SIGFPE , SIG_IGN ));
	fatal("Arithmetic fault.");
}

void sigbus()
{
	ignorf(signal ( SIGBUS , SIG_IGN ));
	fatal("Memory fault.");
}

void sigsegv()
{
	ignorf(signal ( SIGSEGV , SIG_IGN ));
	fatal("Segmentation violation.");
}

void sigpipe()
{
	ignorf(signal ( SIGPIPE , SIG_IGN ));
	/* stilles Ende */
	++fatalerror; /* damit kein Output mehr erzeugt wird */
	shut_down();
	if ( counter )
		exit(1);
	else
		exit(0);
}

void sigterm()
{
	ignorf(signal ( SIGTERM , SIG_IGN ));
	quit("m2b has been killed");
}


void error_start ()
{
	counter = 0;
#ifdef notdef
	ignorf(signal ( SIGHUP , SIG_IGN ));
	ignorf(signal ( SIGINT , interrupt ));
	ignorf(signal ( SIGQUIT , quitinterrupt ));
	ignorf(signal ( SIGILL , sigill ));
	ignorf(signal ( SIGFPE , sigfpe ));
	ignorf(signal ( SIGBUS , sigbus ));
	ignorf(signal ( SIGSEGV , sigsegv ));
	ignorf(signal ( SIGPIPE , sigpipe ));
	ignorf(signal ( SIGTERM , sigterm ));
#endif
}

int anz_errors ()
{
	return counter;
}
