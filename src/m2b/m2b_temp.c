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
   $Id: m2b_temp.c,v 0.1 1997/02/24 15:51:43 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_temp.c,v $
   Revision 0.1  1997/02/24  15:51:43  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Management des Tempfiles
 */

#include	<fcntl.h>
#include	<stdio.h>
#include	<unistd.h>
#include	"pb_mac.h"
#include	"pb_temp.h"

extern char * mktemp();

extern void fatal();
extern void quit();

static	FILE	*tmpfp;
static	TMPSTR	tmp_buf;			/* Ein Record des Tempfiles */
static	char	*tfname;		/* Name des Tempfiles */
static	int	topen = 0;		/* Selbstkontrolle */

/*
 * wird ein einziges Mal und vor allen anderen Routinen dieses
 * Moduls aufgerufen ( mit Kontrolle )
 */

void temp_open ()
{
	static char template[] = "/tmp/m2bXXXXXX";
	if ( topen )
		fatal("Illegal use of `temp_open'.");
	tfname = mktemp(template);
	ignore(close(creat(tfname,0600)));
	if ( (tmpfp=fopen(tfname,"w")) == NULL )
		quit("Can't allocate tempfile.");
	topen = TRUE;
}

/*
 * in der ersten Komponente der TMPSTR ist abgespeichert, wie viel
 * Platz die Struktur tatsaechlich benoetigt. Das ist wegen der
 * variablen Stringlaenge von tmp_buf.ts_line
 */

TMPSTR	*
t_read ()
{
	if ( !topen )
		fatal("Illegal use of `t_read'.");
	if ( fread(&tmp_buf.ts_recsize,sizeof(int),1,tmpfp) == 0 )
		return(NULL);
	if ( fread(&tmp_buf.ts_prelines,tmp_buf.ts_recsize,1,tmpfp) )
		return(&tmp_buf);
	else
		return(NULL);
}

void temp_write ( tmp_buf )
TMPSTR	*tmp_buf;
{
	if ( !topen )
		fatal("Illegal use of `temp_write'.");
	if ( fwrite(tmp_buf,tmp_buf->ts_recsize+sizeof(int),1,tmpfp) )
		return;
	else
		quit("Write error.");
}

void temp_rewind ( type )
char	*type;
{
	if ( !topen )
		fatal("Illegal use of `temp_rewind'.");
	if ( freopen ( tfname , type , tmpfp ) == NULL )
		quit("Read/Write Error");
}

void temp_close ()
{
	if ( !topen )
		fatal("Illegal use of `temp_close'.");
	topen = 0;
	ignore ( fclose ( tmpfp ) );
	ignore ( unlink ( tfname ) );
}

int temp_topen ()
{
	return ( topen );
}
