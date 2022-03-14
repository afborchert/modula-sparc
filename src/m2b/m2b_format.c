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
   $Id: m2b_format.c,v 0.1 1997/02/24 15:51:34 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_format.c,v $
   Revision 0.1  1997/02/24  15:51:34  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 *   Formatierroutinen ( Pass 1 ) :
 *
 *   aufgerufen vom Parser ( m2b_parser.c ) und vom
 *   Einlesemodul ( m2b_in.c ).
 *   Ausgabe in den 2.Pass ( m2b_pass2.c )
 */

#include	<stdio.h>
#include	<ctype.h>
#include	<stdlib.h>
#include	"pb_mac.h"
#include	"m2b_cmtstr.h"
#include	"m2b_strstr.h"

extern char * calloc();

extern void pass2_write();
extern void pass2_flushline();
extern void pass2_open_rge();
extern void pass2_close_rge();
extern void quit();
extern void fatal();
extern void warning();

extern void pushpos();
extern void poppos();
extern void mark_right();
extern void mark_left();
extern void flush_all();

extern void alloc_comment();
extern void flush_previous();
extern void single_flush();
static void read_options();

/*
 * Bemerkung : Spalten laufen hier immer ab 0 !
 */

#define	MAXDEPTH	100		/* max. Verschachtelungstiefe */

	int	formator_on;

static	int	pstack[MAXDEPTH];	/* Stack der Einrueckschritte */
static	int	stacktop = 0;		/* fuer obigen Stack */
static	char	zeile[BUFSIZ];		/* Zeilenbuffer */
static	char	*zeilenp;		/* fuer obigen Buffer */
static	int	column;			/* Ausgabespalte */

static	int	mark;			/* zu justierende Stelle */
static	int	free_col;			/* Spalte, ab der nur ' ' und Kom. */
static	int	cmt;			/* Spalte des hinteren Kommentar's */
static	int	pre;			/* Leerzeilen davor */
static	int	post;			/* Leerzeilen danach */
static	int	pre_allowed;		/* == FALSE in Kommentaren */

static	int	newrange;		/* == TRUE, falls pass2_rge_cl erw. */

static	int	wordcolumn;		/* Spalte das aktuelle Wort beginnt */
static	int	old_wordcolumn;		/* fuer justify_par() */
static	int	in_word = FALSE;	/* =1, falls wir in einem Wort sind */
extern	int	stringon;		/* siehe Einlesemodul pb_in.c */
extern	int	commenton;		/* - " - */
static	int	nomarks = FALSE;	/* bei `o'-Kommentaren im Buffer */

	int	linelength;		/* Ausgabebreite */
	int	step;			/* Einrueckweite */
	int	oflag;
	char	*o_ch = "|";
	int	hflag;
extern	int	pflag;
extern	int	vflag;
	int	comcolumn;

	/* zur Kommunikation mit pb_parser.c */

extern	int	keyword;

/* aus pb_strings.c importiert ... */

extern	char	buferr[];
extern	char	pusherr[];

/* zur Woerterbehandlung */

static	char	*left_ptr = NULL;
static	char	*right_ptr = NULL;

/*
 * putch() wird von getch() aufgerufen, die (fast) jeden Buchstaben,
 * den sie einliest hier weitergibt.
 * Bemerkung :
 * Ueberfluessige Blanks werden schon von getch() aussortiert.
 */

static	int	blankcounter = 0;

void putch ( ch ) 
char	ch;
{
	if ( zeilenp - zeile < BUFSIZ ) { 
		*zeilenp = ch;
		if ( in_word && !isalnum(ch) ) {
			in_word = FALSE;
			mark_right ( zeilenp - 1 );
			old_wordcolumn = wordcolumn;  /* fuer justify_par() */
			wordcolumn = 0;
			}
		else if ( !in_word && isalpha(ch) && !stringon && !commenton 
				&& formator_on ) {
			wordcolumn = zeilenp - zeile;
			mark_left ( zeilenp );
			in_word = TRUE;
			blankcounter = 0;
			}
		if ( !stringon && !commenton && isspace(ch) && formator_on )
			if ( blankcounter )
				return;	/* ueberfl. Blank ignorieren */
			else
				++blankcounter;
		else
			blankcounter = 0;
		++zeilenp;
		}
	else
		quit ( buferr );
}

/*
 * putline() wird vom Parser aufgerufen und bewirkt eine Ausgabe
 * eines '\n' und anschliessend des von putch() gefuellten Buffers
 * Bemerkung :
 * Mit putcomment() wird ein alter (im Buffer gesicherter) Kommentar
 * noch ausgegeben, der an das Ende einer Zeile sollte.
 * Leerzeilenpolitik wird hier nicht gemacht !
 */

void putline ()
{
	register	int	i;
	register	int	j;

	nomarks = FALSE;
	if ( !formator_on ) {
		*zeilenp = '\0';
		zeilenp = zeile;
		pass2_write(zeile);
		pass2_flushline ( -1 , 0 , 0 , 0 , 0 );
		return;
		}
	flush_all(); /* comments */
	pass2_flushline( mark , free_col , cmt , pre , post );
	if ( newrange ) {
		pass2_open_rge();
		newrange = FALSE;
		}
	if ( commenton )
		cmt = 0;
	else {
		pre_allowed = TRUE;
		cmt = -1;
		}
	mark = free_col = -1;
	pre = post = 0;
	column = pstack[stacktop];	/* ab hier Programmtext		*/
	j = 0;
	for ( i = 0 ; i < pstack[stacktop] ; ++i ) /* skip	*/
		if ( pstack[j] == i && oflag ) { /* Striche ?	*/
			++j;
			pass2_write ( o_ch );
			}
		else
			pass2_write ( " " );
	if ( zeilenp != zeile ) {	/* ist etwas auszugeben ?	*/
		*zeilenp = '\0';
		for ( zeilenp = zeile ; isspace(*zeilenp) ; ++zeilenp )
			;
		pass2_write ( zeilenp );
		column += strlen(zeilenp);
#ifdef TRACE
		if ( tflag )
			f_trace ( "putline" , column , zeilenp );
#endif
		zeilenp = zeile;
		}
}

/*
 * appendline() leert nur den Buffer,  o h n e  ein '\n' auszugeben
 * oder sich um Kommentare zu kuemmern.
 * Aussnahme :
 * die Zeile wird laenger als `linelength' erlaubt, dann geht es
 * davon aus ( vorausgesetzt newline_allowed == TRUE ), dass es
 * an der aktuellen Stelle brechen darf.
 */

void appendline( /* nline_allowed */ ) 
{
	int	nline_allowed;

	nline_allowed = TRUE;	/* here always allowed */
	nomarks = FALSE;
	if ( zeilenp == zeile )
		return;
	if ( nline_allowed && linelength &&
		linelength <= column + zeilenp - zeile ) {
		pushpos ( step );	/* beim Brechen einruecken !	*/
		putline();		/* doch ein '\n' ausgeben	*/
		poppos();		/* wieder ausruecken		*/
		}
	else {
		*zeilenp = '\0';
		pass2_write (zeile);
#ifdef TRACE
		if ( tflag )
			f_trace ( "appendline" , column , zeile );
#endif
		column += zeilenp - zeile;
		zeilenp = zeile;
		}
}

/*
 * der Parser darf nicht pass2_close_rge() aufrufen,
 * denn es muss gegebenenfalls noch ein Buffer geleert werden.
 */

void putrange()
{
	newrange = TRUE;
}

/*
 * da ja variabel ( per {%sN} ) eingerueckt werden kann und das
 * ausruecken analog geschehen soll, brauchen wir einen Stack.
 * Auf dem Stacktop steht jeweils, wie weit wir insgesamt ein-
 * ruecken muessen.
 * Der Stack wird ausschliesslich von pushpos() und poppos()
 * verwaltet und bei putstart() initialisiert.
 */

void pushpos ( pos ) 
int	pos;
{
	if ( stacktop >= MAXDEPTH - 1 )
		quit ( pusherr );
	else {
		++stacktop;
		pstack[stacktop] = pstack[stacktop-1] + pos;
		}
}

void poppos()
{
	if ( stacktop >= 1 )
		--stacktop;
	else
		fatal ("Stackunderflow bei poppos()");
}

/*
 * Initialisierung fuer die Formatierroutinen ( sollte die erste
 * Funktion dieses Moduls sein, die aufgerufen wird.
 */

void putstart()
{
	if ( hflag ) {			/* `header' gewuenscht ?? */
		cmt = 0;
		mark = -1;
		free_col = -1;
		pre = post = 0;
		pass2_write ("(********************************");
		pass2_flushline( mark , free_col , cmt , pre , post );
		pass2_write ("* MODULA - Beautifier           *");
		pass2_flushline( mark , free_col , cmt , pre , post );
		pass2_write ("* Version 1.00                  *");
		pass2_flushline( mark , free_col , cmt , pre , post );
		pass2_write ("* (c) Andreas Borchert     1983 *");
		pass2_flushline( mark , free_col , cmt , pre , post );
		pass2_write ("********************************)");
		pass2_flushline( mark , free_col , cmt , pre , post );
		pass2_close_rge();
		pass2_open_rge();
		}
	newrange = FALSE;
	pre_allowed = TRUE;
	cmt = mark = free_col = -1;
	zeilenp = zeile;
	column = 0;
	stacktop = 0;
	pstack[0] = 0;
}

/*
 * putcomment() gibt den letzten Kommentar einer Zeile aus
 * und setzt deswegen `free_col' und `comment' auf die richtigen
 * Werte. (wird von putline() und flush_xxx() aufgerufengerufen)
 * shift ist die Spalte, wo der Kommentar zu plazieren ist
 */

void putcomment ( buffer , shift )
char	*buffer;
int	shift;
{
	if ( shift < column ) {
		pass2_flushline ( mark , -1 , -1 , pre , post );
		column = 0;
		mark = -1;
		pre = post = 0;
		}
	free_col = column-1;
	for ( ; column < shift ; column++ )
		pass2_write ( " " );
	cmt = column;
	pass2_write (buffer);
	column += strlen(buffer);
}

/*
 * fuer ungetch() braucht man...
 */

void back(n)
int	n;
{
	if ( in_word ) {
		in_word = FALSE;
		left_ptr = right_ptr = NULL;
		}
	if ( zeilenp > zeile+n )
		zeilenp -= n;          
	else
		fatal("Illegal use of `back'");
}

/*
 * Zusaetzliche Leerzeilen
 */

void putspace()
{

	/*
	 * Spezialfall : keine Leerzeilen zusaetzlich in Kommentare
	 * einfuegen !
	 */

	if ( cmt >= 0 && pre_allowed )
		pre = post = 1;
	else
		post = 1;
}

void mark_left ( left )
char	*left;
{
	left_ptr = left;
	right_ptr = NULL;
}

void mark_right ( right )
char	*right;
{
	if ( /* right_ptr != NULL || */ left_ptr == NULL )
		fatal("Illegal call of `mark_right'");
	right_ptr = right;
}

void make_keyword ()
{
	int	under_line = 0;
	char	*ptr;

	if ( left_ptr == NULL || right_ptr <= left_ptr )
		fatal("Illegal call of `make_keyword'");
	for ( ptr = left_ptr ; ptr <= right_ptr ; ++ptr )
		if ( oflag )
			++under_line;
	if ( under_line ) {
		int	i;
		char	*rest;

		*zeilenp = '\0';
		if ( zeilenp+2*under_line+1 >= zeile+BUFSIZ )
			quit(buferr);
		column -= 2*under_line;
		if ( zeilenp-1 > right_ptr )
			if ( (rest = calloc(strlen(right_ptr),sizeof(char)))
				== NULL )
				quit("No space available.");
			else
				strcpy ( rest , right_ptr+1 );
		else
			rest = NULL;
		zeilenp = right_ptr+1;
		for ( i = 0 ; i < under_line ; ++i )
			*zeilenp++ = '\b';
		for ( i = 0 ; i < under_line ; ++i )
			*zeilenp++ = '_';
		if ( rest ) {
			strcpy ( zeilenp , rest );
			zeilenp += strlen ( rest );
			free ( rest );
			}
		}
	left_ptr = right_ptr = NULL;
}

/*
 * um ein spaeter zu justierendes Zeichen zu markieren.
 * Vorsicht ! Falls getsym() ein ' ' per ungetch()
 * zurueckgibt, wird es nicht tatsaechlich zurueckgegeben
 * (aus speziellen anderen Gruenden)
 */

void mark_ch ()
{
	if (nomarks)
		return;
	if ( *(zeilenp-1) == ' ' )
		mark = zeilenp - zeile - 2 + column;
	else
		mark = zeilenp - zeile - 1 + column; 
}

/*
 * um bei pflag == TRUE, alle Parameter exakt untereinander zu justieren,
 * braucht man folgendes, welches ein entsprechendes pushpos() ausfuehrt.
 * Es wird unmittelbar nach Erkennen des Wortes (des 1.Parameters) aufgerufen.
 */

void justify_par ()
{
	pushpos(old_wordcolumn+column-pstack[stacktop]);
}

/*
 * Hier kommt der Kommentarmodul
 * Er ist deswegen nicht allein, weil es einfacher ist wenn er direkt
 * in die Formatiervorgaenge eingreifen kann
 */

#define	GETARG(ptr,a)	(isdigit(*ptr)?sscanf(ptr,"%d",&a):FALSE)
#define NEWVALUE(a)	{ int _value; if ( GETARG(ptr,_value)==FALSE ) \
					warning("Illegal local option");\
				      else { a = _value;\
					for(++ptr;isdigit(*ptr);++ptr);\
					--ptr; } }
#define	NEWFLAG(a)	{ if (*ptr=='+') a = TRUE;\
			  else if (*ptr=='-') a = FALSE;\
			  else warning("Illegal local option"); }

#define	AC_MAX	10	/* Anzahl Kommentare, die mit Code vertauscht werden koennen */

static	char	*ac_field[AC_MAX];
static	int	ac_index = 0;

	char	local_option = '%';

void comment ( c_ptr )
CMTSTR	*c_ptr;
{
	register	char	*ptr;

	in_word = FALSE;

	/*
	 * zuerst untersuchen, ob sich eine lokale Option im
	 * Kommentar befindet.
	 */

	ptr = c_ptr->cs_start;
	if ( *(ptr + 2) == local_option && *ptr == '(' && *(ptr + 1) == '*' )
		read_options(ptr+3,c_ptr);

	/*
	 * entsprechend dem Kommentartyp ausgeben ( wenn
	 * ueberhaupt schon )
	 */

	alloc_comment ( c_ptr );
	switch ( c_ptr->cs_type ) {

	case 'l' :			/* linksbuendig */
	case 'a' :			/* aligned */
	case 'm' :			/* mehrzeiliger Kommentar */
	case 'C' :
		if ( ac_index > 1 )
			flush_previous();
		else if ( c_ptr->cs_type != 'm' )
			pass2_flushline ( mark , free_col , cmt , pre , post );
		if ( c_ptr->cs_type != 'm' ) {
			column = 0;
			mark = -1;
			free_col = -1;
			}
		pre = post = 0;
		if ( c_ptr->cs_type == 'a' )
			putcomment ( ac_field[0] , pstack[stacktop] );
		else if ( c_ptr->cs_type == 'm' ) {
			pre_allowed = FALSE;
			putcomment ( ac_field[0] , c_ptr->cs_column );
			}
		else if ( c_ptr->cs_type == 'C' )
			putcomment ( ac_field[0] , comcolumn );
		else
			putcomment ( ac_field[0] , 0 );
		ac_index = 0;
		free ( ac_field[0] );
		break;

	case 'c' :			/* in die Kommentarspalte */
		/* skip */
		break;

	case 'o' :			/* in den Kontext hinein */
		for ( ptr = c_ptr->cs_start ; ptr <= c_ptr->cs_end ; ++ptr )
			putch(*ptr);
		free(ac_field[ac_index-1]);
		--ac_index;
		nomarks = TRUE;
		break;

	default :
		fatal("Illegal type of comment : `%c'",c_ptr->cs_type);

	}
}

static
void read_options ( ptr , c_ptr )
char	*ptr;
CMTSTR	*c_ptr;
{
	do {
		switch ( *ptr++ ) {

		case 'c' :			/* Kommentarspalte */
			
			NEWVALUE(comcolumn)
			break;

		case 'l' :
			NEWVALUE(linelength)
			break;

		case 'p' :
			NEWFLAG(pflag)
			break;

		case 's' :
			NEWVALUE(step)
			break;

		case 'v' :
			NEWFLAG(vflag)
			break;

		case '-' :
			if ( !formator_on )
				warning("Formating has been turned off");
			putrange();
/*
			putline();
*/
			formator_on = FALSE;
			c_ptr->cs_type = 'a';
			--ptr;
			break;

		case '+' :
			putrange();
			formator_on = FALSE;
			putline();
			formator_on = TRUE;
			c_ptr->cs_type = 'a';
			mark = -1;
			pre = post = 0;
			--ptr;
			break;

#ifdef QUITOPTION
		case 'Q' :	/* liefert einen Dump */
#	ifdef DEBUG
			dump_all();
#	endif DEBUG
			fprintf(stderr,"Attention ! Remove tempfile after debugging !!\n");
			abort();
			break;
#endif

		default :

			warning("Illegal local option");
			break;

		}
		++ptr;
		}
	while ( *ptr++ == ';' );
}

void alloc_comment ( c_ptr )
CMTSTR	*c_ptr;
{
	int	size = c_ptr->cs_end - c_ptr->cs_start+2;
	char	*ptr1,*ptr2;

	if ( ac_index >= AC_MAX )
		single_flush();
	if ( (ac_field[ac_index] = calloc ( size+2 , sizeof(char) )) == NULL )
		quit("No space available");
	ptr2 = ac_field[ac_index];
	for ( ptr1 = c_ptr->cs_start ; ptr1 <= c_ptr->cs_end ; ++ptr1 )
		*ptr2++ = *ptr1;
	++ac_index;
}

void single_flush ()
{
	int	index;


	/*
	 * schwieriger Fall :
	 * ein Kommentar, der eigentlich spaeter am Zeilenende heraus-
	 * sollte wird zur Fruehgeburt.
	 */

	warning("Sorry: Layout isn't perfect here...");
	appendline();
	putcomment(ac_field[0],comcolumn);
	free(ac_field[0]);

	/*
	 * die anderen Kommentare ruecken entsprechend nach
	 */

	for ( index = 0 ; index < AC_MAX-1 ; ++index )
		ac_field[index] = ac_field[index+1];
	ac_index = AC_MAX-1;
}

void flush_previous ()
{
	int	index;

	if ( ac_index <= 1 )
		return;
	for ( index = 0 ; index < ac_index-1 ; ++index ) {
		putcomment(ac_field[index],comcolumn);
		free(ac_field[index]);
		pass2_flushline ( mark , free_col , cmt , pre , post );
		mark = -1;
		free_col = 0;
		cmt = comcolumn;
		column = 0;
		pre = post = 0;
		}
	ac_field[0] = ac_field[ac_index-1];
	ac_index = 1;
}

void flush_all ()
{
	if ( ac_index ) {
		flush_previous ();
		putcomment(ac_field[0],comcolumn);
		free(ac_field[0]);
		ac_index = 0;
		}
}

/*
 * hier wird putch() mit einem String gefuettert
 */

void string ( s_ptr )
STRSTR	*s_ptr;
{
	char	*ptr;

	in_word = FALSE;
	for ( ptr = s_ptr->ss_start ; ptr <= s_ptr->ss_end ; ++ptr )
		putch(*ptr);
}

/*
 * fuers entwanzen...
 */

#ifdef DEBUG

format_dump()
{
	register	int	i;

	fprintf(stderr,"pb-format.c :\n-------------\n");
	if ( zeilenp < zeile || zeilenp > zeile+BUFSIZ )
		fprintf(stderr,"zeilenp = undefined.\n");
	else {
		*zeilenp = '\0';
		fprintf(stderr,"zeile = `%s'\n");
		if ( in_word )
			fprintf(stderr,"in_word == TRUE\n");
		}
	fprintf(stderr,"column = %d\n",column);
	fprintf(stderr,"Allocated Comments :\n");
	for ( i = 0 ; i < ac_index ; ++i )
		fprintf(stderr,"%4d\t%s\n",i,ac_field[i]);
}

#endif DEBUG
