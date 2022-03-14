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
   $Id: m2b_match.c,v 0.1 1997/02/24 15:51:37 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_match.c,v $
   Revision 0.1  1997/02/24  15:51:37  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Routinen zur Schluesselworterkennung :
 */

#include	<stdio.h>
#include	<ctype.h>
#include	<stdlib.h>
#include	"pb_mac.h"
#include	"pb_match.h"

extern void quit();

static int hash();
static void insert();

#define	HASHCONST	271

static	HASHTAB	*table[HASHCONST];

/*
 * die erste aufzurufende Routine, die die Hashtabelle anlegt
 */

void match_init ( keywords , symbols , anzahl )
char	**keywords;
int	*symbols;		/* die dazugehoerigen Symbole */
int	anzahl;			/* der Schluesselwoerter */
{
	int	index;
	int	hashindex;

	for ( index = 0 ; index < HASHCONST ; ++index )
		table[index] = NULL;

	for ( index = 0 ; index < anzahl ; ++index ) {
		hashindex = hash(keywords[index]);
		insert(hashindex,keywords[index],symbols[index]);
		}
	/*
	 * fuer Test u. Optimierungsversuche gegebenenfalls
	 * die Hashtabelle ausgeben
	 */

#ifdef HASHDUMP
	hashdump(table);
#endif
}

/*
 * Einfuegung eines einzelnen Schluesselwortes bei bekanntem
 * Index der Hashtabelle
 */

static
void insert ( hashindex , keyword , symbol )
int	hashindex;
char	*keyword;
int	symbol;
{

	HASHTAB	*ptr;


	ptr = table[hashindex];
	if ( (table[hashindex] = (HASHTAB *) calloc ( (unsigned) 1 , sizeof(HASHTAB) )) == NULL )
		quit("No space available");
	table[hashindex]->ht_next = ptr;
	table[hashindex]->ht_keyword = keyword;
	table[hashindex]->ht_symbol = symbol;
}

/*
 * Bestimmung des Index der Hashtabelle eines bestimmten Wortes.
 * Die Schluesselwoerter sollten ziemlich breit auf der Tabelle
 * verteilt sein fuer eine effiziente Suche.
 */

static	int
hash ( word )
char	*word;
{
	int	length;
	register int val;

	length = strlen(word);
	val = (word[0]<<1) + (word[length-1]>>2) + length;
	if (val < 0)
		val = - val;
	return ( val % HASHCONST );
}

/*
 * match() liefert, falls `word' in der Tabelle eingetragen worden ist
 * sein Symbol, ansonsten 0
 */

int
match ( word )
char	*word;
{
	int	hashindex;
	HASHTAB	*ptr;

	hashindex = hash(word);
	for ( ptr = table[hashindex] ; ptr != NULL ; ptr = ptr->ht_next )
		if ( strcmp(ptr->ht_keyword,word) == 0 )
			return(ptr->ht_symbol);
	return(0);
}

#ifdef HASHDUMP

/*
 * um die Qualitaet der hash() Funktion zu kontrollieren :
 */

void hashdump(table)
HASHTAB	*table[];
{
	int	index;
	HASHTAB	*ptr;

	for ( index = 0 ; index < HASHCONST ; ++index ) {
		printf("%4d | ",index);
		for ( ptr = table[index] ; ptr != NULL ; ptr = ptr->ht_next )
			printf("%-10s ",ptr->ht_keyword);
		printf("\n");
		}
}
#endif
