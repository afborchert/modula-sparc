#------------------------------------------------------------------------------
# Modula-2 lister -- makefile for Solaris
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 2/97
#------------------------------------------------------------------------------
LIBDIR = 	/usr/local/lib/modula

OBJS   = 	m2e.o m2e_errnr.o m2e_source.o
CC     = 	gcc
CFLAGS = 	-DPRINT_USAGE -DSYSV -Wall

#------------------------------------------------------------------------------
.PHONY:		all
all:		m2e
m2e:		$(OBJS)
		$(CC) -o m2e $(OBJS)

m2e.o:		m2e_symbols.h
#------------------------------------------------------------------------------
.PHONY:		install
install:	m2e
		cp m2e $(LIBDIR)
#------------------------------------------------------------------------------
.PHONY:		clean
clean:		; rm -f $(OBJS) a.out core
