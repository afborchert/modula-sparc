#------------------------------------------------------------------------------
# Modula-2 Beautifier -- makefile for Solaris
# (c) Andreas Borchert 1983, University of Ulm
#------------------------------------------------------------------------------
BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib/modula
ERROR=$(LIBDIR)/m2_error

CC=gcc
CFLAGS=-g -O2 -DSYSV -DERROR=\"$(ERROR)\" -W
LDFLAGS=

OBJS=	m2b_debug.o m2b_errnr.o m2b_error.o m2b_format.o m2b_ignore.o \
	m2b_in.o m2b_main.o m2b_match.o m2b_optinit.o m2b_options.o \
	m2b_parser.o m2b_pass2.o m2b_shutdown.o m2b_ssyminit.o m2b_temp.o
#------------------------------------------------------------------------------
.PHONY:		all
all:		m2b
m2b:		$(OBJS)
		$(CC) -o m2b $(CFLAGS) $(LDFLAGS) $(OBJS)
#------------------------------------------------------------------------------
.PHONY:		install
install: 	$(BINDIR)/m2b
$(BINDIR)/m2b:	m2b
		cp $< $@
#------------------------------------------------------------------------------
.PHONY:		clean
clean:		rm -f $(OBJS) m2b a.out core out
