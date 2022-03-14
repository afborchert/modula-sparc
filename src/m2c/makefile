#------------------------------------------------------------------------------
# Modula-2 driver -- makefile for Solaris
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 4/88
#------------------------------------------------------------------------------
# don't add comments or spaces to the macro definitions following!

# where to install m2c
BINDIR=/usr/local/bin

# default library directory
LIBDIR=/usr/local/lib/modula

# version number (3-6 chars)
VERSION=3.0b8

# URL of documentation
URL=http://www.mathematik.uni-ulm.de/modula/

#------------------------------------------------------------------------------
CC =		gcc
CFLAGS =	-DVERSION='"$(VERSION)"' -DLIBDIR='"$(LIBDIR)/"' \
		-DURL='"$(URL)"'
#------------------------------------------------------------------------------
.PHONY:		all
all:		m2c
m2c:		m2c.c makefile
		$(CC) -O $(CFLAGS) -o m2c m2c.c
#------------------------------------------------------------------------------
.PHONY:		install
install:	$(BINDIR)/m2c
$(BINDIR)/m2c:	m2c
		cp $? $@
#------------------------------------------------------------------------------
.PHONY:		clean
clean:		; rm -f m2c m2c.o PROT
