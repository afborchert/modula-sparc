#------------------------------------------------------------------------------
# Modula-2 runtime library -- makefile
# (c) University of Ulm, SAI, 89069 Ulm
# afb 4/88
#------------------------------------------------------------------------------
M4 :=		/usr/local/bin/m4
AS =		/usr/ccs/bin/as
ASFLAGS =	-P # -m

.s.a:		
		$(AS) -o $*.o $(ASFLAGS) $*.s
		ar rv $@ $*.o
		rm -f $*.o

.o.a:		
		ar rv $@ $*.o

LIB =		libm2.a
all:		$(LIB) m2rt0.o
$(LIB):		$(LIB)(rem.o) $(LIB)(urem.o) $(LIB)(signal.o) \
		$(LIB)(process.o) $(LIB)(rte.o)

#------------------------------------------------------------------------------
# following snippet has been taken out of the makefile of
# the GNU C Library, release 2.0.1, sub directory sysdeps/sparc
# note that we just need rem and urem, the compiler generates
# native code for the division
#------------------------------------------------------------------------------
# We distribute these files, even though they are generated,
# so as to avoid the need for a functioning m4 to build the library.
divrem := sdiv udiv rem urem

+divrem-NAME-sdiv := div
+divrem-NAME-udiv := udiv
+divrem-NAME-rem := rem
+divrem-NAME-urem := urem
+divrem-NAME = $(+divrem-NAME-$(basename $(notdir $@)))
+divrem-OP-div := div
+divrem-OP-udiv := div
+divrem-OP-rem := rem
+divrem-OP-urem := rem
+divrem-S-div := true
+divrem-S-rem := true
+divrem-S-udiv := false
+divrem-S-urem := false
$(divrem:%=%.s): divrem.m4
	(echo "define(NAME,\`.$(+divrem-NAME)')\
	       define(OP,\`$(+divrem-OP-$(+divrem-NAME))')\
	       define(S,\`$(+divrem-S-$(+divrem-NAME))')\
	       /* This file is generated from divrem.m4; DO NOT EDIT! */"; \
	 cat $<) | $(M4) > $@
#------------------------------------------------------------------------------

LibDir :=	/usr/local/lib/modula
.PHONY:		install
install:	$(LibDir)/$(LIB) $(LibDir)/m2rt0.o
$(LibDir)/$(LIB): $(LIB)
		cp $< $@

$(LibDir)/m2rt0.o: m2rt0.o
		cp $< $@
