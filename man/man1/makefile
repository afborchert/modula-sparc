#------------------------------------------------------------------------------
# Makefile for Modula-2 system documentation
# (C) University of Ulm, SAI, 89069 Ulm
# afb 8/90
# rev afb 9/90: GNU make
# rev afb 9/90
#    avoid patsubst in association with ":=" assignment to
#    keep environment lists < 5kb
# rev afb 4/92: hierarchical makefiles
# rev afb 2/97: adapted to Modula-2 manual pages
#------------------------------------------------------------------------------

Chapter1 :=	$(shell echo *.1)
BinDir :=	../bin
MiscDir :=	../misc

InstallLib :=	/usr/local/lib/modula
InstallMan :=	/usr/local/man/modula
InstallHTML :=	/home/www/htdocs/modula/man/man1
Index :=	$(InstallHTML)/index.html

Release :=	3.0b8

#------------------------------------------------------------------------------
# installation
#------------------------------------------------------------------------------
Man1 :=		$(InstallMan)/man1
Cat1 :=		$(InstallMan)/cat1
Pages1 =	$(patsubst %,$(Man1)/%, $(Chapter1))
CatPages1 =	$(patsubst %,$(Cat1)/%, $(Chapter1))
WWWPages :=	$(patsubst %.1,$(InstallHTML)/%.html, $(Chapter1))

.PHONY:		install
#install:	$(Pages1) $(CatPages1) $(WWWPages) $(Index)
install:	$(WWWPages) $(Index)

$(Pages1):	$(Man1)/%: % makefile $(BinDir)/patch_macros
		rm -f $@
		$(BinDir)/patch_macros \
			-l $(InstallLib) \
			-m $(MiscDir)/macros \
			-r $(Release) \
			$< >$@
		chmod -f 664 $@

$(CatPages1):	$(Cat1)/%: $(Man1)/%
		rm -f $@
		groff -t -e -Tlatin1 -man $< >$@
		chmod -f 664 $@

$(WWWPages):	$(InstallHTML)/%.html: %.1
		rm -f $@
		$(BinDir)/man1tohtml $<
		chmod -f 664 $@

$(Index):	$(WWWPages)
		$(BinDir)/mk_index man1

#------------------------------------------------------------------------------
# cleanup
#------------------------------------------------------------------------------
.PHONY:		clean
clean:
		rcsclean $(Docs)
