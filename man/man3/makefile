#------------------------------------------------------------------------------
# Makefile for Modula-2 system documentation
# (c) University of Ulm, SAI, D-89173 Ulm
# afb 8/90
# rev afb 9/90: GNU make
# rev afb 9/90
#    avoid patsubst in association with ":=" assignment to
#    keep environment lists < 5kb
# rev afb 4/92: hierarchical makefiles
# rev afb 2/97: adapted to Modula-2 manual pages
#------------------------------------------------------------------------------

Chapter3 :=	$(shell echo *.3)
BinDir :=	../bin
MiscDir :=	../misc

InstallLib :=	/usr/local/lib/modula
InstallMan :=	/usr/local/man/modula
InstallHTML :=	/home/www/htdocs/modula/man/man3
Index :=	$(InstallHTML)/index.html

Release :=	3.0b8

#------------------------------------------------------------------------------
# installation
#------------------------------------------------------------------------------
Man3 :=		$(InstallMan)/man3
Cat3 :=		$(InstallMan)/cat3
Pages3 =	$(patsubst %,$(Man3)/%, $(Chapter3))
CatPages3 =	$(patsubst %,$(Cat3)/%, $(Chapter3))
WWWPages :=	$(patsubst %.3,$(InstallHTML)/%.html, $(Chapter3))

.PHONY:		install
#install:	$(Pages3) $(CatPages3) $(WWWPages) $(Index)
install:	$(WWWPages) $(Index)

$(Pages3):	$(Man3)/%: % makefile $(BinDir)/patch_macros
		rm -f $@
		$(BinDir)/patch_macros \
			-l $(InstallLib) \
			-m $(MiscDir)/macros \
			-r $(Release) \
			$< >$@
		chmod -f 664 $@

$(CatPages3):	$(Cat3)/%: $(Man3)/%
		rm -f $@
		groff -t -e -Tlatin1 -man $< >$@
		chmod -f 664 $@

$(WWWPages):	$(InstallHTML)/%.html: %.3
		rm -f $@
		$(BinDir)/man3tohtml $<
		chmod -f 664 $@

$(Index):	$(WWWPages)
		$(BinDir)/mk_index man3

#------------------------------------------------------------------------------
# cleanup
#------------------------------------------------------------------------------
.PHONY:		clean
clean:
		rcsclean $(Docs)
