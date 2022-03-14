# {updated by mmm: Thu Feb 27 14:52:31 1997 GMT}
#
BINDIR =	/usr/local/bin
# {flags: -a- -e- -l- -L+ -m- -1- -v-}
# {rules}
.SUFFIXES:
.SUFFIXES:	.o .sy .m2 .d
MFLAGS =	
M2C =		m2c
.m2.o:
		$(M2C) -c $(MFLAGS) $*.m2
.d.sy:
		$(M2C) $(MFLAGS) $*.d
# {macros}
SRC =		Scan.d Scan.m2 SymTab.d SymTab.m2 mtags.m2
TAGSRC =	Scan.m2 SymTab.m2 mtags.m2
OBJS =		Scan.o SymTab.o mtags.o
SYMS =		Scan.sy SymTab.sy
REFS =		Scan.r SymTab.r mtags.r
# {all}
T =		mtags
all:		$T
# {linkage}
LIBS =		
LDFLAGS =	
$T:		Scan.o SymTab.o mtags.o
		$(M2C) $(MFLAGS) -o $@ $(LDFLAGS) Scan.o SymTab.o mtags.o \
		$(LIBS)
# {dependencies}
Scan.sy:	SymTab.sy
Scan.o:		/usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(FtdIO.sy) Scan.sy \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SymTab.sy \
		/usr/local/lib/modula/SYM(SysPerror.sy)
SymTab.sy:	
SymTab.o:	/usr/local/lib/modula/SYM(FtdIO.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SymTab.sy
mtags.o:	/usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(Arguments.sy) \
		/usr/local/lib/modula/SYM(FtdIO.sy) \
		/usr/local/lib/modula/SYM(PipeIO.sy) Scan.sy \
		/usr/local/lib/modula/SYM(StdIO.sy) SymTab.sy \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysPerror.sy)
#
tags:		$(TAGSRC)
		mtags $(TAGSRC)
#
.PHONY:		clean
clean:		; rm -f $(OBJS) $(SYMS) $(REFS)
#
.PHONY:		install
install:	$(BINDIR)/mtags
$(BINDIR)/mtags: mtags
		cp $^ $@
