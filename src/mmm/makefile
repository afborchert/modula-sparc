# {updated by mmm: Sat Jul 15 19:11:33 2000 GMT}
# {flags: -a+ -e- -l- -L+ -m- -1- -v-}
#
BINDIR =	/usr/local/bin
LIBDIR =	/usr/local/lib/modula
# {rules}
.SUFFIXES:
.SUFFIXES:	.o .sy .m2 .d
MFLAGS =	
M2C =		m2c
.m2.o:
		$(M2C) -c $(MFLAGS) $*.m2
.d.a:
		$(M2C) $(MFLAGS) $*.d
# {macros}
SRC =		Errors.d Errors.m2 FileNames.d FileNames.m2 Library.d \
		Library.m2 Macros.d Macros.m2 Makefile.d Makefile.m2 \
		Options.d Options.m2 Out.d Out.m2 Scan.d Scan.m2 Sources.d \
		Sources.m2 Suffix.d Suffix.m2 SymTab.d SymTab.m2 TopSort.d \
		TopSort.m2 Update.d Update.m2 Write.d Write.m2 mmm.m2
TAGSRC =	Errors.m2 FileNames.m2 Library.m2 Macros.m2 Makefile.m2 \
		Options.m2 Out.m2 Scan.m2 Sources.m2 Suffix.m2 SymTab.m2 \
		TopSort.m2 Update.m2 Write.m2 mmm.m2
OBJS =		Errors.o FileNames.o Library.o Macros.o Makefile.o Options.o \
		Out.o Scan.o Sources.o Suffix.o SymTab.o TopSort.o Update.o \
		Write.o mmm.o
SYMS =		
REFS =		Errors.r FileNames.r Library.r Macros.r Makefile.r Options.r \
		Out.r Scan.r Sources.r Suffix.r SymTab.r TopSort.r Update.r \
		Write.r mmm.r
# {precious}
.PRECIOUS:	SYM
# {all}
T =		mmm
all:		$T
# {linkage}
LIBS =		
LDFLAGS =	
$T:		Errors.o FileNames.o Library.o Macros.o Makefile.o Options.o \
		Out.o Scan.o Sources.o Suffix.o SymTab.o TopSort.o Update.o \
		Write.o mmm.o
		$(M2C) $(MFLAGS) -o $@ $(LDFLAGS) Errors.o FileNames.o \
		Library.o Macros.o Makefile.o Options.o Out.o Scan.o \
		Sources.o Suffix.o SymTab.o TopSort.o Update.o Write.o mmm.o \
		$(LIBS)
# {dependencies}
SYM(Errors.sy):	Errors.d
		$(M2C) $(MFLAGS) Errors.d
Errors.o:	SYM(Errors.sy) /usr/local/lib/modula/SYM(FtdIO.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy)
SYM(FileNames.sy): FileNames.d /usr/local/lib/modula/SYM(StdIO.sy)
		$(M2C) $(MFLAGS) FileNames.d
FileNames.o:	SYM(Errors.sy) /usr/local/lib/modula/SYM(FtdIO.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(StrSpec.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SYM(FileNames.sy) \
		SYM(SymTab.sy)
SYM(Library.sy): Library.d
		$(M2C) $(MFLAGS) Library.d
Library.o:	/usr/local/lib/modula/SYM(Archive.sy) \
		/usr/local/lib/modula/SYM(Directories.sy) \
		/usr/local/lib/modula/SYM(Environment.sy) SYM(Errors.sy) \
		/usr/local/lib/modula/SYM(FtdIO.sy) SYM(Library.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(StrSpec.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysConf.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(FileNames.sy) \
		SYM(SymTab.sy) SYM(Suffix.sy)
SYM(Macros.sy):	Macros.d
		$(M2C) $(MFLAGS) Macros.d
Macros.o:	/usr/local/lib/modula/SYM(Environment.sy) SYM(Macros.sy) \
		SYM(Options.sy) SYM(Out.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Strings.sy)
SYM(Makefile.sy): Makefile.d
		$(M2C) $(MFLAGS) Makefile.d
Makefile.o:	/usr/local/lib/modula/SYM(Conversions.sy) SYM(Macros.sy) \
		SYM(Makefile.sy) SYM(Options.sy) SYM(Out.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SYM(FileNames.sy) \
		SYM(SymTab.sy) SYM(Write.sy) SYM(Suffix.sy)
SYM(Options.sy): Options.d
		$(M2C) $(MFLAGS) Options.d
Options.o:	SYM(Options.sy)
SYM(Out.sy):	Out.d
		$(M2C) $(MFLAGS) Out.d
Out.o:		/usr/local/lib/modula/SYM(ASCII.sy) SYM(Errors.sy) \
		/usr/local/lib/modula/SYM(FtdIO.sy) SYM(Out.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SYM(FileNames.sy)
SYM(Scan.sy):	Scan.d SYM(FileNames.sy) SYM(SymTab.sy)
		$(M2C) $(MFLAGS) Scan.d
Scan.o:		/usr/local/lib/modula/SYM(ASCII.sy) SYM(Errors.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysPerror.sy) SYM(FileNames.sy) \
		SYM(Scan.sy)
SYM(Sources.sy): Sources.d
		$(M2C) $(MFLAGS) Sources.d
Sources.o:	SYM(Errors.sy) /usr/local/lib/modula/SYM(InOut.sy) \
		SYM(Options.sy) SYM(Sources.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SYM(FileNames.sy) \
		SYM(SymTab.sy) SYM(Scan.sy)
SYM(Suffix.sy):	Suffix.d SYM(FileNames.sy) SYM(SymTab.sy)
		$(M2C) $(MFLAGS) Suffix.d
Suffix.o:	SYM(Errors.sy) /usr/local/lib/modula/SYM(Strings.sy) \
		SYM(FileNames.sy) SYM(SymTab.sy) SYM(Suffix.sy)
SYM(SymTab.sy):	SymTab.d SYM(FileNames.sy)
		$(M2C) $(MFLAGS) SymTab.d
SymTab.o:	SYM(Errors.sy) SYM(Options.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SYM(FileNames.sy) \
		SYM(SymTab.sy) SYM(Suffix.sy)
SYM(TopSort.sy): TopSort.d SYM(SymTab.sy)
		$(M2C) $(MFLAGS) TopSort.d
TopSort.o:	/usr/local/lib/modula/SYM(ASCII.sy) SYM(Errors.sy) \
		/usr/local/lib/modula/SYM(InOut.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) SYM(FileNames.sy) \
		SYM(SymTab.sy) SYM(TopSort.sy)
SYM(Update.sy):	Update.d
		$(M2C) $(MFLAGS) Update.d
Update.o:	/usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(Calendar.sy) SYM(Errors.sy) \
		SYM(Macros.sy) SYM(Makefile.sy) SYM(Options.sy) SYM(Out.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(StrSpec.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SYM(Update.sy) \
		SYM(FileNames.sy)
SYM(Write.sy):	Write.d SYM(FileNames.sy) SYM(SymTab.sy)
		$(M2C) $(MFLAGS) Write.d
Write.o:	SYM(Out.sy) /usr/local/lib/modula/SYM(Strings.sy) \
		SYM(FileNames.sy) SYM(SymTab.sy) SYM(TopSort.sy) \
		SYM(Write.sy)
mmm.o:		/usr/local/lib/modula/SYM(Arguments.sy) \
		/usr/local/lib/modula/SYM(Environment.sy) SYM(Errors.sy) \
		/usr/local/lib/modula/SYM(Files.sy) SYM(Library.sy) \
		SYM(Makefile.sy) SYM(Options.sy) SYM(Sources.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysConf.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) SYM(Update.sy) \
		SYM(FileNames.sy) SYM(SymTab.sy) SYM(TopSort.sy) \
		SYM(Suffix.sy)
#
.PHONY:		clean
clean:		; rm -f $(OBJS) $(SYMS) $(REFS)
#
.PHONY:		install
install:	$(BINDIR)/mmm $(LIBDIR)/mmm_proto
$(BINDIR)/mmm:	mmm
		cp $^ $@
$(LIBDIR)/mmm_proto: mmm_proto
		sed '1,/cut here/d' <$^ >$@
