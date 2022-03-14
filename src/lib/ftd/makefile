# {updated by mmm: Thu Feb 27 14:51:52 1997 GMT}
# {flags: -a+ -e- -l+ -L+ -m- -1- -v-}
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
SRC =		Bytes.d Bytes.m2 Chars.d Chars.m2 E.d E.m2 EditWindow.d \
		EditWindow.m2 F.d F.m2 Keywords.d Keywords.m2 L.d L.m2 \
		LongStrings.d LongStrings.m2 OutLines.d OutLines.m2 P.d P.m2 \
		Printf.d Printf.m2 S.d S.m2 Stack.d Stack.m2 W.d W.m2
TAGSRC =	Bytes.m2 Chars.m2 E.m2 EditWindow.m2 F.m2 Keywords.m2 L.m2 \
		LongStrings.m2 OutLines.m2 P.m2 Printf.m2 S.m2 Stack.m2 W.m2
OBJS =		
SYMS =		
REFS =		Bytes.r Chars.r E.r EditWindow.r F.r Keywords.r L.r \
		LongStrings.r OutLines.r P.r Printf.r S.r Stack.r W.r
# {all}
LIB =		libm2.a
all:		$(LIB)
# {linkage}
LIBS =		
LDFLAGS =	
# {dependencies}
$(LIB):		$(LIB)(Bytes.o) $(LIB)(Chars.o) $(LIB)(E.o) \
		$(LIB)(EditWindow.o) $(LIB)(F.o) $(LIB)(Keywords.o) \
		$(LIB)(L.o) $(LIB)(LongStrings.o) $(LIB)(OutLines.o) \
		$(LIB)(P.o) $(LIB)(Printf.o) $(LIB)(S.o) $(LIB)(Stack.o) \
		$(LIB)(W.o)
SYM(Bytes.sy):	Bytes.d
		$(M2C) $(MFLAGS) Bytes.d
$(LIB)(Bytes.o): SYM(Bytes.sy) Bytes.m2
		$(M2C) $(MFLAGS) -c Bytes.m2
		ar rv $(LIB) Bytes.o && rm -f Bytes.o
SYM(Chars.sy):	Chars.d
		$(M2C) $(MFLAGS) Chars.d
$(LIB)(Chars.o): SYM(Chars.sy) Chars.m2
		$(M2C) $(MFLAGS) -c Chars.m2
		ar rv $(LIB) Chars.o && rm -f Chars.o
SYM(E.sy):	E.d ./SYM(SystemTypes.sy) SYM(Printf.sy)
		$(M2C) $(MFLAGS) E.d
$(LIB)(E.o):	SYM(Chars.sy) ./SYM(Errno.sy) ./SYM(StdIO.sy) \
		./SYM(Strings.sy) ./SYM(SysExit.sy) ./SYM(SysGetpid.sy) \
		./SYM(SystemTypes.sy) SYM(LongStrings.sy) SYM(Printf.sy) \
		SYM(E.sy) E.m2
		$(M2C) $(MFLAGS) -c E.m2
		ar rv $(LIB) E.o && rm -f E.o
SYM(EditWindow.sy): EditWindow.d ./SYM(FunctionKey.sy) ./SYM(Windows.sy)
		$(M2C) $(MFLAGS) EditWindow.d
$(LIB)(EditWindow.o): SYM(Chars.sy) ./SYM(FunctionKey.sy) ./SYM(StdIO.sy) \
		./SYM(Windows.sy) SYM(EditWindow.sy) SYM(E.sy) SYM(F.sy) \
		EditWindow.m2
		$(M2C) $(MFLAGS) -c EditWindow.m2
		ar rv $(LIB) EditWindow.o && rm -f EditWindow.o
SYM(F.sy):	F.d ./SYM(StdIO.sy) SYM(Printf.sy)
		$(M2C) $(MFLAGS) F.d
$(LIB)(F.o):	./SYM(StdIO.sy) SYM(LongStrings.sy) SYM(Printf.sy) SYM(E.sy) \
		SYM(F.sy) F.m2
		$(M2C) $(MFLAGS) -c F.m2
		ar rv $(LIB) F.o && rm -f F.o
SYM(Keywords.sy): Keywords.d
		$(M2C) $(MFLAGS) Keywords.d
$(LIB)(Keywords.o): SYM(Keywords.sy) Keywords.m2
		$(M2C) $(MFLAGS) -c Keywords.m2
		ar rv $(LIB) Keywords.o && rm -f Keywords.o
SYM(L.sy):	L.d SYM(LongStrings.sy) SYM(Printf.sy)
		$(M2C) $(MFLAGS) L.d
$(LIB)(L.o):	SYM(LongStrings.sy) SYM(Printf.sy) SYM(E.sy) SYM(L.sy) L.m2
		$(M2C) $(MFLAGS) -c L.m2
		ar rv $(LIB) L.o && rm -f L.o
SYM(LongStrings.sy): LongStrings.d ./SYM(StdIO.sy)
		$(M2C) $(MFLAGS) LongStrings.d
$(LIB)(LongStrings.o): SYM(Bytes.sy) SYM(Chars.sy) SYM(Stack.sy) \
		./SYM(StdIO.sy) ./SYM(Storage.sy) ./SYM(Strings.sy) \
		SYM(LongStrings.sy) LongStrings.m2
		$(M2C) $(MFLAGS) -c LongStrings.m2
		ar rv $(LIB) LongStrings.o && rm -f LongStrings.o
SYM(OutLines.sy): OutLines.d ./SYM(StdIO.sy) SYM(LongStrings.sy)
		$(M2C) $(MFLAGS) OutLines.d
$(LIB)(OutLines.o): SYM(Chars.sy) ./SYM(StdIO.sy) SYM(LongStrings.sy) \
		SYM(OutLines.sy) OutLines.m2
		$(M2C) $(MFLAGS) -c OutLines.m2
		ar rv $(LIB) OutLines.o && rm -f OutLines.o
SYM(P.sy):	P.d SYM(Printf.sy)
		$(M2C) $(MFLAGS) P.d
$(LIB)(P.o):	./SYM(StdIO.sy) SYM(LongStrings.sy) SYM(Printf.sy) SYM(E.sy) \
		SYM(P.sy) P.m2
		$(M2C) $(MFLAGS) -c P.m2
		ar rv $(LIB) P.o && rm -f P.o
SYM(Printf.sy):	Printf.d SYM(LongStrings.sy)
		$(M2C) $(MFLAGS) Printf.d
$(LIB)(Printf.o): SYM(Bytes.sy) SYM(Chars.sy) ./SYM(Conversions.sy) \
		./SYM(Errno.sy) ./SYM(RealConv.sy) ./SYM(Strings.sy) \
		./SYM(SysPerror.sy) SYM(LongStrings.sy) SYM(Printf.sy) \
		Printf.m2
		$(M2C) $(MFLAGS) -c Printf.m2
		ar rv $(LIB) Printf.o && rm -f Printf.o
SYM(S.sy):	S.d SYM(Printf.sy)
		$(M2C) $(MFLAGS) S.d
$(LIB)(S.o):	SYM(Bytes.sy) SYM(LongStrings.sy) SYM(Printf.sy) SYM(S.sy) \
		SYM(E.sy) S.m2
		$(M2C) $(MFLAGS) -c S.m2
		ar rv $(LIB) S.o && rm -f S.o
SYM(Stack.sy):	Stack.d
		$(M2C) $(MFLAGS) Stack.d
$(LIB)(Stack.o): SYM(Bytes.sy) SYM(Stack.sy) ./SYM(Storage.sy) Stack.m2
		$(M2C) $(MFLAGS) -c Stack.m2
		ar rv $(LIB) Stack.o && rm -f Stack.o
SYM(W.sy):	W.d ./SYM(Windows.sy) SYM(Printf.sy)
		$(M2C) $(MFLAGS) W.d
$(LIB)(W.o):	SYM(Bytes.sy) SYM(Chars.sy) ./SYM(Windows.sy) \
		SYM(LongStrings.sy) SYM(Printf.sy) SYM(W.sy) SYM(E.sy) W.m2
		$(M2C) $(MFLAGS) -c W.m2
		ar rv $(LIB) W.o && rm -f W.o
#
tags:		$(TAGSRC)
		mtags $(TAGSRC)
#
.PRECIOUS:	Pr.date
SPOOL =		lp
pr:		Pr.date
Pr.date:	$(SRC)
		pr $? | $(SPOOL)
		-touch Pr.date
#
clean:		; rm -f $(OBJS) $(SYMS) $(REFS)
#
LibDir :=	/usr/local/lib/modula
LibFiles :=	SYM REF $(LIB)
LibDirFiles :=	$(patsubst %,$(LibDir)/%,$(LibFiles))
Defs :=		$(filter, %.d,$(SRC))
Syms :=		$(patsubst %.d,SYM(%.sy),$(Defs))
.PHONY:		install
install:	$(LibDirFiles)
$(LibDirFiles):	$(LibDir)/%: %
		cp $< $@
SYM:		$(Syms)
REF:		$(LIB)
#
Mods :=		$(filter %.m2,$(SRC))
Objs :=		$(patsubst %.m2,%.o,$(Mods))
.PHONY:		cleanlib
cleanlib:	; ar d $(LIB) $(Objs)
