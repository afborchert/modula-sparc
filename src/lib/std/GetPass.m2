(* Ulm's Modula-2 Library
   Copyright (C) 1984-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm's Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: GetPass.m2,v 0.3 1998/04/28 07:31:47 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: GetPass.m2,v $
   Revision 0.3  1998/04/28  07:31:47  borchert
   adapted to Solaris 2.x

   Revision 0.2  1997/02/28  15:50:10  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:26  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE GetPass;

   FROM SysTermIO IMPORT SetTermIO, GetTermIO, TermIO, vmin, vtime, echo,
      icanon, tab1, tab2, opost, parmrk, ignpar, inpck, istrip, ixon, icrnl;
   FROM SysOpen IMPORT Open;
   FROM FtdIO IMPORT FreadChar, FwriteChar, FwriteString, FwriteLn;
   FROM ASCII IMPORT nl, nak, bs, cr, bell;
   FROM RandomGenerator IMPORT Random;
   FROM StdIO IMPORT FILE, stdin, stdout, Fdopen, Fclose, read, write;

   PROCEDURE GetPass(prompt: ARRAY OF CHAR; VAR passwd: ARRAY OF CHAR);
      CONST
         MaxPasswdLen = 14;
         Terminal = "/dev/tty";
         Read = 0;
         Write = 1;
      VAR
         index: CARDINAL; (* in passwd *)
         oldterm, term: TermIO;
         ch: CHAR; (* last character read *)
         bslen: ARRAY[0..MaxPasswdLen-1] OF CARDINAL;
         i: CARDINAL;
         low, high: CARDINAL;
         termin, termout: FILE;
         termfdin: CARDINAL; (* file descriptor of terminal input *)
         termfdout: CARDINAL;
         terminal: BOOLEAN;

      PROCEDURE Isatty(fd: CARDINAL) : BOOLEAN;
	 VAR
	    termio: TermIO;
      BEGIN
	 RETURN GetTermIO(fd, termio)
      END Isatty;

   BEGIN (* GetPass *)
      passwd[0] := 0C;
      terminal := Isatty(0) AND Isatty(1);
      IF terminal THEN
         termin := stdin; termout := stdout;
         termfdin := 0; termfdout := 1;
      ELSIF NOT Open(termfdin, Terminal, Read) OR
            NOT Open(termfdout, Terminal, Write) OR
            NOT Fdopen(termin, termfdin, read, (* buffered = *) FALSE) OR
            NOT Fdopen(termout, termfdout, write, (* buffered = *) FALSE) THEN
         RETURN
      END;
      FwriteString(termout, prompt);
      IF ~GetTermIO(termfdin, term) THEN RETURN END;
      oldterm := term;
      WITH term DO
	 cc[vmin] := 1C; cc[vtime] := 1C;
	 linemodes := linemodes - echo - icanon;
	 inputmodes := inputmodes - parmrk + ignpar - inpck + istrip - icrnl;
	 outputmodes := outputmodes - tab1 - tab2 - opost;
      END;
      IF ~SetTermIO(termfdin, term) THEN RETURN END;
      index := 0;
      REPEAT
         FreadChar(termin, ch);
         CASE ch OF
         | cr, nl: FwriteChar(termout, cr); FwriteChar(termout, nl);
         | bs:
             IF index > 0 THEN
                DEC(index);
                FOR i := 1 TO bslen[index] DO
                   FwriteChar(termout, bs);
                   FwriteChar(termout, " ");
                   FwriteChar(termout, bs);
                END;
             END;
         | nak: (* ^U *)
             WHILE index > 0 DO
                DEC(index);
                FOR i := 1 TO bslen[index] DO
                   FwriteChar(termout, bs);
                   FwriteChar(termout, " ");
                   FwriteChar(termout, bs);
                END;
             END;
         ELSE
	     IF (index <= HIGH(passwd)) & (index <= HIGH(bslen)) &
	           (index <= MaxPasswdLen) THEN
		passwd[index] := ch;
		bslen[index] := Random(1, 3);
		low := ORD('!'); high := ORD('~');
		FOR i := 1 TO bslen[index] DO
		   FwriteChar(termout, CHR(Random(low, high)));
		END;
		INC(index);
	     ELSE
	        FwriteChar(termout, bell);
	     END;
         END;
      UNTIL (ch = nl) OR (ch = cr);
      IF index <= HIGH(passwd) THEN
         passwd[index] := 0C;
      END;
      IF ~SetTermIO(termfdin, oldterm) THEN END;
      IF NOT terminal AND Fclose(termin) AND Fclose(termout) THEN END;
   END GetPass;

END GetPass.
