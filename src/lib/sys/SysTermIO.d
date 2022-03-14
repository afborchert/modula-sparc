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
   $Id: SysTermIO.d,v 0.2 1997/02/28 15:48:02 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysTermIO.d,v $
   Revision 0.2  1997/02/28  15:48:02  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:22  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE SysTermIO;

   (* see termio(7) for explanations *)

   CONST
      (* add ``x-'' globally for big-endian representation of SETs *)
      x = 31;
      (* input modes *)
      ignbrk = { 31 };
      brkint = { 30 };
      ignpar = { 29 };
      parmrk = { 28 };
      inpck = { 27 };
      istrip = { 26 };
      inlcr = { 25 };
      igncr = { 24 };
      icrnl = { 23 };
      iuclc = { 22 };
      ixon = { 21 };
      ixany = { 20 };
      ixoff = { 19 };
      (* output modes *)
      opost = { 31 };
      olcuc = { 30 };
      onlcr = { 29 };
      ocrnl = { 28 };
      onocr = { 27 };
      onlret = { 26 };
      ofill = { 25 };
      ofdel = { 24 };
      (* delays for newline *)
      nldly = { 23 };		(* mask *)
      nl0 = { };
      nl1 = { 23 };
      (* delays for carriage return *)
      crdly = { 21, x-22 };	(* mask *)
      cr0 = { };
      cr1 = { 22 };
      cr2 = { 21 };
      cr3 = { 21, x-22 };
      (* delays for tabs *)
      tabdly = { 19, x-20 };	(* mask *)
      tab1 = { 20 };
      tab2 = { 19 };
      tab3 = { 19, x-20 };	(* expand tabs to spaces *)
      (* delays for backspaces *)
      bsdly = { 18 };		(* mask *)
      bs0 = { };
      bs1 = { 18 };
      (* delays for vertical tabs *)
      vtdly = { 17 };		(* mask *)
      vt0 = { };
      vt1 = { 17 };
      (* delays for form feeds *)
      ffdly = { 16 };
      ff0 = { };
      ff1 = { 16 };

      (* control modes *)
      cbaud = { 28, x-29, x-30, x-31 };	(* mask *)
      b0 = {};
      b50 = { 31 };
      b75 = { 30 };
      b110 = { 29, x-30 };
      b134 = { 29 };
      b150 = { 29, x-31 };
      b200 = { 29, x-30 };
      b300 = { 29, x-30, x-31 };
      b600 = { 28 };
      b1200 = { 28, x-31 };
      b1800 = { 28, x-30 };
      b2400 = { 28, x-30, x-31 };
      b4800 = { 28, x-29 };
      b9600 = { 28, x-29, x-31 };
      b19200 = { 28, x-29, x-30 };
      b38400 = { 28, x-29, x-30, x-31 };
      csize = { 26, x-27 };	(* mask *)
      cs5 = {};
      cs6 = { 27 };
      cs7 = { 26 };
      cs8 = { 26, x-27 };
      cstopb = { 25 };
      cread = { 24 };
      parenb = { 23 };
      parodd = { 22 };
      hupcl = { 21 };
      clocal = { 20 };
      
      (* line modes *)
      isig = { 31 };
      icanon = { 30 };
      xcase = { 29 };
      echo = { 28 };
      echoe = { 27 };
      echok = { 26 };
      echonl = { 25 };
      noflsh = { 24 };
   TYPE
      ControlChars = (vintr, vquit, verase, vkill, veof, veol, veol2, vswtch);
   CONST
      vmin = veof; vtime = veol;

   TYPE
      ControlCharsRange = [MIN(ControlChars)..MAX(ControlChars)];
      InputModes = BITSET;
      OutputModes = BITSET;
      ControlModes = BITSET;
      LineModes = BITSET;
      TermIO =
	 RECORD
	    inputmodes: InputModes;
	    outputmodes: OutputModes;
	    controlmodes: ControlModes;
	    linemodes: LineModes;
	    linedisc: CHAR;
	    cc: ARRAY ControlCharsRange OF CHAR;
	 END;

      Winsize =
	 RECORD
	    rows, cols: CARDINAL;
	    xpixels, ypixels: CARDINAL;
	 END;

   PROCEDURE SetTermIO(fd: CARDINAL; termio: TermIO) : BOOLEAN;

   PROCEDURE GetTermIO(fd: CARDINAL; VAR termio: TermIO) : BOOLEAN;

   PROCEDURE GetWinsize(fd: CARDINAL; VAR winbuf: Winsize) : BOOLEAN;

   PROCEDURE Baudrate(termio: TermIO) : CARDINAL;

   PROCEDURE Isatty(fd: CARDINAL) : BOOLEAN;

END SysTermIO.
