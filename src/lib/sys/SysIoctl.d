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
   $Id: SysIoctl.d,v 0.2 1997/02/28 15:47:40 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysIoctl.d,v $
   Revision 0.2  1997/02/28  15:47:40  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:14  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE SysIoctl;

   (* SUN version *)

   FROM SYSTEM IMPORT BYTE;

   CONST
      shift = 0;
      Tandem = { shift + 15 };
      Cbreak = { shift + 14 };
      Lcase  = { shift + 13 };
      Echo   = { shift + 12 };
      Crmod  = { shift + 11 };
      Raw    = { shift + 10 };
      Oddp   = { shift + 9 };
      Evenp  = { shift + 8 };
      Anyp   = Oddp + Evenp;
      Nldelay = { shift + 6 , shift + 7 };
      Tbdelay = { shift + 4 , shift + 5 };
      Xtabs  = { shift + 4 , shift + 5 };
      Crdelay = { shift + 2 , 3 };
      Vtdelay = { shift + 1 };
      Bsdelay = { shift + 0 };
      Alldelay = Bsdelay + Vtdelay + Crdelay +
                 Xtabs + Tbdelay + Nldelay;

      IocVoid = { 2 };
      IocOut = { 1 };
      IocIn = { 0 };
      IocInOut = IocIn + IocOut;

      getd = BITSET(0) + IocOut;
      setd = BITSET(1) + IocIn;
      hpcl = BITSET(2) + IocVoid;
      modg = BITSET(3) + IocOut;
      mods = BITSET(4) + IocIn;
      getp = BITSET(8) + IocOut;
      setp = BITSET(9) + IocIn;
      setn = BITSET(10) + IocIn;
      excl = BITSET(13) + IocVoid;
      nxcl = BITSET(14) + IocVoid;
      flush = BITSET(16) + IocIn;
      setc = BITSET(17) + IocIn;
      getc = BITSET(18) + IocOut;
      (* BSD or SUN specific ioctl-calls *)
      lbis = BITSET(127) + IocIn;
      lbic = BITSET(126) + IocIn;
      lset = BITSET(125) + IocIn;
      lget = BITSET(124) + IocOut;
      sbrk = BITSET(123) + IocVoid;
      cbrk = BITSET(122) + IocVoid;
      cdtr = BITSET(120) + IocVoid;
      gprgp = BITSET(119) + IocOut;
      sprgp = BITSET(118) + IocIn;
      sltc = BITSET(117) + IocIn;
      gltc = BITSET(116) + IocOut;
      outq = BITSET(115) + IocOut;
      sti = BITSET(114) + IocIn;
      notty = BITSET(113) + IocVoid;
      pkt = BITSET(112) + IocIn;
      stop = BITSET(111) + IocVoid;
      start = BITSET(110) + IocVoid;
      mset = BITSET(109) + IocIn;
      mbis = BITSET(108) + IocIn;
      mbic = BITSET(107) + IocIn;
      mget = BITSET(106) + IocOut;
      remote = BITSET(105) + IocIn;
      gwinsz = BITSET(104) + IocOut;
      swinsz = BITSET(103) + IocIn;
      ucntl = BITSET(102) + IocIn;

      SizeOfSgttyb = 6;	(* size of corresponding C-structures *)
      SizeOfTchars = 6;
      SizeOfWinsize = 8;

      (* values of Sgttyb ispeed and ospeed *)
      b0    =  0;
      b50   =  1;
      b75   =  2;
      b110  =  3;
      b134  =  4;
      b150  =  5;
      b200  =  6;
      b300  =  7;
      b600  =  8;
      b1200 =  9;
      b1800 = 10;
      b2400 = 11;
      b4800 = 12;
      b9600 = 13;
      exta  = 14;
      extb  = 15;

   TYPE

      Sgttyb =
         RECORD
            ispeed: CHAR;
            ospeed: CHAR;
            erase: CHAR;
            kill: CHAR;
            flags: BITSET;
         END;

      Tchars =
         RECORD
            intrc: CHAR;
            quitc: CHAR;
            startc: CHAR;
            stopc: CHAR;
            eofc: CHAR;
            brkc: CHAR;
         END;

      Winsize =
	 RECORD
	    rows, cols: CARDINAL;
	    xpixels, ypixels: CARDINAL; (* not used *)
	 END;

   PROCEDURE Ioctl(fd: CARDINAL; request: BITSET;
                   VAR argp: ARRAY OF BYTE;
		   argpsize: CARDINAL) : BOOLEAN;
      (* argpsize: size of corresponding C-structure *)

   PROCEDURE Stty(fd: CARDINAL; argp: Sgttyb) : BOOLEAN;

   PROCEDURE Gtty(fd: CARDINAL; VAR argp: Sgttyb) : BOOLEAN;

   PROCEDURE Isatty(fd: CARDINAL) : BOOLEAN;

   PROCEDURE GetWinsize(fd: CARDINAL; VAR winbuf: Winsize) : BOOLEAN;

   PROCEDURE Baudrate(speed: CHAR) : CARDINAL;

END SysIoctl.
