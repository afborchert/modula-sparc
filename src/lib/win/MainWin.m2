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
   $Id: MainWin.m2,v 0.2 1997/02/28 15:59:21 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MainWin.m2,v $
   Revision 0.2  1997/02/28  15:59:21  borchert
   header fixed

   Revision 0.1  1997/02/21  19:43:16  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MainWin;

   FROM Windows IMPORT Window, SetWindowPos, GetWindowPos, WindowClear,
      WindowWrite, WindowRead, WindowAtSet, WindowAttributes,
      OpenDeviceFile, OpenTerminfo, CloseDevice, CreateWindow, Terminfo,
      Device, OpenDevice, GetWindowSize, FlushWindow, SetWindowAttributes;
   FROM FtdWin IMPORT WinWriteString, WinReadCard, WinWriteCard,
      WinReadInt, WinWriteInt, WinReadString;
   FROM StdIO IMPORT stdin, stdout, stderr;
   FROM FtdIO IMPORT FwriteString, FwriteLn;
   FROM ASCII IMPORT nl;
   FROM SysExit IMPORT EnterCleanup, Exit;
   IMPORT Windows, FtdWin;

   (* (* exported from definition module *)
   VAR
      mainwin: Window; (* window affected by following procedures *)
      lines, columns: CARDINAL; (* size of mainwin *)
      Done: BOOLEAN;
   *)
   VAR
      tinfo: Terminfo;
      dev: Device;

   PROCEDURE SetPos(line, column: CARDINAL);
   BEGIN
      SetWindowPos(mainwin, line, column);
      Done := Windows.Done;
   END SetPos;

   PROCEDURE GetPos(VAR line, column: CARDINAL);
   BEGIN
      GetWindowPos(mainwin, line, column);
      Done := Windows.Done;
   END GetPos;

   PROCEDURE Clear;
   BEGIN
      WindowClear(mainwin);
      Done := Windows.Done;
   END Clear;

   PROCEDURE Flush;
   BEGIN
      FlushWindow(mainwin);
      Done := Windows.Done;
   END Flush;

   PROCEDURE WriteString(s: ARRAY OF CHAR);
   BEGIN
      WinWriteString(mainwin, s);
      Done := FtdWin.Done;
   END WriteString;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      WindowWrite(mainwin, ch);
      Done := Windows.Done;
   END Write;

   PROCEDURE WriteLn;
   BEGIN
      WindowWrite(mainwin, nl);
      Done := Windows.Done;
   END WriteLn;

   PROCEDURE Read(VAR ch: CHAR);
   BEGIN
      WindowRead(mainwin, ch);
      Done := Windows.Done;
   END Read;

   PROCEDURE ReadString(VAR s: ARRAY OF CHAR);
   BEGIN
      WinReadString(mainwin, s);
      Done := FtdWin.Done;
   END ReadString;

   PROCEDURE WriteInt(i: INTEGER; w: CARDINAL);
   BEGIN
      WinWriteInt(mainwin, i, w);
      Done := FtdWin.Done;
   END WriteInt;

   PROCEDURE ReadInt(VAR i: INTEGER);
   BEGIN
      WinReadInt(mainwin, i);
      Done := FtdWin.Done;
   END ReadInt;

   PROCEDURE WriteCard(c: CARDINAL; w: CARDINAL);
   BEGIN
      WinWriteCard(mainwin, c, w);
      Done := FtdWin.Done;
   END WriteCard;

   PROCEDURE ReadCard(VAR c: CARDINAL);
   BEGIN
      WinReadCard(mainwin, c);
      Done := FtdWin.Done;
   END ReadCard;

   PROCEDURE Cleanup;
   BEGIN
      SetPos(lines-1, 0);
      FlushWindow(mainwin);
      CloseDevice(dev);
   END Cleanup;

BEGIN
   OpenTerminfo(tinfo, "");
   IF NOT Windows.Done THEN
      FwriteString(stderr, "MainWin: Don't know about your terminal.");
      FwriteLn(stderr);
      Exit(255);
   END;
   OpenDeviceFile(dev, "/dev/tty", tinfo);
   CreateWindow(mainwin, dev); GetWindowSize(mainwin, lines, columns);
   SetWindowAttributes(mainwin, WindowAtSet{flushoninput, movecursor, echo});
   EnterCleanup(Cleanup);
END MainWin.
