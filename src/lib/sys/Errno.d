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
   $Id: Errno.d,v 0.2 1997/02/28 15:45:23 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Errno.d,v $
   Revision 0.2  1997/02/28  15:45:23  borchert
   adapted to Solaris 2.5.1

   Revision 0.1  1997/02/21  19:05:06  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Errno;

   (* following constants have been extracted from
         /usr/include/sys/errno.h
      on SunOS 5.5.1 at 1997/02/26
   *)

   CONST
      EPERM           =   1;  EL3RST          =  40;  ELIBSCN         =  85;  
      ENOENT          =   2;  ELNRNG          =  41;  ELIBMAX         =  86;  
      ESRCH           =   3;  EUNATCH         =  42;  ELIBEXEC        =  87;  
      EINTR           =   4;  ENOCSI          =  43;  EILSEQ          =  88;  
      EIO             =   5;  EL2HLT          =  44;  ENOSYS          =  89;  
      ENXIO           =   6;  EDEADLK         =  45;  ELOOP           =  90;  
      E2BIG           =   7;  ENOLCK          =  46;  ERESTART        =  91;  
      ENOEXEC         =   8;  ECANCELED       =  47;  ESTRPIPE        =  92;  
      EBADF           =   9;  ENOTSUP         =  48;  ENOTEMPTY       =  93;  
      ECHILD          =  10;  EDQUOT          =  49;  EUSERS          =  94;  
      EAGAIN          =  11;  EBADE           =  50;  ENOTSOCK        =  95;  
      ENOMEM          =  12;  EBADR           =  51;  EDESTADDRREQ    =  96;  
      EACCES          =  13;  EXFULL          =  52;  EMSGSIZE        =  97;  
      EFAULT          =  14;  ENOANO          =  53;  EPROTOTYPE      =  98;  
      ENOTBLK         =  15;  EBADRQC         =  54;  ENOPROTOOPT     =  99;  
      EBUSY           =  16;  EBADSLT         =  55;  EPROTONOSUPPORT = 120;  
      EEXIST          =  17;  EDEADLOCK       =  56;  ESOCKTNOSUPPORT = 121;  
      EXDEV           =  18;  EBFONT          =  57;  EOPNOTSUPP      = 122;  
      ENODEV          =  19;  ENOSTR          =  60;  EPFNOSUPPORT    = 123;  
      ENOTDIR         =  20;  ENODATA         =  61;  EAFNOSUPPORT    = 124;  
      EISDIR          =  21;  ETIME           =  62;  EADDRINUSE      = 125;  
      EINVAL          =  22;  ENOSR           =  63;  EADDRNOTAVAIL   = 126;  
      ENFILE          =  23;  ENONET          =  64;  ENETDOWN        = 127;  
      EMFILE          =  24;  ENOPKG          =  65;  ENETUNREACH     = 128;  
      ENOTTY          =  25;  EREMOTE         =  66;  ENETRESET       = 129;  
      ETXTBSY         =  26;  ENOLINK         =  67;  ECONNABORTED    = 130;  
      EFBIG           =  27;  EADV            =  68;  ECONNRESET      = 131;  
      ENOSPC          =  28;  ESRMNT          =  69;  ENOBUFS         = 132;  
      ESPIPE          =  29;  ECOMM           =  70;  EISCONN         = 133;  
      EROFS           =  30;  EPROTO          =  71;  ENOTCONN        = 134;  
      EMLINK          =  31;  EMULTIHOP       =  74;  ESHUTDOWN       = 143;  
      EPIPE           =  32;  EBADMSG         =  77;  ETOOMANYREFS    = 144;  
      EDOM            =  33;  ENAMETOOLONG    =  78;  ETIMEDOUT       = 145;  
      ERANGE          =  34;  EOVERFLOW       =  79;  ECONNREFUSED    = 146;  
      ENOMSG          =  35;  ENOTUNIQ        =  80;  EHOSTDOWN       = 147;  
      EIDRM           =  36;  EBADFD          =  81;  EHOSTUNREACH    = 148;  
      ECHRNG          =  37;  EREMCHG         =  82;  EALREADY        = 149;  
      EL2NSYNC        =  38;  ELIBACC         =  83;  EINPROGRESS     = 150;  
      EL3HLT          =  39;  ELIBBAD         =  84;  ESTALE          = 151;  

      EWOULDBLOCK     = EAGAIN;

   CONST
      maxerror = 151;
      maxmsglen = 41;
      maxnamelen = 15;

   TYPE
      ErrorNumber = [0..maxerror];
      ErrorMessage = ARRAY [0..maxmsglen] OF CHAR;
      ErrorName = ARRAY [0..maxnamelen] OF CHAR;

   VAR
      message: ARRAY ErrorNumber OF ErrorMessage;
      name: ARRAY ErrorNumber OF ErrorName;

   VAR
      errno: CARDINAL;

END Errno. 
