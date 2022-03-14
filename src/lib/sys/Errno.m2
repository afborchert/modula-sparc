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
   $Id: Errno.m2,v 0.3 1997/02/28 15:47:17 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Errno.m2,v $
   Revision 0.3  1997/02/28  15:47:17  borchert
   header fixed

   Revision 0.2  1997/02/28  15:45:57  borchert
   initialization of message and name array added

   Revision 0.1  1997/02/21  19:05:26  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Errno;

BEGIN
   message[0] := "";
   name[0] := "";
   message[1] := "Not super-user";
   name[1] := "EPERM";
   message[2] := "No such file or directory";
   name[2] := "ENOENT";
   message[3] := "No such process";
   name[3] := "ESRCH";
   message[4] := "interrupted system call";
   name[4] := "EINTR";
   message[5] := "I/O error";
   name[5] := "EIO";
   message[6] := "No such device or address";
   name[6] := "ENXIO";
   message[7] := "Arg list too long";
   name[7] := "E2BIG";
   message[8] := "Exec format error";
   name[8] := "ENOEXEC";
   message[9] := "Bad file number";
   name[9] := "EBADF";
   message[10] := "No children";
   name[10] := "ECHILD";
   message[11] := "Resource temporarily unavailable";
   name[11] := "EAGAIN";
   message[12] := "Not enough core";
   name[12] := "ENOMEM";
   message[13] := "Permission denied";
   name[13] := "EACCES";
   message[14] := "Bad address";
   name[14] := "EFAULT";
   message[15] := "Block device required";
   name[15] := "ENOTBLK";
   message[16] := "Mount device busy";
   name[16] := "EBUSY";
   message[17] := "File exists";
   name[17] := "EEXIST";
   message[18] := "Cross-device link";
   name[18] := "EXDEV";
   message[19] := "No such device";
   name[19] := "ENODEV";
   message[20] := "Not a directory";
   name[20] := "ENOTDIR";
   message[21] := "Is a directory";
   name[21] := "EISDIR";
   message[22] := "Invalid argument";
   name[22] := "EINVAL";
   message[23] := "File table overflow";
   name[23] := "ENFILE";
   message[24] := "Too many open files";
   name[24] := "EMFILE";
   message[25] := "Inappropriate ioctl for device";
   name[25] := "ENOTTY";
   message[26] := "Text file busy";
   name[26] := "ETXTBSY";
   message[27] := "File too large";
   name[27] := "EFBIG";
   message[28] := "No space left on device";
   name[28] := "ENOSPC";
   message[29] := "Illegal seek";
   name[29] := "ESPIPE";
   message[30] := "Read only file system";
   name[30] := "EROFS";
   message[31] := "Too many links";
   name[31] := "EMLINK";
   message[32] := "Broken pipe";
   name[32] := "EPIPE";
   message[33] := "Math arg out of domain of func";
   name[33] := "EDOM";
   message[34] := "Math result not representable";
   name[34] := "ERANGE";
   message[35] := "No message of desired type";
   name[35] := "ENOMSG";
   message[36] := "Identifier removed";
   name[36] := "EIDRM";
   message[37] := "Channel number out of range";
   name[37] := "ECHRNG";
   message[38] := "Level 2 not synchronized";
   name[38] := "EL2NSYNC";
   message[39] := "Level 3 halted";
   name[39] := "EL3HLT";
   message[40] := "Level 3 reset";
   name[40] := "EL3RST";
   message[41] := "Link number out of range";
   name[41] := "ELNRNG";
   message[42] := "Protocol driver not attached";
   name[42] := "EUNATCH";
   message[43] := "No CSI structure available";
   name[43] := "ENOCSI";
   message[44] := "Level 2 halted";
   name[44] := "EL2HLT";
   message[45] := "Deadlock condition.";
   name[45] := "EDEADLK";
   message[46] := "No record locks available.";
   name[46] := "ENOLCK";
   message[47] := "Operation canceled";
   name[47] := "ECANCELED";
   message[48] := "Operation not supported";
   name[48] := "ENOTSUP";
   message[49] := "Disc quota exceeded";
   name[49] := "EDQUOT";
   message[50] := "invalid exchange";
   name[50] := "EBADE";
   message[51] := "invalid request descriptor";
   name[51] := "EBADR";
   message[52] := "exchange full";
   name[52] := "EXFULL";
   message[53] := "no anode";
   name[53] := "ENOANO";
   message[54] := "invalid request code";
   name[54] := "EBADRQC";
   message[55] := "invalid slot";
   name[55] := "EBADSLT";
   message[56] := "file locking deadlock error";
   name[56] := "EDEADLOCK";
   message[57] := "bad font file fmt";
   name[57] := "EBFONT";
   message[58] := "";
   name[58] := "";
   message[59] := "";
   name[59] := "";
   message[60] := "Device not a stream";
   name[60] := "ENOSTR";
   message[61] := "no data (for no delay io)";
   name[61] := "ENODATA";
   message[62] := "timer expired";
   name[62] := "ETIME";
   message[63] := "out of streams resources";
   name[63] := "ENOSR";
   message[64] := "Machine is not on the network";
   name[64] := "ENONET";
   message[65] := "Package not installed";
   name[65] := "ENOPKG";
   message[66] := "The object is remote";
   name[66] := "EREMOTE";
   message[67] := "the link has been severed";
   name[67] := "ENOLINK";
   message[68] := "advertise error";
   name[68] := "EADV";
   message[69] := "srmount error";
   name[69] := "ESRMNT";
   message[70] := "Communication error on send";
   name[70] := "ECOMM";
   message[71] := "Protocol error";
   name[71] := "EPROTO";
   message[72] := "";
   name[72] := "";
   message[73] := "";
   name[73] := "";
   message[74] := "multihop attempted";
   name[74] := "EMULTIHOP";
   message[75] := "";
   name[75] := "";
   message[76] := "";
   name[76] := "";
   message[77] := "trying to read unreadable message";
   name[77] := "EBADMSG";
   message[78] := "path name is too long";
   name[78] := "ENAMETOOLONG";
   message[79] := "value too large to be stored in data type";
   name[79] := "EOVERFLOW";
   message[80] := "given log. name not unique";
   name[80] := "ENOTUNIQ";
   message[81] := "f.d. invalid for this operation";
   name[81] := "EBADFD";
   message[82] := "Remote address changed";
   name[82] := "EREMCHG";
   message[83] := "Can't access a needed shared lib.";
   name[83] := "ELIBACC";
   message[84] := "Accessing a corrupted shared lib.";
   name[84] := "ELIBBAD";
   message[85] := ".lib section in a.out corrupted.";
   name[85] := "ELIBSCN";
   message[86] := "Attempting to link in too many libs.";
   name[86] := "ELIBMAX";
   message[87] := "Attempting to exec a shared library.";
   name[87] := "ELIBEXEC";
   message[88] := "Illegal byte sequence.";
   name[88] := "EILSEQ";
   message[89] := "Unsupported file system operation";
   name[89] := "ENOSYS";
   message[90] := "Symbolic link loop";
   name[90] := "ELOOP";
   message[91] := "Restartable system call";
   name[91] := "ERESTART";
   message[92] := "if pipe/FIFO, don't sleep in stream head";
   name[92] := "ESTRPIPE";
   message[93] := "directory not empty";
   name[93] := "ENOTEMPTY";
   message[94] := "Too many users (for UFS)";
   name[94] := "EUSERS";
   message[95] := "Socket operation on non-socket";
   name[95] := "ENOTSOCK";
   message[96] := "Destination address required";
   name[96] := "EDESTADDRREQ";
   message[97] := "Message too long";
   name[97] := "EMSGSIZE";
   message[98] := "Protocol wrong type for socket";
   name[98] := "EPROTOTYPE";
   message[99] := "Protocol not available";
   name[99] := "ENOPROTOOPT";
   message[100] := "";
   name[100] := "";
   message[101] := "";
   name[101] := "";
   message[102] := "";
   name[102] := "";
   message[103] := "";
   name[103] := "";
   message[104] := "";
   name[104] := "";
   message[105] := "";
   name[105] := "";
   message[106] := "";
   name[106] := "";
   message[107] := "";
   name[107] := "";
   message[108] := "";
   name[108] := "";
   message[109] := "";
   name[109] := "";
   message[110] := "";
   name[110] := "";
   message[111] := "";
   name[111] := "";
   message[112] := "";
   name[112] := "";
   message[113] := "";
   name[113] := "";
   message[114] := "";
   name[114] := "";
   message[115] := "";
   name[115] := "";
   message[116] := "";
   name[116] := "";
   message[117] := "";
   name[117] := "";
   message[118] := "";
   name[118] := "";
   message[119] := "";
   name[119] := "";
   message[120] := "Protocol not supported";
   name[120] := "EPROTONOSUPPORT";
   message[121] := "Socket type not supported";
   name[121] := "ESOCKTNOSUPPORT";
   message[122] := "Operation not supported on socket";
   name[122] := "EOPNOTSUPP";
   message[123] := "Protocol family not supported";
   name[123] := "EPFNOSUPPORT";
   message[124] := "Address family not supported by";
   name[124] := "EAFNOSUPPORT";
   message[125] := "Address already in use";
   name[125] := "EADDRINUSE";
   message[126] := "Can't assign requested address";
   name[126] := "EADDRNOTAVAIL";
   message[127] := "Network is down";
   name[127] := "ENETDOWN";
   message[128] := "Network is unreachable";
   name[128] := "ENETUNREACH";
   message[129] := "Network dropped connection because";
   name[129] := "ENETRESET";
   message[130] := "Software caused connection abort";
   name[130] := "ECONNABORTED";
   message[131] := "Connection reset by peer";
   name[131] := "ECONNRESET";
   message[132] := "No buffer space available";
   name[132] := "ENOBUFS";
   message[133] := "Socket is already connected";
   name[133] := "EISCONN";
   message[134] := "Socket is not connected";
   name[134] := "ENOTCONN";
   message[135] := "";
   name[135] := "";
   message[136] := "";
   name[136] := "";
   message[137] := "";
   name[137] := "";
   message[138] := "";
   name[138] := "";
   message[139] := "";
   name[139] := "";
   message[140] := "";
   name[140] := "";
   message[141] := "";
   name[141] := "";
   message[142] := "";
   name[142] := "";
   message[143] := "Can't send after socket shutdown";
   name[143] := "ESHUTDOWN";
   message[144] := "Too many references: can't splice";
   name[144] := "ETOOMANYREFS";
   message[145] := "Connection timed out";
   name[145] := "ETIMEDOUT";
   message[146] := "Connection refused";
   name[146] := "ECONNREFUSED";
   message[147] := "Host is down";
   name[147] := "EHOSTDOWN";
   message[148] := "No route to host";
   name[148] := "EHOSTUNREACH";
   message[149] := "operation already in progress";
   name[149] := "EALREADY";
   message[150] := "operation now in progress";
   name[150] := "EINPROGRESS";
   message[151] := "Stale NFS file handle";
   name[151] := "ESTALE";
END Errno.
