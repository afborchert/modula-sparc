(* Ulm Modula-2 Library
   Copyright (C) 1984-2024 by Andreas F. Borchert
   ----------------------------------------------------------------------------
   Ulm Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@andreas-borchert.de
   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Sys;

   (* following constants have been extracted from
         /usr/include/sys/syscall.h
      on SunOS 5.11 on 2024/09/22
   *)

   CONST
      syscall           =   0;           putpmsg           = 133;
      exit              =   1;           uname             = 135;
      spawn             =   2;           setegid           = 136;
      read              =   3;           sysconfig         = 137;
      write             =   4;           adjtime           = 138;
      close             =   6;           systeminfo        = 139;
      linkat            =   7;           sharefs           = 140;
      reflinkat         =   8;           seteuid           = 141;
      recvmmsg          =   9;           forksys           = 142;
      sendmmsg          =  10;           getrandom         = 143;
      symlinkat         =  11;           sigtimedwait      = 144;
      chdir             =  12;           lwpInfo           = 145;
      time              =  13;           yield             = 146;
      brk               =  17;           secsys            = 147;
      lseek             =  19;           lwpSemaPost       = 148;
      getpid            =  20;           lwpSemaTrywait    = 149;
      mount             =  21;           lwpDetach         = 150;
      readlinkat        =  22;           corectl           = 151;
      setuid            =  23;           modctl            = 152;
      getuid            =  24;           fchroot           = 153;
      stime             =  25;           systemStats       = 154;
      pcsample          =  26;           vhangup           = 155;
      alarm             =  27;           gettimeofday      = 156;
      pause             =  29;           getitimer         = 157;
      frealpathat       =  30;           setitimer         = 158;
      stty              =  31;           lwpCreate         = 159;
      gtty              =  32;           lwpExit           = 160;
      nice              =  34;           lwpSuspend        = 161;
      statfs            =  35;           lwpContinue       = 162;
      sync              =  36;           lwpSigqueue       = 163;
      kill              =  37;           lwpSelf           = 164;
      fstatfs           =  38;           lwpSigmask        = 165;
      pgrpsys           =  39;           lwpPrivate        = 166;
      uucopystr         =  40;           sparcFixalign     = 166;
      pipe              =  42;           lwpWait           = 167;
      times             =  43;           lwpMutexWakeup    = 168;
      profil            =  44;           lwpCondWait       = 170;
      faccessat         =  45;           lwpCondSignal     = 171;
      setgid            =  46;           lwpCondBroadcast  = 172;
      getgid            =  47;           pread             = 173;
      mknodat           =  48;           pwrite            = 174;
      msgsys            =  49;           llseek            = 175;
      sysi86            =  50;           instSync          = 176;
      acct              =  51;           brand             = 177;
      shmsys            =  52;           kaio              = 178;
      semsys            =  53;           cpc               = 179;
      ioctl             =  54;           lgrpsys           = 180;
      uadmin            =  55;           rusagesys         = 181;
      fchownat          =  56;           port              = 182;
      utssys            =  57;           pollsys           = 183;
      fdsync            =  58;           labelsys          = 184;
      execve            =  59;           acl               = 185;
      umask             =  60;           auditsys          = 186;
      chroot            =  61;           processorSys      = 187;
      fcntl             =  62;           pOnline           = 189;
      ulimit            =  63;           sigqueue          = 190;
      renameat          =  64;           clockGettime      = 191;
      fstatat           =  66;           clockSettime      = 192;
      fstatat64         =  67;           clockGetres       = 193;
      openat            =  68;           timerCreate       = 194;
      openat64          =  69;           timerDelete       = 195;
      tasksys           =  70;           timerSettime      = 196;
      acctctl           =  71;           timerGettime      = 197;
      exacctsys         =  72;           timerGetoverrun   = 198;
      getpagesizes      =  73;           nanosleep         = 199;
      rctlsys           =  74;           facl              = 200;
      sidsys            =  75;           door              = 201;
      unlinkat          =  76;           setreuid          = 202;
      lwpPark           =  77;           setregid          = 203;
      sendfilev         =  78;           installUtrap      = 204;
      lwpName           =  79;           signotify         = 205;
      getdents          =  81;           schedctl          = 206;
      privsys           =  82;           pset              = 207;
      ucredsys          =  83;           sparcUtrapInstall = 208;
      sysfs             =  84;           resolvepath       = 209;
      getmsg            =  85;           lwpMutexTimedlock = 210;
      putmsg            =  86;           lwpSemaTimedwait  = 211;
      vaMask            =  87;           lwpRwlockSys      = 212;
      memsys            =  88;           getdents64        = 213;
      adi               =  90;           mmap64            = 214;
      setgroups         =  91;           statvfs64         = 218;
      getgroups         =  92;           fstatvfs64        = 219;
      sigprocmask       =  95;           setrlimit64       = 220;
      sigsuspend        =  96;           getrlimit64       = 221;
      sigaltstack       =  97;           pread64           = 222;
      sigaction         =  98;           pwrite64          = 223;
      sigpending        =  99;           rpcsys            = 226;
      context           = 100;           zone              = 227;
      fchmodat          = 101;           autofssys         = 228;
      mkdirat           = 102;           getcwd            = 229;
      statvfs           = 103;           soSocket          = 230;
      fstatvfs          = 104;           soSocketpair      = 231;
      getloadavg        = 105;           bind              = 232;
      nfssys            = 106;           listen            = 233;
      waitid            = 107;           accept            = 234;
      sigsendsys        = 108;           connect           = 235;
      hrtsys            = 109;           shutdown          = 236;
      utimensat         = 110;           recv              = 237;
      sigresend         = 111;           recvfrom          = 238;
      priocntlsys       = 112;           recvmsg           = 239;
      pathconf          = 113;           send              = 240;
      mincore           = 114;           sendmsg           = 241;
      mmap              = 115;           sendto            = 242;
      mprotect          = 116;           getpeername       = 243;
      munmap            = 117;           getsockname       = 244;
      fpathconf         = 118;           getsockopt        = 245;
      vfork             = 119;           setsockopt        = 246;
      fchdir            = 120;           sockconfig        = 247;
      readv             = 121;           ntpGettime        = 248;
      writev            = 122;           ntpAdjtime        = 249;
      uuidsys           = 124;           lwpMutexUnlock    = 250;
      mmapobj           = 127;           lwpMutexTrylock   = 251;
      setrlimit         = 128;           lwpMutexRegister  = 252;
      getrlimit         = 129;           cladm             = 253;
      memcntl           = 131;           uucopy            = 254;
      getpmsg           = 132;           umount2           = 255;

END Sys.
