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
   $Id: Sys.d,v 0.3 1997/02/28 15:47:18 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Sys.d,v $
   Revision 0.3  1997/02/28  15:47:18  borchert
   header fixed

   Revision 0.2  1997/02/26  18:38:50  borchert
   system call numbers adapted to Solaris 2.5.1

   Revision 0.1  1997/02/21  19:05:06  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Sys;

   (* following constants have been extracted from
         /usr/include/sys/syscall.h
      on SunOS 5.5.1 at 1997/02/26
   *)

   CONST
      syscall          =   0;        sigsendsys       = 108;
      exit             =   1;        hrtsys           = 109;
      fork             =   2;        acancel          = 110;
      read             =   3;        async            = 111;
      write            =   4;        priocntlsys      = 112;
      open             =   5;        pathconf         = 113;
      close            =   6;        mincore          = 114;
      wait             =   7;        mmap             = 115;
      creat            =   8;        mprotect         = 116;
      link             =   9;        munmap           = 117;
      unlink           =  10;        fpathconf        = 118;
      exec             =  11;        vfork            = 119;
      chdir            =  12;        fchdir           = 120;
      time             =  13;        readv            = 121;
      mknod            =  14;        writev           = 122;
      chmod            =  15;        xstat            = 123;
      chown            =  16;        lxstat           = 124;
      brk              =  17;        fxstat           = 125;
      stat             =  18;        xmknod           = 126;
      lseek            =  19;        clocal           = 127;
      getpid           =  20;        setrlimit        = 128;
      mount            =  21;        getrlimit        = 129;
      umount           =  22;        lchown           = 130;
      setuid           =  23;        memcntl          = 131;
      getuid           =  24;        getpmsg          = 132;
      stime            =  25;        putpmsg          = 133;
      ptrace           =  26;        rename           = 134;
      alarm            =  27;        uname            = 135;
      fstat            =  28;        setegid          = 136;
      pause            =  29;        sysconfig        = 137;
      utime            =  30;        adjtime          = 138;
      stty             =  31;        systeminfo       = 139;
      gtty             =  32;        seteuid          = 141;
      access           =  33;        vtrace           = 142;
      nice             =  34;        fork1            = 143;
      statfs           =  35;        sigtimedwait     = 144;
      sync             =  36;        lwpInfo          = 145;
      kill             =  37;        yield            = 146;
      fstatfs          =  38;        lwpSemaWait      = 147;
      pgrpsys          =  39;        lwpSemaPost      = 148;
      xenix            =  40;        modctl           = 152;
      dup              =  41;        fchroot          = 153;
      pipe             =  42;        utimes           = 154;
      times            =  43;        vhangup          = 155;
      profil           =  44;        gettimeofday     = 156;
      plock            =  45;        getitimer        = 157;
      setgid           =  46;        setitimer        = 158;
      getgid           =  47;        lwpCreate        = 159;
      signal           =  48;        lwpExit          = 160;
      msgsys           =  49;        lwpSuspend       = 161;
      syssun           =  50;        lwpContinue      = 162;
      sysi86           =  50;        lwpKill          = 163;
      sysppc           =  50;        lwpSelf          = 164;
      acct             =  51;        lwpSetprivate    = 165;
      shmsys           =  52;        lwpGetprivate    = 166;
      semsys           =  53;        lwpWait          = 167;
      ioctl            =  54;        lwpMutexUnlock   = 168;
      uadmin           =  55;        lwpMutexLock     = 169;
      utssys           =  57;        lwpCondWait      = 170;
      fdsync           =  58;        lwpCondSignal    = 171;
      execve           =  59;        lwpCondBroadcast = 172;
      umask            =  60;        pread            = 173;
      chroot           =  61;        pwrite           = 174;
      fcntl            =  62;        llseek           = 175;
      ulimit           =  63;        instSync         = 176;
      rmdir            =  79;        kaio             = 178;
      mkdir            =  80;        tsolsys          = 184;
      getdents         =  81;        acl              = 185;
      sysfs            =  84;        auditsys         = 186;
      getmsg           =  85;        processorBind    = 187;
      putmsg           =  86;        processorInfo    = 188;
      poll             =  87;        pOnline          = 189;
      lstat            =  88;        sigqueue         = 190;
      symlink          =  89;        clockGettime     = 191;
      readlink         =  90;        clockSettime     = 192;
      setgroups        =  91;        clockGetres      = 193;
      getgroups        =  92;        timerCreate      = 194;
      fchmod           =  93;        timerDelete      = 195;
      fchown           =  94;        timerSettime     = 196;
      sigprocmask      =  95;        timerGettime     = 197;
      sigsuspend       =  96;        timerGetoverrun  = 198;
      sigaltstack      =  97;        nanosleep        = 199;
      sigaction        =  98;        facl             = 200;
      sigpending       =  99;        door             = 201;
      context          = 100;        setreuid         = 202;
      evsys            = 101;        setregid         = 203;
      evtrapret        = 102;        installUtrap     = 204;
      statvfs          = 103;        signotifywait    = 210;
      fstatvfs         = 104;        lwpSigredirect   = 211;
      nfssys           = 106;        lwpAlarm         = 212;
      waitsys          = 107;

END Sys.
