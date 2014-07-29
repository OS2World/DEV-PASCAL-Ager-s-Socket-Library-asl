{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.4 $    $Date: 1999/07/15 07:54:48 $    $Author: sag $ }
{                                                                             }
{ Converted from nerrno.h found on the OS/2 Warp 4 CD                         }
UNIT NErrNo;

{&OrgName+,Use32-,Open32-}

INTERFACE

{#ifndef __NERRNO_32H
#define __NERRNO_32H}

{
 * The re-defination of error constants are necessary to avoid conflict with
 * standard IBM C Set/2 V1.0 error constants.
 *
 * All OS/2 SOCKET API error constants are biased by SOCBASEERR from the "normal"
 *
 }
CONST
  SOCBASEERR             = 10000;

{
 * OS/2 SOCKET API definitions of regular Microsoft C 6.0 error constants
 }

  SOCEPERM               = (SOCBASEERR+1);             { Not owner }
  SOCESRCH               = (SOCBASEERR+3);             { No such process }
  SOCEINTR               = (SOCBASEERR+4);             { Interrupted system call }
  SOCENXIO               = (SOCBASEERR+6);             { No such device or address }
  SOCEBADF               = (SOCBASEERR+9);             { Bad file number }
  SOCEACCES              = (SOCBASEERR+13);            { Permission denied }
  SOCEFAULT              = (SOCBASEERR+14);            { Bad address }
  SOCEINVAL              = (SOCBASEERR+22);            { Invalid argument }
  SOCEMFILE              = (SOCBASEERR+24);            { Too many open files }
  SOCEPIPE               = (SOCBASEERR+32);            { Broken pipe }

  SOCEOS2ERR             = (SOCBASEERR+100);            { OS/2 Error }

{
 * OS/2 SOCKET API definitions of regular BSD error constants
 }

  SOCEWOULDBLOCK         = (SOCBASEERR+35);            { Operation would block }
  SOCEINPROGRESS         = (SOCBASEERR+36);            { Operation now in progress }
  SOCEALREADY            = (SOCBASEERR+37);            { Operation already in progress }
  SOCENOTSOCK            = (SOCBASEERR+38);            { Socket operation on non-socket }
  SOCEDESTADDRREQ        = (SOCBASEERR+39);            { Destination address required }
  SOCEMSGSIZE            = (SOCBASEERR+40);            { Message too long }
  SOCEPROTOTYPE          = (SOCBASEERR+41);            { Protocol wrong type for socket }
  SOCENOPROTOOPT         = (SOCBASEERR+42);            { Protocol not available }
  SOCEPROTONOSUPPORT     = (SOCBASEERR+43);            { Protocol not supported }
  SOCESOCKTNOSUPPORT     = (SOCBASEERR+44);            { Socket type not supported }
  SOCEOPNOTSUPP          = (SOCBASEERR+45);            { Operation not supported on socket }
  SOCEPFNOSUPPORT        = (SOCBASEERR+46);            { Protocol family not supported }
  SOCEAFNOSUPPORT        = (SOCBASEERR+47);            { Address family not supported by protocol family }
  SOCEADDRINUSE          = (SOCBASEERR+48);            { Address already in use }
  SOCEADDRNOTAVAIL       = (SOCBASEERR+49);            { Can't assign requested address }
  SOCENETDOWN            = (SOCBASEERR+50);            { Network is down }
  SOCENETUNREACH         = (SOCBASEERR+51);            { Network is unreachable }
  SOCENETRESET           = (SOCBASEERR+52);            { Network dropped connection on reset }
  SOCECONNABORTED        = (SOCBASEERR+53);            { Software caused connection abort }
  SOCECONNRESET          = (SOCBASEERR+54);            { Connection reset by peer }
  SOCENOBUFS             = (SOCBASEERR+55);            { No buffer space available }
  SOCEISCONN             = (SOCBASEERR+56);            { Socket is already connected }
  SOCENOTCONN            = (SOCBASEERR+57);            { Socket is not connected }
  SOCESHUTDOWN           = (SOCBASEERR+58);            { Can't send after socket shutdown }
  SOCETOOMANYREFS        = (SOCBASEERR+59);            { Too many references: can't splice }
  SOCETIMEDOUT           = (SOCBASEERR+60);            { Connection timed out }
  SOCECONNREFUSED        = (SOCBASEERR+61);            { Connection refused }
  SOCELOOP               = (SOCBASEERR+62);            { Too many levels of symbolic links }
  SOCENAMETOOLONG        = (SOCBASEERR+63);            { File name too long }
  SOCEHOSTDOWN           = (SOCBASEERR+64);            { Host is down }
  SOCEHOSTUNREACH        = (SOCBASEERR+65);            { No route to host }
  SOCENOTEMPTY           = (SOCBASEERR+66);            { Directory not empty }

{
 * OS/2 SOCKET API errors redefined as regular BSD error constants
 }

  EWOULDBLOCK            = SOCEWOULDBLOCK;
  EINPROGRESS            = SOCEINPROGRESS;
  EALREADY               = SOCEALREADY;
  ENOTSOCK               = SOCENOTSOCK;
  EDESTADDRREQ           = SOCEDESTADDRREQ;
  EMSGSIZE               = SOCEMSGSIZE;
  EPROTOTYPE             = SOCEPROTOTYPE;
  ENOPROTOOPT            = SOCENOPROTOOPT;
  EPROTONOSUPPORT        = SOCEPROTONOSUPPORT;
  ESOCKTNOSUPPORT        = SOCESOCKTNOSUPPORT;
  EOPNOTSUPP             = SOCEOPNOTSUPP;
  EPFNOSUPPORT           = SOCEPFNOSUPPORT;
  EAFNOSUPPORT           = SOCEAFNOSUPPORT;
  EADDRINUSE             = SOCEADDRINUSE;
  EADDRNOTAVAIL          = SOCEADDRNOTAVAIL;
  ENETDOWN               = SOCENETDOWN;
  ENETUNREACH            = SOCENETUNREACH;
  ENETRESET              = SOCENETRESET;
  ECONNABORTED           = SOCECONNABORTED;
  ECONNRESET             = SOCECONNRESET;
  ENOBUFS                = SOCENOBUFS;
  EISCONN                = SOCEISCONN;
  ENOTCONN               = SOCENOTCONN;
  ESHUTDOWN              = SOCESHUTDOWN;
  ETOOMANYREFS           = SOCETOOMANYREFS;
  ETIMEDOUT              = SOCETIMEDOUT;
  ECONNREFUSED           = SOCECONNREFUSED;
  ELOOP                  = SOCELOOP;
  ENAMETOOLONG           = SOCENAMETOOLONG;
  EHOSTDOWN              = SOCEHOSTDOWN;
  EHOSTUNREACH           = SOCEHOSTUNREACH;
  ENOTEMPTY              = SOCENOTEMPTY;

{#endif   __NERRNO_32H }

IMPLEMENTATION

END.

