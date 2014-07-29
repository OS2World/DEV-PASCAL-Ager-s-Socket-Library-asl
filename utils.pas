{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.4 $    $Date: 1999/07/15 07:54:48 $    $Author: sag $ }
{                                                                             }
{ Converted from utils.h found on the OS/2 Warp 4 CD                          }
UNIT Utils;

{&OrgName+,Use32-,Open32-}

INTERFACE

USES CTypes;

{#ifndef __UTILS_32H
#define __UTILS_32H}

{&CDECL+}
FUNCTION lswap(a: ulong): ulong;
FUNCTION bswap(a: ushort): ushort;
FUNCTION rexec(host: pchar; port: int; user: pchar; passwd: pchar; cmd: pchar; var err_sd2p: int): int;
{ int _System getpid(void); }
{&CDECL-}

{ Definition for bswap }
FUNCTION htonl(x: ulong): ulong; INLINE; BEGIN htonl:=lswap(x); END;
FUNCTION ntohl(x: ulong): ulong; INLINE; BEGIN ntohl:=lswap(x); END;
FUNCTION htons(x: ushort): ushort; INLINE; BEGIN htons:=bswap(x); END;
FUNCTION ntohs(x: ushort): ushort; INLINE; BEGIN ntohs:=bswap(x); END;

(*
#ifndef X11
#define bzero(x,y) memset((x),'\0',(y))
#define bcopy(x,y,z) memcpy((y),(x),(z))
#define bcmp(x,y,z)  memcmp((y),(x),(z))
#endif { X11 }
#define ovbcopy(x,y,z) bcopy((x),(y),(z))
#define copyout(x,y,z) memcpy((y),(x),(z))
#define strcasecmp(x,y) strcmpi((x),(y))
#define strncasecmp(x,y,z) strnicmp(x,y,z)
#define sleep(x) DosSleep(((long)(x))*1000L)

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#define imin(x,y) MIN((x),(y))
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a,b) (((a)<(b))?(a):(b))
#endif

#define timercmp(t1,t2,op) (((t1)->tv_sec op (t2)->tv_sec) || \
                           (((t1)->tv_sec == (t2)->tv_sec) \
                           && ((t1)->tv_usec op (t2)->tv_usec)))
#define random() ((unsigned long)rand())
#define srandom(x) srand(x)
*)
{#endif  __UTILS_32H }

IMPLEMENTATION

{&CDECL+}
FUNCTION lswap;         EXTERNAL;
FUNCTION bswap;         EXTERNAL;
FUNCTION rexec;         EXTERNAL;
{&CDECL-}

END.

