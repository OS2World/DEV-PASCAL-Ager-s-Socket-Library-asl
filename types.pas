{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.5 $    $Date: 2002/04/28 20:01:41 $    $Author: sag $ }
{                                                                             }
{ Converted from types.h found on the OS/2 Warp 4 CD                          }
UNIT Types;

{&OrgName+,Use32-,Open32-,Comments-}

INTERFACE

USES CTypes;

{#ifndef __TYPES_32H
#define __TYPES_32H}

{#define INET}
TYPE
  u_long = LongInt;
  u_short = Integer;
  ushort = Integer;
  u_int = int;
  u_char = byte;

{#ifndef __off_t}
  off_t = long;
{#define __off_t
#endif { off_t }

  caddr_t = PChar;
  daddr_t = long;
{#define NIL ((char *) 0)}

CONST
  PZERO = 0;
  BSD   = 43;

{#include <errno.h>
#include <nerrno.h>
#include <sys/time.h>
#include <utils.h>}

  MAXHOSTNAMELEN = 120;
{#ifndef MAXPATHLEN
#define MAXPATHLEN 80
#endif}
  MAXSOCKETS = 2048;

{$ifndef X11}     { defined by Xos.h }
//#define index(string,c) strchr((string),(c))
{$endif}

CONST
{
#define TRUE 1
#define FALSE 0}
  SIGALRM = 0;

{ #endif  __TYPES_32H }

IMPLEMENTATION

END.
