{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.5 $    $Date: 1999/07/15 07:55:50 $    $Author: sag $ }
{                                                                             }
{ Converted from tcps.c found on the OS/2 Warp 4 CD                           }
PROGRAM tcps;
{*******************************************************copyrite.xic**}
{                                                                     }
{   Licensed Materials - Property of IBM                              }
{                                                                     }
{   This product contains "Restricted Materials of IBM":              }
{      Program Number:   5798RXW                                      }
{      Program Name:     IBM TCP/IP Version 1.2 for OS/2              }
{   (C) Copyright IBM Corporation. 1990, 1991.                        }
{                                                                     }
{   All rights reserved.                                              }
{                                                                     }
{   US Government Users Restricted Rights -                           }
{   Use, duplication or disclosure restricted by GSA ADP Schedule     }
{   Contract with IBM Corp.                                           }
{                                                                     }
{   See IBM Copyright Instructions.                                   }
{                                                                     }
{*******************************************************copyrite.xic**}
{ Include Files.
#include <stdio.h>
#include <stdlib.h>
#include <types.h>
#include <netinet\in.h>
#include <sys\socket.h> }

{$I aslDefine.Inc}
{&PMType VIO}

USES SysUtils, CTypes, aslSocket,
{$IFDEF OS2}
     OS2Socket, NetDB, SockIn, Utils;
{$ELSE}
     Winsock;
{$ENDIF}

VAR
  port: ushort;               { port server binds to                  }
  buf: ARRAY[0..12] of Char;  { buffer for sending and receiving data }
  client: sockaddr_in;        { client address information            }
  server: sockaddr_in;        { server address information            }
  s: int;                     { socket for accepting connections      }
  ns: int;                    { socket connected to client            }
  namelen: int;               { length of client name                 }

{ Server Main. }
BEGIN
  { Check arguments. Should be only one: the port number to bind to. }
  IF ParamCount<>1 THEN
  BEGIN
    WriteLn('Usage: '+ParamStr(0)+' port');
    Halt(1);
  END;

  { Initialize with sockets. }
//  sock_init;

  { First argument should be the port. }
  port:=StrToInt(ParamStr(1));

  { Get a socket for accepting connections. }
  s:=SockSocket(PF_INET, SOCK_STREAM, 0);
  if s<0 THEN
  BEGIN
    Sockpsock_errno('Socket()');
    Halt(2);
  END;

  { Bind the socket to the server address. }
  server.sin_family:=AF_INET;
  server.sin_port:=htons(port);
  server.sin_addr.s_addr:=INADDR_ANY;

  if SockBind(s, sockaddr(server), sizeof(server)) < 0 THEN
  BEGIN
    Sockpsock_errno('Bind()');
    Halt(3);
  END;

  { Listen for connections. Specify the backlog as 1. }
  IF SockListen(s, 1) <> 0 THEN
  BEGIN
    Sockpsock_errno('Listen()');
    Halt(4);
  END;

  { Accept a connection. }
  namelen:=sizeof(client);
  ns:=SockAccept(s, sockaddr(client), namelen);
  if ns=-1 THEN
  BEGIN
    Sockpsock_errno('Accept()');
    Halt(5);
  END;
  WriteLn('Connect from: ',inet_ntoa(client.sin_addr));

  { Receive the message on the newly connected socket. }
  FillChar(Buf, SizeOf(Buf), 0);
  if SockRecv(ns, buf, sizeof(buf), 0)=-1 THEN
  BEGIN
    Sockpsock_errno('Recv()');
    Halt(6);
  END;
  WriteLn('Got: ',Buf);

  { Send the message back to the client. }
  if SockSend(ns, buf, sizeof(buf), 0) < 0 THEN
  BEGIN
    Sockpsock_errno('Send()');
    Halt(7);
  END;

  SockClose(ns);
  SockClose(s);

  WriteLn('Server ended successfully');
END.

