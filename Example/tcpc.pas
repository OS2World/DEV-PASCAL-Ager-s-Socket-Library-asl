{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.6 $    $Date: 1999/07/15 07:55:50 $    $Author: sag $ }
{                                                                             }
{ Converted from tcpc.c found on the OS/2 Warp 4 CD                           }
PROGRAM tcpc;
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
#include <sys\socket.h>
#include <netdb.h> }

{$I aslDefine.Inc}
{&PMType VIO}

USES SysUtils, CTypes, aslSocket,
{$IFDEF OS2}
     OS2Socket, NetDB, SockIn, Utils;
{$ELSE}
     Winsock;
{$ENDIF}

VAR
  port: ushort;                 { port client will connect to              }
  buf : ARRAY[0..20] Of Char;   { data buffer for sending and receiving    }
  Tmp : ARRAY[0..20] Of Char;
  hostnm: phostent;             { server host name information             }
  server: sockaddr_in;          { server address                           }
  s: int;                       { client socket                            }

{ Client Main. }
BEGIN
  { Check Arguments Passed. Should be hostname and port. }
  IF ParamCount<>2 THEN
  BEGIN
    WriteLn('Usage: '+ParamStr(0)+' hostname port');
    Halt(1);
  END;

  { Initialize with sockets. }
//  sock_init;      // Done in aslSocket

  { The host name is the first argument. Get the server address. }
//  StrPCopy(@Tmp, ParamStr(1));
  hostnm:=SockGetHostByName(ParamStr(1));
  IF hostnm=nil THEN
  BEGIN
    WriteLn('Gethostbyname failed');
    Halt(2);
  END;

  { The port is the second argument. }
  port:=StrToInt(ParamStr(2));

  { Put a message into the buffer. }
  StrCopy(buf, 'the message');

  { Put the server information into the server structure.
    The port must be put into network byte order. }
  server.sin_family:=AF_INET;
  server.sin_port:=htons(port);
  server.sin_addr:=hostnm^.h_addr^^;

  { Get a stream socket. }
  s:=SockSocket(PF_INET, SOCK_STREAM, 0);
  IF s<0 THEN
  BEGIN
    Sockpsock_errno('Socket()');
    Halt(3);
  END;

  { Connect to the server. }
  WriteLn('Connecting to: ',inet_ntoa(hostnm^.h_addr^^));
  IF SockConnect(s, sockaddr(server), sizeof(server)) < 0 THEN
  BEGIN
    Sockpsock_errno('Connect()');
    Halt(4);
  END;

  IF SockSend(s, buf, sizeof(buf), 0) < 0 THEN
  BEGIN
    Sockpsock_errno('Send()');
    Halt(5);
  END;

  { The server sends back the same message. Receive it into the buffer. }
  FillChar(Buf, SizeOf(Buf), 0);
  IF SockRecv(s, buf, sizeof(buf), 0) < 0 THEN
  BEGIN
    Sockpsock_errno('Recv()');
    Halt(6);
  END;
  WriteLn('Got: ',Buf);

  { Close the socket. }
  SockClose(s);

  WriteLn('Client Ended Successfully');
END.

