{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.3 $    $Date: 2002/10/01 01:07:32 $    $Author: sag $ }
{                                                                             }
{ Converted to oop from tcps.c found on the OS/2 Warp 4 CD                    }
PROGRAM tcps;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  SysUtils, CTypes, aslTCPSocket;

VAR
  s   : TTCPServerSocket;     { socket for accepting connections      }
  ns  : TTCPClientSocket;     { socket connected to client            }
  Buf : STRING;               { buffer for sending and receiving data }

{ Server Main. }
BEGIN
  { Check arguments. Should be only one: the port number to bind to. }
  IF ParamCount<>1 THEN
  BEGIN
    WriteLn('Usage: '+ParamStr(0)+' port');
    Halt(1);
  END;

  { First argument should be the port. }
  { Get a socket for accepting connections. }
  s:=TTCPServerSocket.Create(ParamStr(1));

  { Listen for connections. Specify the backlog as 1. }
  s.Listen;

  { Accept a connection. }
  ns:=s.AcceptConnection;
  WriteLn('Connect from: ',ns.PeerName);

  { Receive the message on the newly connected socket. }
  ns.ReadLn(Buf);
  WriteLn('Got: ',Buf);

  { Send the message back to the client. }
  ns.WriteLn(Buf);

  ns.Destroy;
  s.Destroy;

  WriteLn('Server ended successfully');
END.

