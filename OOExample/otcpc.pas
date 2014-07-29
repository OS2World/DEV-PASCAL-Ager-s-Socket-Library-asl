{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.3 $    $Date: 2002/10/01 01:07:32 $    $Author: sag $ }
{                                                                             }
{ Converted to oop from tcpc.c found on the OS/2 Warp 4 CD                    }
PROGRAM tcpc;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  SysUtils, CTypes, aslTCPSocket;

VAR
  Sock : TBufTCPClientSocket;      { client socket                            }
  aBuf : STRING;

{ Client Main. }
BEGIN
  { Check Arguments Passed. Should be hostname and port. }
  IF ParamCount<>2 THEN
  BEGIN
    WriteLn('Usage: '+ParamStr(0)+' hostname port');
    Halt(1);
  END;

  Sock:=TBufTCPClientSocket.Create;

  { Put a message into the buffer. }
  aBuf:='the message';

  { Connect to the server. }
  Sock.Connect(ParamStr(1), ParamStr(2));
  WriteLn('Connected to: ',Sock.PeerName);

  Sock.WriteLn(aBuf);

  { The server sends back the same message. Receive it into the buffer. }
  Sock.ReadLn(aBuf);
  WriteLn('Got: ',aBuf);

  { Close the socket. }
  Sock.Destroy;

  WriteLn('Client Ended Successfully');
END.

