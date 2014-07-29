{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.9 $    $Date: 2002/10/01 01:07:32 $    $Author: sag $ }
{                                                                             }
{ OO Finger Deamon example                                                    }
PROGRAM FingerD;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  VPUtils, Application, aslTCPSocket;

CONST
  Greet : String  = 'Simple Finger Daemon OO-Version'#10#13+
                    'Demo Message...';

TYPE
  TFingerServerApp = CLASS(TaslApplication)
    PROCEDURE ShowCopyright; OVERRIDE;
    PROCEDURE Run; OVERRIDE;
  END;


  PROCEDURE TFingerServerApp.ShowCopyright;
  BEGIN
    INHERITED ShowCopyright;
    WriteLn('Simple finger server demo program.');
    WriteLn;
  END;

  PROCEDURE TFingerServerApp.Run;
  VAR
    Svr    : TTCPServerSocket;
    Cli    : TTCPClientSocket;
    i      : Integer;
    Buffer : Array[0..512] of Byte;
  BEGIN
    Svr:=TTCPServerSocket.Create('finger');
    Svr.OnLogLine:=SocketLogLine;
    TRY
      REPEAT
        Svr.Listen;
        Cli:=Svr.AcceptConnection;
        Cli.OnLogLine:=SocketLogLine;
        TRY
          SocketLogLine(Self, 'Connect from: '+Svr.PeerIP+' = '+Svr.PeerName);
          Cli.WaitForData;
          i:=Cli.Read(Buffer, SizeOf(Buffer));
          SocketLogLine(Self, 'Got: '+Int2Str(i)+' bytes.');
          i:=Cli.WriteLn(Greet);
          SocketLogLine(Self, 'Sent:'+Int2Str(i)+' bytes.');
        FINALLY
          Cli.Destroy;
        END;
      UNTIL False;
    FINALLY
      Svr.Destroy;
    END;
  END;

VAR
  App: TFingerServerApp;

BEGIN
  App:=TFingerServerApp.Create('Finger server');
  App.Run;
  App.Destroy;
END.

