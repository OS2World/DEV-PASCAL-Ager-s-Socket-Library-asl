{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.3 $    $Date: 2002/10/01 01:07:33 $    $Author: sag $ }
{                                                                             }
{ Simple Mail Transfer Protocol (SMTP) minimal server example                 }
PROGRAM SMTPServer;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  Classes, aslGlobals, aslTCPSocket, aslAbsServer, aslSMTPServer;

TYPE
  TMySMTPServer = CLASS(TSMTPServer)
    FUNCTION StoreMessage(ASender: String; ARecipiants, AMessage: TStringList): Boolean; OVERRIDE;
  END;


  FUNCTION TMySMTPServer.StoreMessage(ASender: String; ARecipiants, AMessage: TStringList): Boolean;
  BEGIN
    Result:=True;
  END;

VAR
  S: TServerManager;

  FUNCTION CreateSMTP(ASvrMgr: TServermanager; ACliSocket: TTCPClientSocket): TAbsServer;
  BEGIN
    CreateSMTP:=TMySMTPServer.Create(ASvrMgr, ACliSocket, 'MY.DOMAIN.DK', 'SMTP server. '+aslPartOf+' R'+aslRelease);
  END;

BEGIN
  S:=TServerManager.Create('smtp', CreateSMTP);
  S.Execute;
  S.Destroy;
END.
