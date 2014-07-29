{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.1 $    $Date: 1999/04/14 18:33:21 $    $Author: sag $ }
{                                                                             }
{ Simple Mail Transfer Protocol (SMTP) server class (RFC 821)                 }
UNIT aslSMTPServer;

{$I aslDefine.Inc}

INTERFACE

USES Classes, aslAbsServer, aslTCPSocket;

CONST
{ Mandatory commands }
  cmd_smtpHelo =  1;
  cmd_smtpMail =  2;
  cmd_smtpRcpt =  3;
  cmd_smtpData =  4;
  cmd_smtpRset =  5;
  cmd_smtpNoop =  6;
{ Optional commands }

{ Please add your own commands after this one (100 and up) }
  cmd_smtpQuit = 99;


{ POP3 server states }
  st_smtpAuth     =  0;
  st_smtpTrans    =  1;
  st_smtpData     =  2;

  st_smtpReserved = 99;

TYPE
  TSMTPServer = CLASS(TAbsTextServer)
  PRIVATE
    FState          : Integer;
    FServerName,
    FGreeting,
    FSenderDomain,
    FSender         : String;
    FRecipients     : TStringList;
    FMessage        : TStringList;
  PROTECTED
    PROCEDURE cmdHelo(VAR Message: TaslMessage); MESSAGE cmd_smtpHelo;
    PROCEDURE cmdMail(VAR Message: TaslMessage); MESSAGE cmd_smtpMail;
    PROCEDURE cmdRcpt(VAR Message: TaslMessage); MESSAGE cmd_smtpRcpt;
    PROCEDURE cmdData(VAR Message: TaslMessage); MESSAGE cmd_smtpData;
    PROCEDURE cmdRset(VAR Message: TaslMessage); MESSAGE cmd_smtpRSet;
    PROCEDURE cmdNoop(VAR Message: TaslMessage); MESSAGE cmd_smtpNoop;
    PROCEDURE cmdQuit(VAR Message: TaslMessage); MESSAGE cmd_smtpQuit;
  PUBLIC
    CONSTRUCTOR Create(ASvrMgr: TServerManager; AClientSocket: TTCPClientSocket; AServerName, AGreeting: String);
    DESTRUCTOR Destroy; OVERRIDE;

    PROCEDURE SendGreeting; OVERRIDE;
    PROCEDURE SendSignOff; OVERRIDE;
    PROCEDURE SendUnknownCmd(ACmd: String); OVERRIDE;

    PROCEDURE DoReset; VIRTUAL;
    FUNCTION  VerifySenderDomain(ADomain: String): Boolean; VIRTUAL;
    FUNCTION  ValidateRecipient(ARecipient: String): Boolean; VIRTUAL;
    FUNCTION  StoreMessage(ASender: String; ARecipiants, AMessage: TStringList): Boolean; VIRTUAL; ABSTRACT;
  END;


IMPLEMENTATION

  CONSTRUCTOR TSMTPServer.Create(ASvrMgr: TServerManager; AClientSocket: TTCPClientSocket; AServerName, AGreeting: String);
  BEGIN
    FServerName:=AServerName;
    FGreeting:=AGreeting;
    FState:=st_smtpAuth;
    FSenderDomain:='';
    FSender:='';
    FRecipients:=TStringList.Create;
    FMessage:=TStringList.Create;
    INHERITED Create(ASvrMgr, AClientSocket);
    CmdColl.Add('HELO',       cmd_smtpHelo);
    CmdColl.Add('MAIL FROM:', cmd_smtpMail);
    CmdColl.Add('RCPT TO:',   cmd_smtpRcpt);
    CmdColl.Add('DATA',       cmd_smtpData);
    CmdColl.Add('RSET',       cmd_smtpRset);
    CmdColl.Add('NOOP',       cmd_smtpNoop);

    CmdColl.Add('QUIT',       cmd_smtpQuit);
  END;

  DESTRUCTOR TSMTPServer.Destroy;
  BEGIN
    FRecipients.Destroy;
    FMessage.Destroy;
    INHERITED Destroy;
  END;

  PROCEDURE TSMTPServer.SendGreeting;
  BEGIN
    CliSocket.WriteLn('220 '+FServerName+' '+FGreeting+' ready');
  END;

  PROCEDURE TSMTPServer.SendSignOff;
  BEGIN
    CliSocket.WriteLn('221 '+FServerName+' '+FGreeting+' signing off');
  END;

  PROCEDURE TSMTPServer.SendUnknownCmd(ACmd: String);
  BEGIN
    CliSocket.WriteLn('502 Unknown command: '+ACmd);
  END;


  PROCEDURE TSMTPServer.DoReset;
  BEGIN
    FState:=st_smtpTrans;
    FSender:='';
    FRecipients.Clear;
    FMessage.Clear;
  END;

  FUNCTION TSMTPServer.VerifySenderDomain(ADomain: String): Boolean;
  BEGIN
    VerifySenderDomain:=True;
  END;

  FUNCTION TSMTPServer.ValidateRecipient(ARecipient: String): Boolean;
  BEGIN
    ValidateRecipient:=True;
  END;


  PROCEDURE TSMTPServer.cmdHelo(VAR Message: TaslMessage);
  BEGIN
    FSenderDomain:=Message.Parm;
    IF VerifySenderDomain(FSenderDomain) THEN
    BEGIN
      CliSocket.WriteLn('250 '+FServerName);
      FState:=st_smtpTrans;
    END ELSE
      CliSocket.WriteLn('501 Not allowed to talk to you');
  END;

  PROCEDURE TSMTPServer.cmdMail(VAR Message: TaslMessage);
  BEGIN
    IF FState>st_smtpAuth THEN
    BEGIN
      FSender:=Message.Parm;
      CliSocket.WriteLn('250 OK');
      IF (FSender<>'') AND (FRecipients.Count>0) THEN FState:=st_smtpData;
    END ELSE
      CliSocket.WriteLn('503 Must send HELO first');
  END;

  PROCEDURE TSMTPServer.cmdRcpt(VAR Message: TaslMessage);
  BEGIN
    IF FState>st_smtpAuth THEN
    BEGIN
      IF ValidateRecipient(Message.Parm) THEN
      BEGIN
        FRecipients.Add(Message.Parm);
        CliSocket.WriteLn('250 OK');
        IF (FSender<>'') AND (FRecipients.Count>0) THEN FState:=st_smtpData;
      END ELSE
        CliSocket.WriteLn('550 Can''t send to '+Message.Parm);
    END ELSE
      CliSocket.WriteLn('503 Must send HELO first');
  END;

  PROCEDURE TSMTPServer.cmdData(VAR Message: TaslMessage);
  BEGIN
    IF FSender='' THEN
       CliSocket.WriteLn('503 Must send MAIL FROM: first')
    ELSE
      IF FRecipients.Count=0 THEN
         CliSocket.WriteLn('503 Must send RCPT TO: first')
      ELSE
      BEGIN
        CliSocket.WriteLn('354 Send mail; end with <CRLF>.<CRLF>');
        GetMsgLines(FMessage);
        IF StoreMessage(FSender, FRecipients, FMessage) THEN
        BEGIN
          CliSocket.WriteLn('250 OK');
          DoReset;
        END ELSE
        BEGIN
          CliSocket.WriteLn('554 Can''t store message');
          FMessage.Clear;
        END;
      END;
  END;

  PROCEDURE TSMTPServer.cmdRset(VAR Message: TaslMessage);
  BEGIN
    DoReset;
    CliSocket.WriteLn('250 OK');
  END;

  PROCEDURE TSMTPServer.cmdNoop(VAR Message: TaslMessage);
  BEGIN
    CliSocket.WriteLn('250 OK');
  END;

  PROCEDURE TSMTPServer.cmdQuit(VAR Message: TaslMessage);
  BEGIN
    ClientQuit:=True;
  END;


END.
