{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.28 $    $Date: 2002/09/29 21:53:36 $    $Author: sag $ }
{                                                                             }
{ Abstract Client class                                                       }
UNIT aslAbsClient;

{$I aslDefine.Inc}

INTERFACE

USES Classes, SysUtils, aslAbsSocket, aslTCPSocket;

TYPE
  EAbsClient = class(EaslException);

  TAbsClient = CLASS
  PRIVATE
    FHost             : STRING;
    FService          : STRING;
    FGreeting         : STRING;
    FOnLogLine        : TLogLineEvent;
    FOnReadLn         : TNotifyEvent;
    FOnWriteLn        : TNotifyEvent;
    FGetResponseAfterConnect : Boolean;

  PROTECTED
    FLastResponse     : STRING;
    FLastResponseCode : Word;
    FSocket           : TTCPClientSocket;

    PROCEDURE SendCommandNoResponse(Cmd: STRING);
    PROCEDURE SendCommand(Cmd: STRING);
    PROCEDURE SendCommandEx(Cmd: STRING; Extra: TStringList);
    PROCEDURE GetResponse(Extra: TStringList); VIRTUAL;
    PROCEDURE GetMsgLines(StringList: TStringList);
    PROCEDURE SendMsgLines(StringList: TStringList);
    PROCEDURE GetLines(StringList: TStringList);
    PROCEDURE SendLines(StringList: TStringList);
    PROCEDURE DoLogLine(Msg: STRING);
    PROCEDURE EvtReadLn(Sender: TObject);
    PROCEDURE EvtWriteLn(Sender: TObject);

  PUBLIC
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;
    PROCEDURE Connect(Host: STRING); VIRTUAL;
    PROCEDURE Disconnect; VIRTUAL;

    PROPERTY Socket: TTCPClientSocket           READ FSocket;
    PROPERTY Host: STRING                       READ FHost;
    PROPERTY Service: STRING                    READ FService          WRITE FService;
    PROPERTY Greeting: STRING                   READ FGreeting;
    PROPERTY LastResponse: STRING               READ FLastResponse     WRITE FLastResponse;
    PROPERTY LastResponseCode: Word             READ FLastResponseCode WRITE FLastResponseCode;
    PROPERTY GetResponseAfterConnect: Boolean   READ FGetResponseAfterConnect WRITE FGetResponseAfterConnect;
  { Events }
    PROPERTY OnLogLine: TLogLineEvent      READ FOnLogLine        WRITE FOnLogLine;
    PROPERTY OnReadLn: TNotifyEvent        READ FOnReadLn         WRITE FOnReadLn;
    PROPERTY OnWriteLn: TNotifyEvent       READ FOnWriteLn        WRITE FOnWriteLn;
  END;

IMPLEMENTATION


{ TAbsClient }

  CONSTRUCTOR TAbsClient.Create;
  BEGIN
    INHERITED Create;
    FSocket:=TBufTCPClientSocket.Create;
    FSocket.OnReadLn:=EvtReadLn;
    FSocket.OnWriteLn:=EvtWriteLn;
    FHost:='';
    FService:='';
    FGreeting:='';
    FLastResponse:='';
    FLastResponseCode:=0;
    FOnLogLine:=NIL;
    FOnReadLn:=NIL;
    FOnWriteLn:=NIL;
    FGetResponseAfterConnect:=True;
  END;

  DESTRUCTOR TAbsClient.Destroy;
  BEGIN
    IF Assigned(FSocket) THEN
    BEGIN
      FSocket.Destroy;
      FSocket:=Nil;
    END;
    INHERITED Destroy;
  END;

  PROCEDURE TAbsClient.Connect(Host: STRING);
  BEGIN
    IF FService='' THEN
      RAISE EAbsClient.Create('Connect: No service specified', '');
    IF NOT Assigned(FSocket) THEN
      RAISE EAbsClient.Create('Connect: Socket is not initialized (Service='+FService+')', '');
    FHost:=Host;
    FSocket.Connect(FHost, FService);
    IF FGetResponseAfterConnect THEN
    BEGIN
      GetResponse(Nil);
      FGreeting:=FLastResponse;
    END;
  END;

  PROCEDURE TAbsClient.Disconnect;
  BEGIN
    FSocket.Disconnect;
    FHost:='';
    FService:='';
    FLastResponse:='';
    FLastResponseCode:=0;
  END;


  PROCEDURE TAbsClient.SendCommandNoResponse(Cmd: STRING);
  BEGIN
    DoLogLine('<'+Cmd);
    FSocket.WriteLn(Cmd);
  END;

  PROCEDURE TAbsClient.SendCommand(Cmd: STRING);
  BEGIN
    SendCommandNoResponse(Cmd);
    GetResponse(Nil);
  END;

  PROCEDURE TAbsClient.SendCommandEx(Cmd: STRING; Extra: TStringList);
  BEGIN
    SendCommandNoResponse(Cmd);
    GetResponse(Extra);
  END;


  PROCEDURE TAbsClient.GetResponse(Extra: TStringList);
  VAR
    Ok, i : Integer;
    S     : String;
  BEGIN
    i:=FSocket.ReadLn(FLastResponse);
    IF i>0 THEN
    BEGIN
      DoLogLine('>'+FLastResponse);
      Val(Copy(FLastResponse,1,3), FLastResponseCode, Ok);

      IF Copy(FLastResponse,4,1)='-' THEN
      BEGIN
        REPEAT
          i:=FSocket.ReadLn(S);
          IF (i>0) THEN
          BEGIN
            IF Extra<>nil THEN Extra.Add(S);
            DoLogLine('>'+S);
          END;
        UNTIL (Copy(S,1,4)=(Copy(FLastResponse,1,3)+' ')) OR (i=0);
      END;
    END;
    IF i<0 THEN
      RAISE EabsClient.Create('GetResponse', '');
  END;

  PROCEDURE TAbsClient.GetMsgLines(StringList: TStringList);
  VAR
    S, S1 : STRING;
    i     : Integer;
  BEGIN
    REPEAT
      i:=FSocket.ReadLn(S);
      S1:=S;
      IF (Length(S)>1) AND (Copy(S,1,1)='.') THEN S:=Copy(S, 2, Length(S)-1);
      IF (S1<>'.') AND Assigned(StringList) THEN StringList.Add(S);
    UNTIL (S1='.') OR (i<0);
    IF i<0 THEN
      RAISE EabsClient.Create('GetResponse', '');
  END;

  PROCEDURE TAbsClient.SendMsgLines(StringList: TStringList);
  VAR
    i : Integer;
  BEGIN
    IF Assigned(StringList) THEN
      FOR i:=0 TO StringList.Count-1 DO
        IF (StringList[i]<>'') AND (Copy(StringList[i],1,1)='.') THEN
          Socket.WriteLn('.'+StringList[i])
        ELSE
          Socket.WriteLn(StringList[i]);
  END;

  PROCEDURE TAbsClient.GetLines(StringList: TStringList);
//!! Check where used and why timeout needed?
  VAR
    S : STRING;
    i : Integer;
  BEGIN
    WHILE FSocket.WaitForDataTime(1000)>0 DO
    BEGIN
      i:=FSocket.ReadLn(S);
      IF i>0 THEN
      BEGIN
        DoLogLine('>'+S);
        IF Assigned(StringList) THEN StringList.Add(S);
      END;
    END;
    IF i<0 THEN
      RAISE EabsClient.Create('GetLines', '');
  END;

  PROCEDURE TAbsClient.SendLines(StringList: TStringList);
  VAR
    i : Integer;
  BEGIN
    IF Assigned(StringList) THEN
      FOR i:=0 TO StringList.Count-1 DO
      Socket.WriteLn(StringList[i]);
  END;

  PROCEDURE TAbsClient.DoLogLine(Msg: STRING);
  BEGIN
    IF Assigned(FOnLogLine) THEN FOnLogLine(Self,Msg);
  END;

  PROCEDURE TAbsClient.EvtReadLn(Sender: TObject);
  BEGIN
    IF Assigned(FOnReadLn) THEN FOnReadLn(Self);
  END;

  PROCEDURE TAbsClient.EvtWriteLn(Sender: TObject);
  BEGIN
    IF Assigned(FOnWriteLn) THEN FOnWriteLn(Self);
  END;

END.


