{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.14 $    $Date: 2002/03/29 08:02:58 $    $Author: sag $ }
{                                                                             }
{ Post Office Protocol - Version 3 (POP3) client class (STD 53)               }
UNIT aslPOP3Client;

{$I aslDefine.Inc}

INTERFACE

USES Classes, SysUtils, aslAbsSocket, aslAbsClient;

CONST
  wTrue  : Word = Word(True);
  wFalse : Word = Word(False);

TYPE
  EAbsPOP3Client = CLASS(EaslException);

  TAbsPOP3Client = CLASS(TAbsClient)
  PROTECTED
    PROCEDURE GetResponse(Extra: TStringList); OVERRIDE;
  PUBLIC
    CONSTRUCTOR Create;
    PROCEDURE Connect(Host: STRING); OVERRIDE;

{ Standard commands }
    PROCEDURE User(Name: String);
    PROCEDURE Pass(Password: String);
    PROCEDURE Stat(VAR Msgs, Size: Word);
    PROCEDURE List(MsgInfo: TStringList);
    FUNCTION  ListOneMsg(MsgNum: Word): Word;
    PROCEDURE Retr(MsgNum: Word; Msg: TStringList);
    PROCEDURE Dele(MsgNum: Word);
    PROCEDURE Noop;
    PROCEDURE Rset;
    PROCEDURE Quit;

{ Optional commands }
    PROCEDURE Top(MsgNum, Lines: Word; MsgLines: TStringList);
    PROCEDURE Uidl(MsgInfo: TStringList);
    PROCEDURE UidlOneMsg(MsgNum: Word);
    PROCEDURE Apop(Name, MD5Digest: STRING);
  END;

  TPOP3Client = CLASS(TAbsPOP3Client)
  PRIVATE
    FMsgs    : Word;
    FSize    : Word;
    FDelAfter: Boolean;
  PUBLIC
    CONSTRUCTOR Create;
    PROCEDURE Connect(Host, Name, Password: STRING);
    PROCEDURE Disconnect; OVERRIDE;

    PROCEDURE GetMsg(MsgNum: Word; Msg: TStringList);

    PROPERTY Msgs: Word        READ FMsgs;
    PROPERTY Size: Word        READ FSize;
    PROPERTY DelAfter: Boolean READ FDelAfter WRITE FDelAfter;
  END;

IMPLEMENTATION

{ TAbsPOP3Client }

  CONSTRUCTOR TAbsPOP3Client.Create;
  BEGIN
    INHERITED Create;
    Service:='pop3';
  END;

  PROCEDURE TAbsPOP3Client.Connect(Host: STRING);
  BEGIN
    INHERITED Connect(Host);
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Connect', LastResponse);
  END;

  PROCEDURE TAbsPOP3Client.GetResponse(Extra: TStringList);
  BEGIN
    INHERITED GetResponse(Extra);
    IF Copy(LastResponse,1,3)='+OK' THEN
      LastResponseCode:=wTrue
    ELSE
      LastResponseCode:=wFalse;
  END;


  PROCEDURE TAbsPOP3Client.User(Name: STRING);
  BEGIN
    SendCommand('USER '+Name);
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('User', LastResponse);
  END;

  PROCEDURE TAbsPOP3Client.Pass(Password: STRING);
  BEGIN
    SendCommand('PASS '+Password);
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Pass', LastResponse);
  END;

  PROCEDURE TAbsPOP3Client.Stat(VAR Msgs, Size: Word);
  VAR
    S: String;
    i: Integer;
  BEGIN
    Msgs:=0; Size:=0;
    SendCommand('STAT');
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Stat', LastResponse);
    S:=Copy(LastResponse,5,Length(LastResponse)-4);
    i:=Pos(' ',S);
    Msgs:=StrToInt(Copy(S,1,i-1));
    Size:=StrToInt(Copy(S,i+1,Length(s)-i));
  END;

  PROCEDURE TAbsPOP3Client.List(MsgInfo: TStringList);
  BEGIN
    SendCommand('LIST');
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('List', LastResponse);
    GetMsgLines(MsgInfo)
  END;

  FUNCTION TAbsPOP3Client.ListOneMsg(MsgNum: Word): Word;
  VAR
    S: STRING;
    i: Integer;
  BEGIN
    SendCommand('LIST '+IntToStr(MsgNum));
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('ListOneMsg', LastResponse);
    S:=Copy(LastResponse,5,Length(LastResponse)-4);
    i:=Pos(' ',S);
    Result:=StrToInt(Copy(S,i+1,Length(s)-i));
  END;

  PROCEDURE TAbsPOP3Client.Retr(MsgNum: Word; Msg: TStringList);
  BEGIN
    SendCommand('RETR '+IntToStr(MsgNum));
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Retr', LastResponse);
    GetMsgLines(Msg);
  END;

  PROCEDURE TAbsPOP3Client.Dele(MsgNum: Word);
  BEGIN
    SendCommand('DELE '+IntToStr(MsgNum));
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Dele', LastResponse);
  END;

  PROCEDURE TAbsPOP3Client.Noop;
  BEGIN
    SendCommand('NOOP');
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Noop', LastResponse);
  END;

  PROCEDURE TAbsPOP3Client.Rset;
  BEGIN
    SendCommand('RSET');
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Rset', LastResponse);
  END;

  PROCEDURE TAbsPOP3Client.Quit;
  BEGIN
    SendCommand('QUIT');
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Quit', LastResponse);
  END;


  PROCEDURE TAbsPOP3Client.Top(MsgNum, Lines: Word; MsgLines: TStringList);
  BEGIN
    SendCommand('TOP '+IntToStr(MsgNum)+' '+IntToStr(Lines));
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Top', LastResponse);
    GetMsgLines(MsgLines);
  END;

  PROCEDURE TAbsPOP3Client.Uidl(MsgInfo: TStringList);
  BEGIN
    SendCommand('UIDL');
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Uidl', LastResponse);
    GetMsgLines(MsgInfo)
  END;

  PROCEDURE TAbsPOP3Client.UidlOneMsg(MsgNum: Word);
  BEGIN
    SendCommand('UIDL '+IntToStr(MsgNum));
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('UidlOneMsg', LastResponse);
  END;

  PROCEDURE TAbsPOP3Client.Apop(Name, MD5Digest: STRING);
  BEGIN
    SendCommand('APOP '+Name+' '+MD5Digest);
    IF LastResponseCode<>wTrue THEN
      RAISE EAbsPOP3Client.Create('Apop', LastResponse);
  END;


{ TPOP3Client }

  CONSTRUCTOR TPOP3Client.Create;
  BEGIN
    INHERITED Create;
    FMsgs:=0;
    FSize:=0;
  END;

  PROCEDURE TPOP3Client.Connect(Host, Name, Password: STRING);
  BEGIN
    INHERITED Connect(Host);
    IF Name<>'' THEN User(Name);
    IF Password<>'' THEN Pass(Password);
    Stat(FMsgs, FSize);
  END;

  PROCEDURE TPOP3Client.Disconnect;
  BEGIN
    Quit;
    INHERITED Disconnect;
    FMsgs:=0;
    FSize:=0;
  END;

  PROCEDURE TPOP3Client.GetMsg(MsgNum: Word; Msg: TStringList);
  BEGIN
    Retr(MsgNum, Msg);
// Check msg received OK!
    IF FDelAfter THEN Dele(MsgNum);
  END;

END.

