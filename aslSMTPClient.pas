{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.18 $    $Date: 2002/10/02 01:13:10 $    $Author: sag $ }
{                                                                             }
{ Simple Mail Transfer Protocol (SMTP) client class (RFC 821)                 }
{ EHLO extension as per RFC 1869                                              }
{ SIZE extension as per RFC 1870                                              }
{ ETRN extension as per RFC 1985                                              }
{ Message format as per RFC 822                                               }
{                                                                             }
{ Parts copyright (c) Dwayne Heaton (dwayne@vmadd.demon.co.uk)                }
{ HTML mods by Kevin McCoy, IDK, Inc. 2002/04/28; kgmccoy@idk-inc.com         }

UNIT aslSMTPClient;

{$I aslDefine.Inc}

INTERFACE

USES Classes, SysUtils, aslAbsSocket, aslAbsClient;

CONST
  SMTPDateTemplate = 'd mmm yyyy hh:nn:ss';

TYPE
  TSMTPMessage = CLASS;
  TSMTPHTMLMessage = CLASS;

  EAbsSMTPClient = CLASS(EaslException);

  TAbsSMTPClient = CLASS(TAbsClient)
  PUBLIC
    CONSTRUCTOR Create;
    PROCEDURE Connect(Host: STRING); OVERRIDE;

{ RFC 821 commands }
    PROCEDURE Helo(Domain: STRING);
    PROCEDURE Mail(From: STRING);
    PROCEDURE Send(From: STRING);
    PROCEDURE Soml(From: STRING);
    PROCEDURE Saml(From: STRING);
    PROCEDURE Rcpt(To_: STRING);
    PROCEDURE Data;
    PROCEDURE Rset;
    PROCEDURE Vrfy(User: STRING);
    PROCEDURE Expn(MailingList: STRING; Members: TStringList);
    PROCEDURE Help(Topic: STRING; HelpText: TStringList);
    PROCEDURE Noop;
    PROCEDURE Turn;
    PROCEDURE Quit;

{ Extensions }
    PROCEDURE Ehlo(Domain: STRING; ExtraCmds: TStringList);
  END;

  TSMTPClient = CLASS(TAbsSMTPClient)
  PRIVATE
    FDomain : STRING;
    FCmds   : TStringList;
  PUBLIC
    CONSTRUCTOR Create;
    PROCEDURE Connect(Host, Domain: STRING);
    PROCEDURE Disconnect; OVERRIDE;

    PROCEDURE SendMsg(Msg: TSMTPMessage);
    PROCEDURE SendHTMLMsg(Msg: TSMTPHTMLMessage);

    PROPERTY Domain: STRING     READ FDomain WRITE FDomain;
  END;

  TEncode = (encNone, enc64, encUU, encXX);
  PEncode = ^TEncode;
  TAttachments = CLASS
  PRIVATE
    FFiles  : TStringList;
    FEncode : Array[1..50] of PEncode;
    FNum    : LongInt;
    FMime   : Boolean;
  PUBLIC
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;

    PROCEDURE Add(Filename : String; Encode : TEncode);
    FUNCTION Count : LongInt;
    FUNCTION Get(Index : LongInt; Var Filename : String; Var Encode : TEncode) : Boolean;

    PROPERTY NeedMime : Boolean READ FMime;
  END;

  TSMTPMessage = CLASS
  PRIVATE
    FHeader       : BOOLEAN;

    FFrom         : STRING;
    FTo           : TStringList;
    FSubject      : STRING;
    FMessageID    : STRING;
    FDate         : STRING;
    FReplyTo      : STRING;

    FOrganisation : STRING;
    FMimeVersion  : STRING;
    FContentType  : STRING;
    FBoundary     : STRING;

    FPriority     : WORD;
    FMSPriority   : STRING;
    FMailer       : STRING;
//  FReturnPath : STRING;
//  MimeInfo ?????

//  FRestHeader: TStrings;
    FMsgBody      : TStringList;

//  FileAttaches TFileColl???
    FileAttaches : TAttachments;

    FMimeMsg : Boolean;

  PUBLIC
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;
    PROCEDURE Clear; VIRTUAL;

    PROCEDURE AddTo(To_: STRING);
    PROCEDURE AddMsgBody(Line: STRING);
    PROCEDURE AddAttach(Filename: STRING; Encode: TEncode);
// FromFile
    PROPERTY Header: BOOLEAN       READ FHeader       WRITE FHeader;

    PROPERTY From: STRING          READ FFrom         WRITE FFrom;
    PROPERTY To_: TStringList      READ FTo           WRITE FTo;
    PROPERTY Subject: STRING       READ FSubject      WRITE FSubject;
    PROPERTY MessageID: STRING     READ FMessageID    WRITE FMessageID;
    PROPERTY Date: STRING          READ FDate         WRITE FDate;
    PROPERTY ReplyTo: STRING       READ FReplyTo      WRITE FReplyTo;

    PROPERTY Priority: WORD        READ FPriority     WRITE FPriority;
    PROPERTY MSPriority: STRING    READ FMSPriority   WRITE FMSPriority;
    PROPERTY Mailer: STRING        READ FMailer       WRITE FMailer;

    PROPERTY MimeMessage : Boolean READ FMimeMsg      WRITE FMimeMsg;
    PROPERTY Organisation: STRING  READ FOrganisation WRITE FOrganisation;
    PROPERTY MimeVersion: STRING   READ FMimeVersion  WRITE FMimeVersion;
    PROPERTY ContentType: STRING   READ FContentType  WRITE FContentType;
    PROPERTY Boundary: STRING      READ FBoundary     WRITE FBoundary;

    PROPERTY MsgBody: TStringList  READ FMsgBody      WRITE FMsgBody;
  END;

  TSMTPHTMLMessage = CLASS(TSMTPMessage)
  PRIVATE
    FHTMLBoundary: STRING;
    FHTMLMsgBody : TStringList;
  PUBLIC
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;
    PROCEDURE AddHTMLBody(Line: STRING);
    PROCEDURE Clear; OVERRIDE;

    PROPERTY HTMLMsgBody: TStringList  READ FHTMLMsgBody      WRITE FHTMLMsgBody;
    PROPERTY HTMLBoundary: STRING      READ FHTMLBoundary     WRITE FHTMLBoundary;
  END;

IMPLEMENTATION

USES aslEncode, aslMimeTypes;


{ TAbsSMTPClient }

  CONSTRUCTOR TAbsSMTPClient.Create;
  BEGIN
    INHERITED Create;
    Service:='smtp';
  END;

  PROCEDURE TAbsSMTPClient.Connect(Host: STRING);
  BEGIN
    INHERITED Connect(Host);
    IF LastResponseCode<>220 THEN
      RAISE EAbsSMTPClient.Create('Connect', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Helo(Domain: STRING);
  BEGIN
    SendCommand('HELO '+Domain);
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Helo', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Mail(From: STRING);
  BEGIN
    SendCommand('MAIL FROM: <'+From+'>');
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Mail', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Send(From: STRING);
  BEGIN
    SendCommand('SEND FROM: <'+From+'>');
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Send', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Soml(From: STRING);
  BEGIN
    SendCommand('SOML FROM: <'+From+'>');
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Soml', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Saml(From: STRING);
  BEGIN
    SendCommand('SAML FROM: <'+From+'>');
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Saml', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Rcpt(To_: STRING);
  BEGIN
    SendCommand('RCPT TO: <'+To_+'>');
    IF (LastResponseCode<>250) AND (LastResponseCode<>251) THEN
      RAISE EAbsSMTPClient.Create('Rcpt', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Data;
  BEGIN
    SendCommand('DATA');
    IF LastResponseCode<>354 THEN
      RAISE EAbsSMTPClient.Create('Data', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Rset;
  BEGIN
    SendCommand('RSET');
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Rset', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Vrfy(User: STRING);
  BEGIN
    SendCommand('VRFY '+User);
    IF (LastResponseCode<>250) AND (LastResponseCode<>251) THEN
      RAISE EAbsSMTPClient.Create('Vrfy', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Expn(MailingList: STRING; Members: TStringList);
  BEGIN
    SendCommand('EXPN '+MailingList);
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Expn', LastResponse);
    IF Assigned(Members) THEN Members.Add(LastResponse);
    GetLines(Members);
  END;

  PROCEDURE TAbsSMTPClient.Help(Topic: STRING; HelpText: TStringList);
  BEGIN
    IF Topic<>'' THEN SendCommand('HELP '+Topic) ELSE SendCommand('HELP');
    IF (LastResponseCode<>211) AND (LastResponseCode<>214) THEN
      RAISE EAbsSMTPClient.Create('Help', LastResponse);
    IF Assigned(HelpText) THEN HelpText.Add(LastResponse);
    GetLines(HelpText);
  END;

  PROCEDURE TAbsSMTPClient.Noop;
  BEGIN
    SendCommand('NOOP');
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Noop', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Turn;
  BEGIN
    SendCommand('TURN');
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Turn', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Quit;
  BEGIN
    SendCommand('QUIT');
    IF LastResponseCode<>221 THEN
      RAISE EAbsSMTPClient.Create('Quit', LastResponse);
  END;

  PROCEDURE TAbsSMTPClient.Ehlo(Domain: STRING; ExtraCmds: TStringList);
  BEGIN
    SendCommandEx('EHLO '+Domain, ExtraCmds);
    IF LastResponseCode<>250 THEN
      RAISE EAbsSMTPClient.Create('Ehlo', LastResponse);
  END;


{ TSMTPClient }

  CONSTRUCTOR TSMTPClient.Create;
  BEGIN
    INHERITED Create;
    FDomain:='';
  END;

  PROCEDURE TSMTPClient.Connect(Host, Domain: STRING);
  BEGIN
    INHERITED Connect(Host);
    FDomain:=Domain;
    TRY
      Ehlo(FDomain, FCmds);
    EXCEPT
      ON EAbsSMTPClient DO Helo(FDomain);
    ELSE
      RAISE;
    END;
  END;

  PROCEDURE TSMTPClient.Disconnect;
  BEGIN
    Quit;
    INHERITED Disconnect;
    FDomain:='';
  END;

  PROCEDURE TSMTPClient.SendMsg(Msg: TSMTPMessage);
  VAR
    i : Integer;
    S : STRING;
    E : TEncode;
    Filename : STRING;
    Content  : STRING;
    Encode : TAbsEncode;
    Mime : TMimeMgr;
  BEGIN
    Mail(Msg.From);
    FOR i:=0 TO Msg.FTo.Count-1 DO
      Rcpt(Msg.FTo[i]);
    Data;

    IF Msg.Header THEN
    BEGIN
      Socket.WriteLn('From: '+Msg.From);
      S:='To: ' + Msg.To_[0];
      FOR i:=1 TO Msg.To_.Count-1 DO
        S:=S+', '+Msg.To_[i];
      Socket.WriteLn(S);
      IF Msg.Subject<>'' THEN Socket.WriteLn('Subject: '+Msg.Subject);
      IF Msg.MessageID<>'' THEN Socket.WriteLn('Message-Id: '+Msg.MessageID);
      TimeSeparator:=':';
      IF Msg.Date<>'' THEN
        Socket.WriteLn('Date: '+Msg.Date)
      ELSE
        Socket.WriteLn('Date: '+FormatDateTime(SMTPDateTemplate, Now));
      IF Msg.ReplyTo<>'' THEN Socket.WriteLn('Reply-To: '+Msg.ReplyTo);
      IF Msg.Organisation<>'' THEN Socket.Writeln('Organisation: '+Msg.Organisation);
      IF Msg.Priority<>0 THEN Socket.Writeln('X-Priority: '+IntToStr(Msg.Priority));
      IF Msg.FMSPriority<>'' THEN Socket.Writeln('X-MSMail-Priority: '+Msg.MSPriority);
      IF Msg.Mailer<>'' THEN Socket.Writeln('X-Mailer: '+Msg.Mailer);

      IF Assigned(Msg.FileAttaches) THEN
      BEGIN
        IF Msg.FileAttaches.NeedMime THEN Msg.MimeMessage:=True;
        Msg.MimeVersion:='1.0';
        Msg.ContentType:='multipart/mixed';
      END;
      Msg.Boundary:=aslEncode.MakeUniqueID;
      IF Msg.MimeVersion<>'' THEN Socket.Writeln('Mime-Version: '+Msg.MimeVersion);
      IF Msg.MimeMessage THEN
        IF Msg.ContentType<>'' THEN Socket.Writeln('Content-Type: '+Msg.ContentType+'; boundary="'+Msg.Boundary+'"');
      Socket.WriteLn('');

      IF Assigned(Msg.FileAttaches) AND Msg.MimeMessage THEN
      BEGIN
        Socket.WriteLn('  This message is in MIME format.  The first part should be readable text,');
        Socket.WriteLn('  while the remaining parts are likely unreadable without MIME-aware tools.');
        Socket.WriteLn('  Send mail to mime@docserver.cac.washington.edu for more info.');
        Socket.WriteLn('');
        Socket.WriteLn('--'+Msg.Boundary);
        Socket.WriteLn('Content-Type: text/plain; charset="us-ascii"');
        Socket.WriteLn('');
      END;
    END;
    SendMsgLines(Msg.MsgBody);

    IF Assigned(Msg.FileAttaches) THEN
    BEGIN
      Mime:=TMimeMgr.Create;
      FOR i:=1 TO Msg.FileAttaches.Count DO
      BEGIN
        IF Msg.MimeMessage THEN Socket.WriteLn('--'+Msg.Boundary);
        Msg.FileAttaches.Get(i, Filename, E);
        Content:=Mime.MimeType(ExtractFilename(Filename));
        IF Msg.MimeMessage THEN Socket.WriteLn('Content-Type: '+Content+'; name="'+ExtractFilename(Filename)+'"');
        Content:='Content-Transfer-Encoding: ';
        CASE E OF
          encNone :
            Content:='';
          enc64   :
            Content:=Content + 'base64';
          encUU   :
            Content:=Content + 'x-uuencode';
          encXX   :
            Content:=Content + 'x-xxencode';
        END;
        IF Msg.MimeMessage AND (Content<>'') THEN Socket.WriteLn(Content);
        IF Msg.MimeMessage THEN Socket.WriteLn('');
        CASE E OF
          enc64 :
            Encode:=T64Encode.Create(Filename);
          encUU :
            Encode:=TUUEncode.Create(Filename);
          encXX :
            Encode:=TXXEncode.Create(Filename);
        END;
        WHILE NOT Encode.EncodeEof DO
          Socket.WriteLn(Encode.Encode);
        Encode.Destroy;
        Encode:=Nil;
      END;
      IF Msg.MimeMessage THEN Socket.WriteLn('--'+Msg.Boundary+'--');
      Mime.Destroy;
    END;
    Socket.WriteLn('.');

    GetResponse(Nil);
  END;

  PROCEDURE TSMTPClient.SendHTMLMsg(Msg: TSMTPHTMLMessage);
  VAR
    i : Integer;
    S : STRING;
    E : TEncode;
    Filename : STRING;
    Content  : STRING;
    Encode : TAbsEncode;
    Mime : TMimeMgr;
  BEGIN
    Mail(Msg.From);
    FOR i:=0 TO Msg.FTo.Count-1 DO
      Rcpt(Msg.FTo[i]);
    Data;

    IF Msg.Header THEN
    BEGIN
      Socket.WriteLn('From: '+Msg.From);
      S:='To: ' + Msg.To_[0];
      FOR i:=1 TO Msg.To_.Count-1 DO
        S:=S+', '+Msg.To_[i];
      Socket.WriteLn(S);
      IF Msg.Subject<>'' THEN Socket.WriteLn('Subject: '+Msg.Subject);
      IF Msg.MessageID<>'' THEN Socket.WriteLn('Message-Id: '+Msg.MessageID);
      TimeSeparator:=':';
      IF Msg.Date<>'' THEN
        Socket.WriteLn('Date: '+Msg.Date)
      ELSE
        Socket.WriteLn('Date: '+FormatDateTime(SMTPDateTemplate, Now));
      IF Msg.ReplyTo<>'' THEN Socket.WriteLn('Reply-To: '+Msg.ReplyTo);
      IF Msg.Organisation<>'' THEN Socket.Writeln('Organisation: '+Msg.Organisation);
      IF Msg.Priority <> 0 THEN Socket.Writeln('X-Priority: '+IntToStr(Msg.Priority));
      IF Msg.FMSPriority<>'' THEN Socket.Writeln('X-MSMail-Priority: '+Msg.MSPriority);
      IF Msg.Mailer<>'' THEN Socket.Writeln('X-Mailer: '+Msg.Mailer);

      Msg.MimeVersion:='1.0';
      Msg.ContentType:='multipart/mixed';

      Msg.Boundary:=aslEncode.MakeUniqueID;
      Msg.HTMLBoundary:=aslEncode.MakeUniqueID;
      Socket.Writeln('Mime-Version: '+Msg.MimeVersion);
      Socket.Writeln('Content-Type: '+Msg.ContentType+'; boundary="'+Msg.Boundary+'"');
      Socket.WriteLn('');

      Socket.WriteLn('  This message is in MIME format.  The first part should be readable text,');
      Socket.WriteLn('  while the remaining parts are likely unreadable without MIME-aware tools.');
      Socket.WriteLn('  Send mail to mime@docserver.cac.washington.edu for more info.');
      Socket.WriteLn('');
      Socket.WriteLn('--'+Msg.Boundary);
      Socket.WriteLn('Content-Type: multipart/alternative;');
      Socket.WriteLn('    boundary="' + Msg.HTMLBoundary+'"');
      Socket.WriteLn('');
      Socket.WriteLn('');
      Socket.WriteLn('--' + Msg.HTMLBoundary);
      Socket.WriteLn('Content-Type: text/plain;');
      Socket.WriteLn('    charset="iso8859-1"');
      Socket.WriteLn('Content-Transfer-Encoding: quoted/printable');
      Socket.WriteLn('');
    END;
    SendMsgLines(Msg.MsgBody);
    Socket.WriteLn('--' + Msg.HTMLBoundary);
    Socket.WriteLn('Content-Type: text/html;');
    Socket.WriteLn('    charset="iso8859-1"');
    Socket.WriteLn('Content-Transfer-Encoding: quoted/printable');
    Socket.WriteLn('');
    SendMsgLines(Msg.HTMLMsgBody);
    Socket.WriteLn('');
    Socket.WriteLn('--' + Msg.HTMLBoundary + '--');
    Socket.WriteLn('');

    IF Assigned(Msg.FileAttaches) THEN
    BEGIN
      Mime:=TMimeMgr.Create;
      FOR i:=1 TO Msg.FileAttaches.Count DO
      BEGIN
        IF Msg.MimeMessage THEN Socket.WriteLn('--'+Msg.Boundary);
        Msg.FileAttaches.Get(i, Filename, E);
        Content:=Mime.MimeType(ExtractFilename(Filename));
        IF Msg.MimeMessage THEN Socket.WriteLn('Content-Type: '+Content+'; name="'+ExtractFilename(Filename)+'"');
        Content:='Content-Transfer-Encoding: ';
        CASE E OF
          encNone :
            Content:='';
          enc64   :
            Content:=Content + 'base64';
          encUU   :
            Content:=Content + 'x-uuencode';
          encXX   :
            Content:=Content + 'x-xxencode';
        END;
        IF Msg.MimeMessage AND (Content<>'') THEN Socket.WriteLn(Content);
        IF Msg.MimeMessage THEN Socket.WriteLn('');
        CASE E OF
          enc64 :
            Encode:=T64Encode.Create(Filename);
          encUU :
            Encode:=TUUEncode.Create(Filename);
          encXX :
            Encode:=TXXEncode.Create(Filename);
        END;
        WHILE NOT Encode.EncodeEof DO
          Socket.WriteLn(Encode.Encode);
        Encode.Destroy;
        Encode:=Nil;
      END;
      IF Msg.MimeMessage THEN Socket.WriteLn('--'+Msg.Boundary+'--');
      Mime.Destroy;
    END;
    Socket.WriteLn('.');

    GetResponse(Nil);
  END;


{ TAttachments }

  CONSTRUCTOR TAttachments.Create;
  BEGIN
    INHERITED Create;
    FFiles:=TStringList.Create;
    FNum:=0;
    FMime:=False;
  END;

  DESTRUCTOR TAttachments.Destroy;
  VAR
    i:Integer;
  BEGIN
    FOR i:=1 TO FNum DO
      FreeMem(FEncode[i], SizeOf(TEncode));
    FFiles.Destroy;
    FNum:=0;
    FMime:=False;
    INHERITED Destroy;
  END;

  PROCEDURE TAttachments.Add(Filename : String; Encode : TEncode);
  BEGIN
    FFiles.Add(Filename);
    Inc(FNum, 1);
    GetMem(FEncode[FNum], SizeOf(TEncode));
    FEncode[FNum]^:=Encode;
    IF Encode IN [enc64] THEN
      FMime:=True;
  END;

  FUNCTION TAttachments.Count : LongInt;
  BEGIN
    Result:=FNum;
  END;

  FUNCTION TAttachments.Get(Index : LongInt; Var Filename : String; Var Encode : TEncode) : Boolean;
  BEGIN
    Result:=False;
    Filename:='';
    Encode:=encNone;
    IF (Index<1) OR (Index>FNum) THEN
      Exit;
    Filename:=FFiles[Index-1];
    Encode:=FEncode[Index]^;
  END;


{ TSMTPMessage }

  CONSTRUCTOR TSMTPMessage.Create;
  BEGIN
    INHERITED Create;
    FHeader:=True;
    FFrom:='';
    FTo:=nil;
    FSubject:='';
    FMessageID:='';
    FDate:='';
    FReplyTo:='';
    FOrganisation:='';
    FMimeVersion:='';
    FContentType:='';
    FMimeMsg:=True;
    FMsgBody:=Nil;
    FileAttaches:=Nil;
    FPriority:=0;
    FMSPriority:='';
    FMailer:='';
  END;

  DESTRUCTOR TSMTPMessage.Destroy;
  BEGIN
    IF Assigned(FTo) THEN FTo.Destroy;
    FTo:=Nil;
    IF Assigned(FMsgBody) THEN FMsgBody.Destroy;
    FMsgBody:=Nil;
    IF Assigned(FileAttaches) THEN FileAttaches.Destroy;
    FileAttaches:=Nil;
    INHERITED Destroy;
  END;

  PROCEDURE TSMTPMessage.Clear;
  BEGIN
    IF Assigned(FTo) THEN FTo.Destroy;
    IF Assigned(FMsgBody) THEN FMsgBody.Destroy;
    IF Assigned(FileAttaches) THEN FileAttaches.Destroy;
    FHeader:=True;
    FFrom:='';
    FTo:=Nil;
    FSubject:='';
    FMessageID:='';
    FDate:='';
    FReplyTo:='';
    FOrganisation:='';
    FMimeVersion:='';
    FContentType:='';
    FMimeMsg:=True;
    FMsgBody:=Nil;
    FileAttaches:=Nil;
    FPriority:= 0;
    FMSPriority:='';
    FMailer:='';
  END;

  PROCEDURE TSMTPMessage.AddTo(To_: STRING);
  BEGIN
    IF NOT Assigned(FTo) THEN FTo:=TStringList.Create;
    FTo.Add(To_);
  END;

  PROCEDURE TSMTPMessage.AddMsgBody(Line: STRING);
  BEGIN
    IF NOT Assigned(FMsgBody) THEN FMsgBody:=TStringList.Create;
    FMsgBody.Add(Line);
  END;

  PROCEDURE TSMTPMessage.AddAttach(Filename: STRING; Encode: TEncode);
  BEGIN
    IF NOT Assigned(FileAttaches) THEN FileAttaches:=TAttachments.Create;
    FileAttaches.Add(Filename, Encode);
  END;


{ TSMTPHTMLMessage }

  CONSTRUCTOR TSMTPHTMLMessage.Create;
  BEGIN
    INHERITED Create;
    FHTMLMsgBody:=Nil;
  END;

  DESTRUCTOR TSMTPHTMLMessage.Destroy;
  BEGIN
    IF Assigned(FHTMLMsgBody) THEN FHTMLMsgBody.Destroy;
    FHTMLMsgBody:=Nil;
    INHERITED Destroy;
  END;

  PROCEDURE TSMTPHTMLMessage.Clear;
  BEGIN
    IF Assigned(FHTMLMsgBody) THEN FHTMLMsgBody.Destroy;
    FHTMLMsgBody:=Nil;
    INHERITED Clear;
  END;

  PROCEDURE TSMTPHTMLMessage.AddHTMLBody(Line: STRING);
  BEGIN
    IF NOT Assigned(FHTMLMsgBody) THEN FHTMLMsgBody:=TStringList.Create;
    FHTMLMsgBody.Add(Line);
  END;

END.
