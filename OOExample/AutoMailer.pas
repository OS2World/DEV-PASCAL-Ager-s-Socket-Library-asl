{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.3 $    $Date: 2002/10/01 01:07:31 $    $Author: sag $ }
{                                                                             }
{ Send short message(s) to a list of addresses                                }
PROGRAM AutoMailer;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  SysUtils, Application, aslSMTPClient;

TYPE
  TAutoMailerApp = CLASS(TaslApplication)
  PRIVATE
    FServer    : STRING;
    FConfigFile: STRING;
  PUBLIC
    CONSTRUCTOR Create(ApplicationName: STRING);
    PROCEDURE ShowCopyright; OVERRIDE;
//    PROCEDURE ShowHelp; OVERRIDE;
    FUNCTION  ParseCmdLine: Boolean; OVERRIDE;
    PROCEDURE Run; OVERRIDE;

    PROCEDURE SendMessages;
  END;

  FUNCTION NextWord(VAR s: STRING): STRING;
  VAR
    p  : Byte;
  BEGIN
    p:=Pos(',',s);
    IF p>0 THEN
    BEGIN
      NextWord:=Copy(s, 1, p-1);
      Delete(s, 1, p);
    END ELSE
    BEGIN
      NextWord:=s;
      s:='';
    END;
    s:=TrimLeft(s);
  END;


  CONSTRUCTOR TAutoMailerApp.Create(ApplicationName: STRING);
  BEGIN
    INHERITED Create(ApplicationName);
    FConfigFile:='AutoMailer.cfg';
  END;


  PROCEDURE TAutoMailerApp.ShowCopyright;
  BEGIN
    INHERITED ShowCopyright;
    WriteLn('Send short message(s) to a list of addresses.');
    WriteLn;
  END;

  FUNCTION TAutoMailerApp.ParseCmdLine: Boolean;
  BEGIN
    IF ParamCount>0 THEN
      FConfigFile:=ParamStr(1);
    ParseCmdLine:=True;
  END;

  PROCEDURE TAutoMailerApp.Run;
  BEGIN
    INHERITED Run;
    IF (FConfigFile<>'') THEN SendMessages ELSE ShowHelp;
  END;

  PROCEDURE TAutoMailerApp.SendMessages;
  VAR
    CfgFile   : Text;
    Line      : STRING;
    From      : STRING;
    To_       : STRING;
    Subject   : STRING;
    Body      : STRING;
    Cli       : TSMTPCLient;
    Msg       : TSMTPMessage;
  BEGIN
    Assign(CfgFile, FConfigFile);
    {$I-} Reset(CfgFile); {$I+}
    IF IOResult=0 THEN
    BEGIN
      WHILE NOT EoF(CfgFile) DO
      BEGIN
        ReadLn(CfgFile, Line);
        IF (Line<>'') AND (Copy(Line,1,1)<>';') THEN
        BEGIN
          IF FServer='' THEN
            FServer:=Line
          ELSE
          BEGIN
            From:=NextWord(Line);
            To_:=NextWord(Line);
            Subject:=NextWord(Line);
            Body:=NextWord(Line);

            Msg:=TSMTPMessage.Create;
            Msg.AddTo(To_);
            Msg.From:=From;
            Msg.Subject:=Subject;
            IF Body<>'' THEN
              Msg.AddMsgBody(Body);

            Cli:=TSMTPClient.Create;
        {    IF UseLog THEN Cli.OnLogLine:=SocketLogLine;}
            Cli.Connect(FServer, Copy(From, Pos('@', From)+1, Length(From)-Pos('@', From)));
            Cli.SendMsg(Msg);

            Cli.Destroy;
            Msg.Destroy;

          END;
        END;
      END;
    END ELSE
      ErrorExit('Can''t open "'+FConfigFile+'"',1);
  END;

VAR
  App: TAutoMailerApp;

BEGIN
  App:=TAutoMailerApp.Create('AutoMailer');
  App.Run;
  App.Destroy;
END.
