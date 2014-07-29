{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.13 $    $Date: 2002/10/01 01:07:33 $    $Author: sag $ }
{                                                                             }
{ Simple Mail Transfer Protocol (SMTP) client example                         }
PROGRAM SMTPSend;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  SysUtils, Application, aslGlobals, aslSMTPClient;

TYPE
  TSMTPClientApp = CLASS(TaslApplication)
  PRIVATE
    Server : STRING;
    From   : STRING;
    To_    : STRING;
    Domain : STRING;
    Subject: STRING;
    FName  : STRING;
    Encode : TEncode;

  PUBLIC
    PROCEDURE ShowCopyright; OVERRIDE;
    PROCEDURE ShowHelp; OVERRIDE;
    FUNCTION  ParseCmdLine: Boolean; OVERRIDE;
    PROCEDURE Run; OVERRIDE;
    PROCEDURE InitConfig; OVERRIDE;

    PROCEDURE SendTheMessage;
  END;

  PROCEDURE TSMTPClientApp.ShowCopyright;
  BEGIN
    INHERITED ShowCopyright;
    WriteLn('Send a message from the commandline.');
    WriteLn;
  END;

  PROCEDURE TSMTPClientApp.ShowHelp;
  BEGIN
    WriteLn('Usage:');
    WriteLn;
    WriteLn('  SMTPSend /S<Server> /F<From> /T<To> [/D<Domain>] [/B<Subject>] [/L[<LogFile>]');
    WriteLn('           [/A<Filename>] [/E<Encoding>]');
    WriteLn;
    WriteLn('where:');
    WriteLn('  <Server>   is the name og the SMTP server.');
    WriteLn('  <From>     e-mail address of sender.');
    WriteLn('  <To>       e-mail address of receiver.');
    WriteLn('  <Domain>   your domain, if different from the domain in <From>.');
    WriteLn('  <Subject>  subject of the message.');
    WriteLn('  <LogFile>  is the name of the file to log to, if /L is specified without a');
    WriteLn('             filename, loginfo will be written to the screen.');
    WriteLn('  <Filename> is the file to attach to the message. to specify a list of files');
    WriteLn('             use @<filename>. one filename per line');
    WriteLn('  <Encoding> file attachment encoding to use. one of 64, UU or XX');
    WriteLn;
    WriteLn('  The message body is read from stdin.');
    ErrorExit('',1);
  END;

  FUNCTION TSMTPClientApp.ParseCmdLine: Boolean;
  VAR
    i : Integer;
    s : STRING;
  BEGIN
    FOR i:=1 TO ParamCount DO
      IF Length(ParamStr(i))>1 THEN
      BEGIN
        CASE UpCase(ParamStr(i)[2]) OF
          'S' : Server:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
          'F' : From:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
          'T' : To_:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
          'D' : Domain:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
          'B' : Subject:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
          'L' : BEGIN
            Log:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
            UseLog:=True;
          END;
          'A' : FName:=Copy(ParamStr(i),3,Length(ParamStr(i))-2);
          'E' : BEGIN
            s:=Copy(ParamStr(i),3,Length(ParamStr(i))-2);
            IF CompareText(s, 'UU') = 0 THEN Encode:=encUU ELSE
            IF CompareText(s, 'XX') = 0 THEN Encode:=encXX ELSE
              Encode:=enc64;
          END;
          ELSE
            ShowHelp;
        END;
      END ELSE
        ShowHelp;
    IF (Domain='') AND (Pos('@',From)>0) THEN
       Domain:=Copy(From, Pos('@', From)+1, Length(From)-Pos('@', From));
    Result:=True;
  END;

  PROCEDURE TSMTPClientApp.Run;
  BEGIN
    INHERITED Run;
    IF (Server<>'') AND (From<>'') AND (To_<>'') AND (Domain<>'') THEN SendTheMessage ELSE ShowHelp;
  END;

  PROCEDURE TSMTPClientApp.InitConfig;
  BEGIN
    INHERITED InitConfig;
    Encode:=enc64;
  END;

  PROCEDURE TSMTPClientApp.SendTheMessage;
  VAR
    Cli : TSMTPCLient;
    Msg : TSMTPMessage;
    MsgF: Text;
    S   : String;
  BEGIN
    Msg:=TSMTPMessage.Create;
    Msg.AddTo(To_);
    Msg.From:=From;
    Msg.Subject:=Subject;

    Assign(MsgF, '');
    Reset(MsgF);
    WHILE Not Eof(MsgF) DO
    BEGIN
      ReadLn(MsgF,S);
      Msg.AddMsgBody(S);
    END;
    Close(MsgF);

    IF FName[1]='@' THEN
    BEGIN
      Assign(MsgF, Copy(FName, 2, Length(FName)-1));
{$I-}
      Reset(MsgF);
{$I+}
      IF IOResult = 0 THEN
        WHILE Not Eof(MsgF) DO
        BEGIN
          ReadLn(MsgF,S);
          Msg.AddAttach(S,Encode);
        END;
{$I-}
      Close(MsgF);
{$I+}
    END ELSE
       IF FName<>'' THEN Msg.AddAttach(FName,Encode);

    Cli:=TSMTPClient.Create;
    IF UseLog THEN Cli.OnLogLine:=SocketLogLine;
    Cli.Connect(Server, Domain);
    Cli.SendMsg(Msg);

    Cli.Destroy;
    Msg.Destroy;
  END;

VAR
  App: TSMTPClientApp;

BEGIN
  App:=TSMTPClientApp.Create('SMTPSend');
  App.Run;
  App.Destroy;
END.

