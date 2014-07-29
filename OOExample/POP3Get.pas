{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.8 $    $Date: 2002/10/01 01:07:32 $    $Author: sag $ }
{                                                                             }
{ Post Office Protocol - Version 3 (POP3) client example                      }
PROGRAM POP3Get;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  Classes, SysUtils, Application, aslGlobals, aslAbsSocket, aslPOP3Client;

TYPE
  TPOP3ClientApp = CLASS(TaslApplication)
  PRIVATE
    Server : STRING;
    User   : STRING;
    Pass   : STRING;
    Path   : STRING;
    Delete : Boolean;

  PUBLIC
    PROCEDURE ShowCopyright; OVERRIDE;
    PROCEDURE ShowHelp; OVERRIDE;
    FUNCTION  ParseCmdLine: Boolean; OVERRIDE;
    PROCEDURE Run; OVERRIDE;

    PROCEDURE GetTheMail;
  END;


  PROCEDURE TPOP3ClientApp.ShowCopyright;
  BEGIN
    INHERITED ShowCopyright;
    WriteLn('Downloads e-mail from a POP3 server to text files.');
    WriteLn;
  END;

  PROCEDURE TPOP3ClientApp.ShowHelp;
  BEGIN
    WriteLn('Usage:');
    WriteLn;
    WriteLn('  POP3Get /S<Server> /U<User> [/P<Password>] [/D<Dir>] [/L[<LogFile>]] [/R]');
    WriteLn;
    WriteLn('where:');
    WriteLn('  <Server>   is the name of the POP3 server.');
    WriteLn('  <User>     is the user name.');
    WriteLn('  <Password> is the password for <user> - if required.');
    WriteLn('  <Dir>      is the directory to place the messages in - current is default.');
    WriteLn('  <LogFile>  is the name of the file to log to, if /L is specified without a');
    WriteLn('             filename, loginfo will be written to the screen.');
    WriteLn('  /R         removes the message from the server after download.');
    WriteLn;
    WriteLn('examples:');
    WriteLn('  POP3Get /Smail /Uuser /Psecret /Dc:\mail /Lc:\mail\mail.log /R');
    WriteLn('  POP3Get /Smail /Uuser /Psecret /R');
    ErrorExit('',1);
  END;

  FUNCTION TPOP3ClientApp.ParseCmdLine: Boolean;
  VAR
    i : Integer;
  BEGIN
    FOR i:=1 TO ParamCount DO
      IF Length(ParamStr(i))>1 THEN
      BEGIN
        CASE UpCase(ParamStr(i)[2]) OF
          'S' : Server:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
          'U' : User:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
          'P' : Pass:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
          'D' : BEGIN
            Path:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
            IF (Path<>'') AND (Copy(Path, Length(Path), 1)<>'\') THEN Path:=Path+'\';
          END;
          'L' : BEGIN
            Log:=Copy(ParamStr(i), 3, Length(ParamStr(i))-2);
            UseLog:=True;
          END;
          'R' : Delete:=True;
          ELSE
            ShowHelp;
        END;
      END ELSE
        ShowHelp;
    Result:=True;
  END;


  PROCEDURE TPOP3ClientApp.Run;
  BEGIN
    INHERITED Run;
    IF (Server<>'') AND (User<>'') THEN GetTheMail ELSE ShowHelp;
  END;

  PROCEDURE TPOP3ClientApp.GetTheMail;
  VAR
    i   : Integer;
    Cli : TPOP3CLient;
    Msg : TStringList;
  BEGIN
    TRY
      Cli:=TPOP3Client.Create;
      TRY
        IF UseLog THEN Cli.OnLogLine:=SocketLogLine;
        SocketLogLine(Self, '');
        Cli.DelAfter:=Delete;
        Cli.Connect(Server, User, Pass);
        WriteLn('You have ',Cli.Msgs,' messages, with a total size of ',Cli.Size,' bytes waiting');
        Msg:=TStringList.Create;
        FOR i:=1 TO Cli.Msgs DO
        BEGIN
          Write('Fetching: ',i,#13);
          Cli.GetMsg(i, Msg);
          Msg.SaveToFile(Path+'DL-'+IntToHex(i,5)+'.POP');
          Msg.Clear;
        END;
        Msg.Destroy;
        WriteLn;
      FINALLY
        Cli.Destroy;
      END;
    EXCEPT
      ON e: EaslException DO WriteLn('Error: ',e.Message);
      ELSE RAISE;
    END;
  END;

VAR
  App: TPop3ClientApp;

BEGIN
  App:=TPop3ClientApp.Create('POP3Get');
  App.Run;
  App.Destroy;
END.

