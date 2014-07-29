{ Ager's Socket Library (c) Copyright 1998-01 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.2 $    $Date: 2001/03/02 02:08:42 $    $Author: sag $ }
{                                                                             }
{ Application class                                                           }
UNIT Application;

{$I aslDefine.Inc}

INTERFACE

TYPE
  TAbsApplication = CLASS
    CONSTRUCTOR Create(ApplicationName: STRING);
    PROCEDURE ShowCopyright; VIRTUAL;
    FUNCTION  ParseCmdLine: Boolean; VIRTUAL;
    PROCEDURE ShowHelp; VIRTUAL;
    PROCEDURE Run; VIRTUAL;
    FUNCTION  ReadConfig: Boolean; VIRTUAL;
    PROCEDURE InitConfig; VIRTUAL;

    PROCEDURE ErrorExit(ErrMsg: STRING; ErrCode: Integer);
  PRIVATE
    FApplicationName: STRING;
  END;

  TaslApplication = CLASS(TAbsApplication)
    FLog    : STRING;
    FUseLog : Boolean;

    PROCEDURE ShowCopyright; OVERRIDE;
    PROCEDURE SocketLogLine(TSender: TObject; Msg: String);

    PROPERTY Log: STRING      READ FLog    WRITE FLog;
    PROPERTY UseLog: Boolean  READ FUseLog WRITE FUseLog;
  END;

IMPLEMENTATION

USES aslGlobals;

{ TAbsApplication }

  CONSTRUCTOR TAbsApplication.Create(ApplicationName: STRING);
  BEGIN
    INHERITED Create;
    FApplicationName:=ApplicationName;
    ShowCopyright;
    IF NOT ParseCmdLine THEN ShowHelp;
  END;

  PROCEDURE TAbsApplication.ShowCopyright;
  BEGIN
  END;

  FUNCTION TAbsApplication.ParseCmdLine: Boolean;
  BEGIN
    Result:=True;
  END;

  PROCEDURE TAbsApplication.ShowHelp;
  BEGIN
  END;

  PROCEDURE TAbsApplication.Run;
  BEGIN
    IF NOT ReadConfig THEN InitConfig;
  END;

  FUNCTION TAbsApplication.ReadConfig: Boolean;
  BEGIN
    Result:=False;
  END;

  PROCEDURE TAbsApplication.InitConfig;
  BEGIN
  END;

  PROCEDURE TAbsApplication.ErrorExit(ErrMsg: STRING; ErrCode: Integer);
  BEGIN
    WriteLn(ErrMsg);
    Halt(ErrCode);
  END;


{ TaslApplication }

  PROCEDURE TaslApplication.ShowCopyright;
  BEGIN
    INHERITED ShowCopyright;
    WriteLn(FApplicationName+'  Release '+aslRelease+'   '+aslCopyright);
    WriteLn(aslPartOf);
  END;

  PROCEDURE TaslApplication.SocketLogLine(TSender: TObject; Msg: String);
  VAR
    Out : Text;
  BEGIN
    IF Log<>'' THEN
    BEGIN
      Assign(Out, Log);
      {$I-} Append(Out); {$I+}
      IF IOResult<>0 THEN Rewrite(Out);
      WriteLn(Out, Msg);
      {$I-} Close(Out); {$I+}
      IF IOResult<>0 THEN ;
    END ELSE  
      WriteLn(Msg);
  END;

END.

