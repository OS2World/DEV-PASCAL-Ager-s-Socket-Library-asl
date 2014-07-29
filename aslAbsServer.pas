{ Ager's Socket Library (c) Copyright 1998-01 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.4 $    $Date: 2001/02/03 19:36:06 $    $Author: sag $ }
{                                                                             }
{ Abstract Server and ServerManager classes                                   }
UNIT aslAbsServer;

{$I aslDefine.Inc}

INTERFACE

USES vpSysLow, SysUtils, Classes, aslUtils, aslClasses, aslTCPSocket;

TYPE
  TaslMessage = RECORD
    Msg    : Cardinal;
    Parm   : String;
    Result : Integer;
  END;

  PaslCmds = ^TaslCmds;
  TaslCmds = RECORD
    Cmd   : String;
    CmdId : Integer;
  END;

  TCmdList = CLASS
  PRIVATE
    FCommands : TList;
  PUBLIC
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;

    FUNCTION Add(ACmd: String; ACmdId: Integer): Boolean;
    FUNCTION DelByName(ACmd: String): Boolean;
    FUNCTION DelById(ACmdId: Integer): Boolean;
    FUNCTION Find(Str: String; VAR ACmd: String): LongInt;
  END;


  TServerManager = CLASS;
  TAbsServer     = CLASS;

  TCreateServerProc = FUNCTION(ASvrMgr: TServerManager; ACliSocket: TTCPClientSocket): TAbsServer;

  TServerManager = CLASS
  PRIVATE
    FTerminated     : Boolean;
    FMaxConnections : Integer;
    FProto      : String;
    FSvrSocket  : TTCPServerSocket;
    FCreateProc : TCreateServerProc;
    FServerList : TList;
  PUBLIC
    CONSTRUCTOR Create(AProto: STRING; ACreateProc: TCreateServerProc);
    DESTRUCTOR Destroy; OVERRIDE;
    FUNCTION Execute: LongInt;

    PROCEDURE AddServerThread(ASvr: TAbsServer);
    PROCEDURE DelServerThread(ASvr: TAbsServer);

    PROCEDURE CreateServer(ACliSocket: TTCPClientSocket); VIRTUAL;
    PROCEDURE EvtLogLine(Sender: TObject; Msg: String);

    PROPERTY MaxConnections: Integer READ FMaxConnections WRITE FMaxConnections;
    PROPERTY Proto: STRING           READ FProto;
  END;


  TAbsServer = CLASS(TThread)
  PRIVATE
    FCliSocket : TTCPClientSocket;
    FSvrMgr    : TServerManager;
  PUBLIC
    CONSTRUCTOR Create(ASvrMgr: TServerManager; AClientSocket: TTCPClientSocket);
    DESTRUCTOR Destroy; OVERRIDE;

    PROPERTY CliSocket: TTCPClientSocket READ FCliSocket;
  END;

  TAbsTextServer = CLASS(TAbsServer)
  PRIVATE
    FClientQuit   : Boolean;
    FClientHangup : Boolean;
    FCmdColl      : TCmdList;
  PROTECTED
    CONSTRUCTOR Create(ASvrMgr: TServerManager; AClientSocket: TTCPClientSocket);
    DESTRUCTOR Destroy; OVERRIDE;

    PROCEDURE Execute; OVERRIDE;
    PROCEDURE SendGreeting; VIRTUAL;
    PROCEDURE SendSignOff; VIRTUAL;
    PROCEDURE SendUnknownCmd(ACmd: String); VIRTUAL; ABSTRACT;

    PROCEDURE GetMsgLines(StringList: TStringList);

    PROPERTY ClientQuit: Boolean     READ FClientQuit   WRITE FClientQuit;
    PROPERTY ClientHangup: Boolean   READ FClientHangup WRITE FClientHangup;
    PROPERTY CmdColl: TCmdList READ FCmdColl;
  END;

IMPLEMENTATION

{ TServerManager }

  CONSTRUCTOR TServerManager.Create(AProto: STRING; ACreateProc: TCreateServerProc);
  BEGIN
    INHERITED Create;
    FTerminated:=False;
    FMaxConnections:=0;
    FProto:=AProto;
    FSvrSocket:=Nil;
    FCreateProc:=ACreateProc;
    FServerList:=TList.Create;
  END;

  DESTRUCTOR TServerManager.Destroy;
  VAR
    i : Integer;
  BEGIN
    FOR i:=0 TO FServerList.Count-1 DO
      TAbsServer(FServerList[i]).Terminate;
    WHILE FServerList.Count>0 DO
      SysCtrlSleep(1000);
    FServerList.Destroy;
    INHERITED Destroy;
  END;

  FUNCTION TServerManager.Execute: LongInt;
  VAR
    FCliSocket : TTCPClientSocket;
  BEGIN
    FSvrSocket:=TTCPServerSocket.Create(FProto);
    FSvrSocket.OnLogLine:=EvtLogLine;
    TRY
      REPEAT
        FSvrSocket.Listen;
        FCliSocket:=FSvrSocket.AcceptConnection;
//        IF NOT AcceptAddress THEN HangUp;
        IF (FMaxConnections>0) AND (FServerList.Count>=FMaxConnections) THEN
          FCliSocket.Destroy     // Send "server busy" to client
        ELSE
          CreateServer(FCliSocket);
      UNTIL FTerminated;
    FINALLY
      FSvrSocket.Destroy;
    END;
  END;

  PROCEDURE TServerManager.AddServerThread(ASvr: TAbsServer);
  BEGIN
    FServerList.Add(ASvr);
  END;

  PROCEDURE TServerManager.DelServerThread(ASvr: TAbsServer);
  BEGIN
    FServerList.Remove(ASvr);
  END;

  PROCEDURE TServerManager.CreateServer(ACliSocket: TTCPClientSocket);
  BEGIN
    FCreateProc(Self, ACliSocket);
  END;

  PROCEDURE TServerManager.EvtLogLine(Sender: TObject; Msg: String);
  BEGIN
    WriteLn(msg);
  END;


{ TAbsServer }

  CONSTRUCTOR TAbsServer.Create(ASvrMgr: TServerManager; AClientSocket: TTCPClientSocket);
  BEGIN
    FCliSocket:=AClientSocket;
    FSvrMgr:=ASvrMgr;
    FSvrMgr.AddServerThread(Self);
    INHERITED Create;
  END;

  DESTRUCTOR TAbsServer.Destroy;
  BEGIN
    FSvrMgr.DelServerThread(Self);
    FCliSocket.Destroy;
    INHERITED Destroy;
  END;


{ TAbsTextServer }

  CONSTRUCTOR TAbsTextServer.Create(ASvrMgr: TServerManager; AClientSocket: TTCPClientSocket);
  BEGIN
    ClientQuit:=False;
    ClientHangup:=False;
    FCmdColl:=TCmdList.Create;
    INHERITED Create(ASvrMgr, AClientSocket);
  END;

  DESTRUCTOR TAbsTextServer.Destroy;
  BEGIN
    FCmdColl.Destroy;
    INHERITED Destroy;
  END;

  PROCEDURE TAbsTextServer.Execute;
  VAR
    InStr : String;
    Cmd   : String;
    Msg   : TaslMessage;
  BEGIN
    SendGreeting;
    REPEAT
      TRY
        IF FCliSocket.WaitForDataTime(10000)>0 THEN
        BEGIN
          FCliSocket.ReadLn(InStr);
          InStr:=Trim(InStr);

          FillChar(Msg, SizeOf(Msg), 0);
          Msg.Msg:=FCmdColl.Find(InStr, Cmd);
          Msg.Parm:=Trim(Copy(InStr,Length(Cmd)+1,Length(InStr)-Length(Cmd)));
          IF Msg.Msg>0 THEN Dispatch(Msg) ELSE SendUnknownCmd(InStr);
        END;
      EXCEPT
        ON e: ETCPClientSocket DO ;
        ELSE RAISE;
      END;
    UNTIL (FCliSocket.SockErrNo=10054) OR FClientQuit OR FClientHangup OR Terminated;
    IF FClientQuit THEN SendSignOff;
  END;

  PROCEDURE TAbsTextServer.SendGreeting;
  BEGIN
  END;

  PROCEDURE TAbsTextServer.SendSignOff;
  BEGIN
  END;

  PROCEDURE TAbsTextServer.GetMsgLines(StringList: TStringList);
  VAR
    S, S1 : STRING;
  BEGIN
    REPEAT
      FCliSocket.ReadLn(S);
      S1:=S;
      IF (Length(S)>1) AND (Copy(S,1,1)='.') THEN S:=Copy(S, 2, Length(S)-1);
      IF (S1<>'.') AND Assigned(StringList) THEN StringList.Add(S);
    UNTIL S1='.';
  END;


{ TCmdList }

  CONSTRUCTOR TCmdList.Create;
  BEGIN
    INHERITED Create;
    FCommands:=TList.Create;
  END;

  DESTRUCTOR TCmdList.Destroy;
  VAR
    i : Integer;
  BEGIN
    FOR i:=0 TO FCommands.Count-1 DO
      Dispose(PaslCmds(FCommands[i]));
    FCommands.Destroy;
    INHERITED Destroy;
  END;

  FUNCTION TCmdList.Add(ACmd: String; ACmdId: Integer): Boolean;
  VAR
    Cmds : PaslCmds;
    S    : String;
  BEGIN
    IF Find(ACmd,S)=0 THEN
    BEGIN
      New(Cmds);
      Cmds^.Cmd:=UpperCase(ACmd);
      Cmds^.CmdId:=ACmdId;
      FCommands.Add(Cmds);
      Add:=True;
    END ELSE
      Add:=False;
  END;

  FUNCTION TCmdList.DelByName(ACmd: String): Boolean;
  VAR
    i : Integer;
  BEGIN
    i:=0;
    WHILE (FCommands.Count>i) AND (PaslCmds(FCommands[i])^.Cmd<>UpperCase(Copy(ACmd,1,Length(PaslCmds(FCommands[i])^.Cmd)))) DO
      Inc(i);

    IF (FCommands.Count>0) AND (FCommands.Count>i) THEN
    BEGIN
      Dispose(PaslCmds(FCommands[i]));
      DelByName:=True;
    END ELSE
      DelByName:=False;
  END;

  FUNCTION TCmdList.DelById(ACmdId: Integer): Boolean;
  VAR
    i : Integer;
  BEGIN
    i:=0;
    WHILE (FCommands.Count>i) AND (PaslCmds(FCommands[i])^.CmdId<>ACmdId) DO
      Inc(i);

    IF (FCommands.Count>0) AND (FCommands.Count>i) THEN
    BEGIN
      Dispose(PaslCmds(FCommands[i]));
      DelById:=True;
    END ELSE
      DelById:=False;
  END;

  FUNCTION TCmdList.Find(Str: String; VAR ACmd: String): LongInt;
  VAR
    i : Integer;
  BEGIN
    i:=0;
    WHILE (FCommands.Count>i) AND (PaslCmds(FCommands[i])^.Cmd<>UpperCase(Copy(Str,1,Length(PaslCmds(FCommands[i])^.Cmd)))) DO
      Inc(i);

    IF (FCommands.Count>0) AND (FCommands.Count>i) THEN
    BEGIN
      ACmd:=PaslCmds(FCommands[i])^.Cmd;
      Find:=PaslCmds(FCommands[i])^.CmdId
    END ELSE
    BEGIN
      ACmd:='';
      Find:=0;
    END;
  END;


END.

