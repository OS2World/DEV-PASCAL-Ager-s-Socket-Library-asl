{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.2 $    $Date: 1999/04/14 18:24:21 $    $Author: sag $ }
{                                                                             }
{ Post Office Protocol - Version 3 (POP3) server class (STD 53)               }
UNIT aslPOP3Server;

{$I aslDefine.Inc}

INTERFACE

USES SysUtils, Classes, aslUtils, aslTCPSocket, aslAbsServer;

CONST
{ Mandatory commands }
  cmd_pop3User =  1;
  cmd_pop3Pass =  2;
  cmd_pop3List =  3;
  cmd_pop3Stat =  4;
  cmd_pop3Retr =  5;
  cmd_pop3Dele =  6;
  cmd_pop3Noop =  7;
{ Optional commands }
  cmd_pop3RSet =  8;
  cmd_pop3Top  =  9;
  cmd_pop3Uidl = 10;
{ Please add your own commands after this one (100 and up) }
  cmd_pop3Quit = 99;


{ POP3 server states }
  st_pop3Auth     =  0;
  st_pop3Trans    =  1;
  st_pop3Update   =  2;

  st_pop3Reserved = 99;

{ Messages }
  msg_pop3OK          = '+OK';
  msg_pop3Err         = '-ERR';
  msg_pop3UnknownCmd  = msg_pop3Err+' Unknown command:';
  msg_pop3NotLoggedIn = msg_pop3Err+' You need to login first';
  msg_pop3UnknownMsg  = msg_pop3Err+' no such message';

TYPE
  TMsgNum = LongInt;

  TPOP3Server = CLASS(TAbsTextServer)
  PRIVATE
    FGreeting    : String;
    FUser, FPass : String;
    FState       : Integer;
    FDelMsgList  : TList;
  PROTECTED
    PROCEDURE cmdUser(VAR Message: TaslMessage); MESSAGE cmd_pop3User;
    PROCEDURE cmdPass(VAR Message: TaslMessage); MESSAGE cmd_pop3Pass;
    PROCEDURE cmdList(VAR Message: TaslMessage); MESSAGE cmd_pop3List;
    PROCEDURE cmdStat(VAR Message: TaslMessage); MESSAGE cmd_pop3Stat;
    PROCEDURE cmdRetr(VAR Message: TaslMessage); MESSAGE cmd_pop3Retr;
    PROCEDURE cmdDele(VAR Message: TaslMessage); MESSAGE cmd_pop3Dele;
    PROCEDURE cmdNoop(VAR Message: TaslMessage); MESSAGE cmd_pop3Noop;
    PROCEDURE cmdRSet(VAR Message: TaslMessage); MESSAGE cmd_pop3RSet;
    PROCEDURE cmdTop(VAR Message: TaslMessage);  MESSAGE cmd_pop3Top;
    PROCEDURE cmdUidl(VAR Message: TaslMessage); MESSAGE cmd_pop3Uidl;

    PROCEDURE cmdQuit(VAR Message: TaslMessage); MESSAGE cmd_pop3Quit;

  PUBLIC
    CONSTRUCTOR Create(ASvrMgr: TServerManager; AClientSocket: TTCPClientSocket; AGreeting: String);
    DESTRUCTOR Destroy; OVERRIDE;

    PROCEDURE SendGreeting; OVERRIDE;
    PROCEDURE SendSignOff; OVERRIDE;
    PROCEDURE SendUnknownCmd(ACmd: String); OVERRIDE;

    FUNCTION  ValidateUser(AUser, APass: String): Boolean; VIRTUAL; ABSTRACT;

    FUNCTION  GetMaxMsgs: TMsgNum; VIRTUAL; ABSTRACT;
    FUNCTION  GetMsgSize(AMsgNum: TMsgNum): LongInt; VIRTUAL; ABSTRACT;
    FUNCTION  GetMessage(AMsgNum: TMsgNum; AMsg: TStringList): Boolean; VIRTUAL; ABSTRACT;
    FUNCTION  GetMessageTop(AMsgNum: TMsgNum; AMsgLines: Integer; AMsg: TStringList): Boolean; VIRTUAL; ABSTRACT;
    FUNCTION  GetMsgUidl(AMsgNum: TMsgNum): STRING; VIRTUAL; ABSTRACT;

    FUNCTION  MarkMsgDeleted(AMsgNum: TMsgNum): Boolean; VIRTUAL;
    FUNCTION  unDeleteMsgs: Boolean; VIRTUAL;
    FUNCTION  isDeleted(AMsgNum: TMsgNum): Boolean; VIRTUAL;
    FUNCTION  PhysDeleteMsg(AMsgNum: TMsgNum): Boolean; VIRTUAL; ABSTRACT;

    PROPERTY Greeting: String READ FGreeting WRITE FGreeting;
    PROPERTY User: String     READ FUser;
    PROPERTY Pass: String     READ FPass;
    PROPERTY State: Integer   READ FState;
  END;

IMPLEMENTATION

{ TPOP3Server }

  CONSTRUCTOR TPOP3Server.Create(ASvrMgr: TServerManager; AClientSocket: TTCPClientSocket; AGreeting: String);
  BEGIN
    FGreeting:=AGreeting;
    FDelMsgList:=TList.Create;
    INHERITED Create(ASvrMgr, AClientSocket);
    CmdColl.Add('USER', cmd_pop3User);
    CmdColl.Add('PASS', cmd_pop3Pass);
    CmdColl.Add('LIST', cmd_pop3List);
    CmdColl.Add('STAT', cmd_pop3Stat);
    CmdColl.Add('RETR', cmd_pop3Retr);
    CmdColl.Add('DELE', cmd_pop3Dele);
    CmdColl.Add('NOOP', cmd_pop3Noop);
    CmdColl.Add('RSET', cmd_pop3RSet);

    CmdColl.Add('QUIT', cmd_pop3Quit);
  END;

  DESTRUCTOR TPOP3Server.Destroy;
  BEGIN
    FDelMsgList.Destroy;
    INHERITED Destroy;
  END;

  PROCEDURE TPOP3Server.SendGreeting;
  BEGIN
    CliSocket.WriteLn(msg_pop3OK+' '+FGreeting+' ready');
  END;

  PROCEDURE TPOP3Server.SendSignOff;
  BEGIN
    CliSocket.WriteLn(msg_pop3OK+' '+FGreeting+' signing off');
  END;

  PROCEDURE TPOP3Server.SendUnknownCmd(ACmd: String);
  BEGIN
    CliSocket.WriteLn(msg_pop3UnknownCmd+' '+ACmd);
  END;



  FUNCTION TPOP3Server.unDeleteMsgs: Boolean;
  BEGIN
    FDelMsgList.Clear;
    unDeleteMsgs:=True;
  END;

  FUNCTION TPOP3Server.MarkMsgDeleted(AMsgNum: TMsgNum): Boolean;
  BEGIN
    FDelMsgList.Add(Pointer(AMsgNum));
    MarkMsgDeleted:=True;
  END;

  FUNCTION TPOP3Server.isDeleted(AMsgNum: TMsgNum): Boolean;
  BEGIN
    isDeleted:=FDelMsgList.IndexOf(Pointer(AMsgNum))<>-1;
  END;



  PROCEDURE TPOP3Server.cmdUser(VAR Message: TaslMessage);
  BEGIN
    FUser:=Message.Parm;
    CliSocket.WriteLn('+OK now send PASS');
  END;

  PROCEDURE TPOP3Server.cmdPass(VAR Message: TaslMessage);
  BEGIN
    IF FUser<>'' THEN
    BEGIN
      FPass:=Message.Parm;
      IF ValidateUser(FUser, FPass) THEN
      BEGIN
        CliSocket.WriteLn('+OK Welcome '+FUser);
        FState:=st_pop3Trans;
      END ELSE
      BEGIN
        CliSocket.WriteLn('-ERR Unknown user or password');
//        ClientHangup:=True;
      END;
    END ELSE
      CliSocket.WriteLn('-ERR Please send USER first');
  END;

  PROCEDURE TPOP3Server.cmdList(VAR Message: TaslMessage);
  VAR
    MsgNum, i : TMsgNum;
  BEGIN
    IF FState=st_pop3Trans THEN
    BEGIN
      IF Message.Parm<>'' THEN
      BEGIN
        MsgNum:=StrToIntDef(Message.Parm, 0);
        IF (MsgNum>0) AND (MsgNum<=GetMaxMsgs) AND (NOT isDeleted(MsgNum)) THEN
           CliSocket.WriteLn(Msg_pop3OK+' '+IntToStr(MsgNum)+' '+IntToStr(GetMsgSize(MsgNum)))
        ELSE
           CliSocket.WriteLn(msg_pop3UnknownMsg);
      END ELSE
      BEGIN
        CliSocket.WriteLn('+OK listing follows');
        FOR i:=1 TO GetMaxMsgs DO
          IF Not isDeleted(i) THEN
            CliSocket.WriteLn(IntToStr(i)+' '+IntToStr(GetMsgSize(i)));
        CliSocket.WriteLn('.');
      END;
    END ELSE
      CliSocket.WriteLn(msg_pop3NotLoggedIn);
  END;

  PROCEDURE TPOP3Server.cmdStat(VAR Message: TaslMessage);
  VAR
    TotSize : LongInt;
    i       : TMsgNum;
  BEGIN
    IF FState=st_pop3Trans THEN
    BEGIN
      TotSize:=0;
      FOR i:=1 TO GetMaxMsgs DO
        IF NOT isDeleted(i) THEN
          Inc(TotSize, GetMsgSize(i));
      CliSocket.WriteLn(msg_pop3OK+' '+IntToStr(GetMaxMsgs)+' '+IntToStr(TotSize));
    END ELSE
      CliSocket.WriteLn(msg_pop3NotLoggedIn);
  END;

  PROCEDURE TPOP3Server.cmdRetr(VAR Message: TaslMessage);
  VAR
    MsgNum, i : TMsgNum;
    Msg       : TStringList;
  BEGIN
    IF FState=st_pop3Trans THEN
    BEGIN
      MsgNum:=StrToIntDef(Message.Parm, 0);
      IF (MsgNum>0) AND (MsgNum<=GetMaxMsgs) AND (NOT isDeleted(MsgNum)) THEN
      BEGIN
        CliSocket.WriteLn('+OK Retriving message '+IntToStr(MsgNum));
        Msg:=TStringList.Create;
        GetMessage(MsgNum, Msg);
        FOR i:=0 TO Msg.Count-1 DO
          CliSocket.WriteLn(Msg[i]);
        CliSocket.WriteLn('.');
        Msg.Free;
      END ELSE
        CliSocket.WriteLn(msg_pop3UnknownMsg);
    END ELSE
      CliSocket.WriteLn(msg_pop3NotLoggedIn);
  END;

  PROCEDURE TPOP3Server.cmdDele(VAR Message: TaslMessage);
  VAR
    MsgNum : TMsgNum;
  BEGIN
    IF FState=st_pop3Trans THEN
    BEGIN
      MsgNum:=StrToIntDef(Message.Parm, 0);
      IF (MsgNum>0) AND (MsgNum<=GetMaxMsgs) AND (NOT isDeleted(MsgNum)) THEN
      BEGIN
        IF MarkMsgDeleted(MsgNum) THEN
          CliSocket.WriteLn('+OK Message '+IntToStr(MsgNum)+' deleted')
        ELSE
          CliSocket.WriteLn('-ERR Message '+IntToStr(MsgNum)+' can''t be deleted')
      END ELSE
        CliSocket.WriteLn(msg_pop3UnknownMsg);
    END ELSE
      CliSocket.WriteLn(msg_pop3NotLoggedIn);
  END;

  PROCEDURE TPOP3Server.cmdNoop(VAR Message: TaslMessage);
  BEGIN
    CliSocket.WriteLn(msg_pop3OK);
  END;

  PROCEDURE TPOP3Server.cmdRSet(VAR Message: TaslMessage);
  BEGIN
    IF FState=st_pop3Trans THEN
    BEGIN
      IF UnDeleteMsgs THEN
        CliSocket.WriteLn('+OK Messages reset')
      ELSE
        CliSocket.WriteLn('-ERR Messages can''t be reset');
    END ELSE
      CliSocket.WriteLn(msg_pop3NotLoggedIn);
  END;

  PROCEDURE TPOP3Server.cmdTop(VAR Message: TaslMessage);
  VAR
    MsgNum  : TmsgNum;
    MsgLines: Integer;
    Msg     : TStringList;
    i       : Integer;
  BEGIN
    IF FState=st_pop3Trans THEN
    BEGIN
      MsgNum:=StrToIntDef(NextWord(Message.Parm), 0);
      IF NOT isDeleted(MsgNum) AND (MsgNum>0) AND (MsgNum<=GetMaxMsgs) THEN
      BEGIN
        MsgLines:=StrToIntDef(Message.Parm, -1);
        IF MsgLines>=0 THEN
        BEGIN
          CliSocket.WriteLn('+OK Retriving top of message '+IntToStr(MsgNum));
          Msg:=TStringList.Create;
          GetMessageTop(MsgNum, MsgLines, Msg);
          FOR i:=0 TO Msg.Count-1 DO
            CliSocket.WriteLn(Msg[i]);
          CliSocket.WriteLn('.');
          Msg.Free;
        END ELSE
          CliSocket.WriteLn(msg_pop3Err+' Invalid parameter');
      END ELSE
        CliSocket.WriteLn(msg_pop3UnknownMsg);
    END ELSE
      CliSocket.WriteLn(msg_pop3NotLoggedIn);
  END;

  PROCEDURE TPOP3Server.cmdUidl(VAR Message: TaslMessage);
  VAR
    MsgNum, i : TMsgNum;
  BEGIN
    IF FState=st_pop3Trans THEN
    BEGIN
      IF Message.Parm<>'' THEN
      BEGIN
        MsgNum:=StrToIntDef(Message.Parm, 0);
        IF (MsgNum>0) AND (MsgNum<=GetMaxMsgs) AND (NOT isDeleted(MsgNum)) THEN
           CliSocket.WriteLn(Msg_pop3OK+' '+IntToStr(MsgNum)+' '+GetMsgUidl(MsgNum))
        ELSE
           CliSocket.WriteLn(msg_pop3UnknownMsg);
      END ELSE
      BEGIN
        CliSocket.WriteLn('+OK listing follows');
        FOR i:=1 TO GetMaxMsgs DO
          IF Not isDeleted(i) THEN
            CliSocket.WriteLn(IntToStr(i)+' '+GetMsgUidl(i));
        CliSocket.WriteLn('.');
      END;
    END ELSE
      CliSocket.WriteLn(msg_pop3NotLoggedIn);
  END;


  PROCEDURE TPOP3Server.cmdQuit(VAR Message: TaslMessage);
  VAR
    i : TMsgNum;
  BEGIN
    FState:=st_pop3Update;
    FOR i:=0 TO FDelMsgList.Count-1 DO
      PhysDeleteMsg(TMsgNum(FDelMsgList[i]));
    ClientQuit:=True;
  END;

END.

