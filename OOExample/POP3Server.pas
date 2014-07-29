{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.3 $    $Date: 2002/10/01 01:07:33 $    $Author: sag $ }
{                                                                             }
{ Post Office Protocol - Version 3 (POP3) minimal server example              }
PROGRAM POP3Server;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  Classes, aslGlobals, aslTCPSocket, aslAbsServer, aslPOP3Server;

TYPE
  TMyPOP3Server = CLASS(TPOP3Server)
    FUNCTION  ValidateUser(AUser, APass: String): Boolean; OVERRIDE;
    FUNCTION  GetMaxMsgs: TMsgNum; OVERRIDE;
    FUNCTION  GetMsgSize(AMsgNum: TMsgNum): LongInt; OVERRIDE;
    FUNCTION  GetMessage(AMsgNum: TMsgNum; AMsg: TStringList): Boolean; OVERRIDE;
    FUNCTION  PhysDeleteMsg(AMsgNum: TMsgNum): Boolean; OVERRIDE;

    { If you want to support the 'TOP' command, implement the following function }
    { and add the following line to your constructor:                            }
    {   CmdColl.Add('TOP',  cmd_pop3Top);                                        }
    {                                                                            }
//    FUNCTION  GetMessageTop(AMsgNum: TMsgNum; AMsgLines: Integer; AMsg: TStringList): Boolean; OVERRIDE

    { If you want to support the 'UIDL' command implement the following function }
    { and add the following line to your constructor:                            }
    {   CmdColl.Add('UIDL', cmd_pop3Uidl);                                       }
    {                                                                            }
//    FUNCTION  GetMsgUidl(AMsgNum: TMsgNum): STRING; OVERRIDE;

  END;

  FUNCTION TMyPOP3Server.ValidateUser(AUser, APass: String): Boolean;
  BEGIN
    ValidateUser:=True;
  END;

  FUNCTION TMyPOP3Server.GetMaxMsgs: TMsgNum;
  BEGIN
    GetMaxMsgs:=10;
  END;

  FUNCTION TMyPOP3Server.GetMsgSize(AMsgNum: TMsgNum): LongInt;
  BEGIN
    GetMsgSize:=AMsgNum*7;
  END;

  FUNCTION TMyPOP3Server.GetMessage(AMsgNum: TMsgNum; AMsg: TStringList): Boolean;
  BEGIN
    GetMessage:=True;
  END;

  FUNCTION TMyPOP3Server.PhysDeleteMsg(AMsgNum: TMsgNum): Boolean;
  BEGIN
    PhysDeleteMsg:=True;
  END;


VAR
  S: TServerManager;

  FUNCTION CreatePOP3(ASvrMgr: TServermanager; ACliSocket: TTCPClientSocket): TAbsServer;
  BEGIN
    CreatePOP3:=TMyPOP3Server.Create(ASvrMgr, ACliSocket, 'POP3 server. '+aslPartOf+' R'+aslRelease);
  END;

BEGIN
  S:=TServerManager.Create('pop3', CreatePOP3);
  S.Execute;
  S.Destroy;
END.

