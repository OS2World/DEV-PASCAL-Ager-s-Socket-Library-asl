{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.2 $    $Date: 2002/10/01 01:07:32 $    $Author: sag $ }
{                                                                             }
{ Concurrent Version System (CVS) client program                              }
PROGRAM CVSClient;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  Application, aslCVSClient;

TYPE
  TCVSClientApp = CLASS(TaslApplication)
  PRIVATE
    Server : STRING;
    User   : STRING;
    Pass   : STRING;

  PUBLIC
    PROCEDURE ShowCopyright; OVERRIDE;
//    PROCEDURE ShowHelp; OVERRIDE;
//    FUNCTION  ParseCmdLine: Boolean; OVERRIDE;
    PROCEDURE Run; OVERRIDE;

//    PROCEDURE GetTheMail;

    PROCEDURE LogOn;
  END;


  PROCEDURE TCVSClientApp.ShowCopyright;
  BEGIN
    INHERITED ShowCopyright;
//    WriteLn('CVS Client.');
    WriteLn;
  END;

  PROCEDURE TCVSClientApp.Run;
  BEGIN
    INHERITED Run;

    Logon;
  END;

  PROCEDURE TCVSClientApp.Logon;
  VAR
    Client : TCVSClient;
  BEGIN
    Client:=TCVSClient.Create;
    Client.OnLogLine:=SocketLogLine;
    Client.Connect('cvs');
    Client.PServerAuth('d:/cvsroot', 'sag', 'password');
    Client.Disconnect;
    Client.Destroy;
  END;


VAR
  App: TCVSClientApp;

BEGIN
  App:=TCVSClientApp.Create('CVS Client');
  App.Run;
  App.Destroy;
END.

