{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.8 $    $Date: 2002/10/01 01:07:32 $    $Author: sag $ }
{                                                                             }
{ Network News Transfer Protocol (NNTP) client example                        }
PROGRAM NNTPClient;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  Classes, SysUtils, aslTCPSocket, aslNNTPClient;

TYPE
  TMyNNTPClient = CLASS(TNNTPClient)
    PROCEDURE EvtLogLine(Sender: TObject; Msg: String);
  END;

VAR
  Cli : TMyNNTPCLient;
  SLst, Msg : TStringList;
  c : Char;
  i : Integer;
  s : String;


  PROCEDURE TMyNNTPClient.EvtLogLine(Sender: TObject; Msg: String);
  begin
    WriteLn(msg);
  end;

BEGIN
 TRY
  Cli:=TMyNNTPClient.Create;
  Cli.OnLogLine:=Cli.EvtLogLine;
  Cli.Connect('news.ecomstation.com');
//  WriteLn(Cli.WelcomeMsg);

// Get and write groups to screen
  SLst:=TStringList.Create;
  Cli.List(SLst);

  FOR i:=0 TO SLst.Count-1 DO
    WriteLn(SLst[i]);

// select group and get messages
  s:=Cli.Group('ecomstation.support.networking');

  Msg:=TStringList.Create;
  FOR i:=1 TO 200 DO    // Get these values from s above
  BEGIN
    Cli.ArticleByNo(i, Msg);
    Msg.SaveToFile(int2Str(i)+'.msg');
    Msg.Clear;
  END;

 EXCEPT
   ON E:Exception DO
     WriteLn(E.Message);
 END;
  SLst.Destroy;
  Cli.Destroy;
END.

