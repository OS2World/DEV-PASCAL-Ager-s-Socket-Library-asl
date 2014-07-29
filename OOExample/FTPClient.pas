{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.2 $    $Date: 2002/10/01 01:07:32 $    $Author: sag $ }
{                                                                             }
{ File Transfer Protocol client example                                       }
PROGRAM FTPClient;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  Classes, aslFTPClient;

TYPE
  TMyFTPClient = CLASS(TFTPClient)
  PUBLIC
    f : File;

    FUNCTION DoReadFile(VAR Buffer; Size: Integer; VAR Actual: Integer): Boolean;
  END;

VAR
  FTPCli : TMyFTPClient;
  SL     : TStringList;

  FUNCTION TMyFTPClient.DoReadFile(VAR Buffer; Size: Integer; VAR Actual: Integer): Boolean;
  BEGIN
    BlockRead(f, Buffer, Size, Actual);
    Write(FilePos(f),#13);
    DoReadFile:=(Actual=Size);
  END;

  PROCEDURE PrintSL;
  VAR
    i : Integer;
  BEGIN
    FOR i:=0 TO SL.Count-1 DO
      WriteLn(SL[i]);
    SL.Clear;
  END;

BEGIN
  FTPCli:=TMyFTPClient.Create;
  FTPCli.Connect('ftp.', 'anonymous', 'asl@poboxes.com');

  SL:=TStringList.Create;
  FTPCli.List('*',SL);
  WriteLn('List   : '+FtpCli.LastResponse);
  PrintSL;

  FTPCli.Help('', SL);
  WriteLn('Help   : '+FtpCli.LastResponse);
  PrintSL;

//  FTPCli.Cwd('x');
//  WriteLn('Cwd    : '+FtpCli.LastResponse);

  Assign(FtpCli.f, 'e:\jpd0942.zip'); Reset(FtpCli.f,1);
  FTPCli.Stor('jpd0942.zip', FtpCli.DoReadFile);
  Close(FtpCli.f);

  FTPCli.Quit;
  WriteLn('Quit   : '+FtpCli.LastResponse);

  FTPCli.Destroy;
END.
