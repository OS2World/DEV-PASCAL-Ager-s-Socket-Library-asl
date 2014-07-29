{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.4 $    $Date: 2002/09/30 20:32:51 $    $Author: sag $ }
{                                                                             }
{ File Transfer Protocol (FTP) client class (RFC 959)                         }
UNIT aslFTPClient;

{$I aslDefine.Inc}

INTERFACE

USES Classes, SysUtils, aslSocket, aslAbsSocket, aslAbsClient, aslTCPSocket;

TYPE
  TFTPWriteFileProc = PROCEDURE(VAR Buffer; Size: Integer) OF OBJECT;
  TFTPReadFileProc  = FUNCTION(VAR Buffer; Size: Integer; VAR Actual: Integer): Boolean OF OBJECT;


  EAbsFTPClient = CLASS(EaslException);

  TAbsFTPClient = CLASS(TAbsClient)
  PROTECTED
    FPassive    : Boolean;
    FDataSocket : TTCPClientSocket;
    FDataPort   : String;
    FDataIP     : String;

    FUNCTION ConnectDataSocket: Integer;
    FUNCTION CloseDataSocket: Integer;
  PUBLIC
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;
    PROCEDURE Connect(Host: STRING); OVERRIDE;

{ Standard commands }
    PROCEDURE User(Name: String);
    PROCEDURE Pass(Password: String);
    PROCEDURE Acct(AccountInfo: String);
    PROCEDURE Cwd(Path: String);
    PROCEDURE CdUp;
    PROCEDURE SMnt(Path: String);
    PROCEDURE ReIn;
    PROCEDURE Quit;

    PROCEDURE Port(Port: String); VIRTUAL; { x,x,x,x,y,y (x=hostno, y=portno) }
    PROCEDURE Pasv; VIRTUAL;
    PROCEDURE Type_(AType: String);     { A[<sp> N | T |C ] |
                                          E[<sp> N | T |C ] |
                                          I |
                                          L <byte-size>}
    PROCEDURE Stru(Structure: String);  { F | R | P }
    PROCEDURE Mode(AMode: String);      { S | B | C }

    PROCEDURE Retr(RemoteFileName: String; WriteProc: TFTPWriteFileProc);
    PROCEDURE Stor(FileName: String; ReadProc: TFTPReadFileProc);
    FUNCTION  StoU(ReadProc: TFTPReadFileProc): String;
    PROCEDURE Appe(FileName: String; ReadProc: TFTPReadFileProc);
    PROCEDURE Allo(Size, RecSize: LongInt);
    PROCEDURE Rest(From: String);
    PROCEDURE RnFr(FromName: String);
    PROCEDURE RnTo(ToName: String);
    PROCEDURE Abor;
    PROCEDURE Dele(FileName: String);
    PROCEDURE RmD(Path: String);
    PROCEDURE MkD(Path: String);
    FUNCTION  PwD: String;
    PROCEDURE List(FileSpec: String; DirList: TStringList);
    PROCEDURE NLst(FileSpec: String; DirList: TStringList);
    PROCEDURE Site(Cmd: String);
    FUNCTION  Syst: String;
    PROCEDURE Stat(FileSpec: String; StatInfo: TStringList);
    PROCEDURE Help(Command: String; HelpInfo: TStringList);
    PROCEDURE Noop;

    PROPERTY DataPort: String READ FDataPort WRITE FDataPort;
    PROPERTY DataIP: String   READ FDataIP   WRITE FDataIP;
    PROPERTY Passive: Boolean READ FPassive  WRITE FPassive;
  END;

  TFTPClient = CLASS(TAbsFTPClient)
  PUBLIC
    PROCEDURE Connect(Host, Name, Password: STRING);
    PROCEDURE Port(Port: String); OVERRIDE;
    PROCEDURE Pasv; OVERRIDE;
  END;

IMPLEMENTATION

USES VPUtils, aslUtils;

{ TAbsFTPClient }

  CONSTRUCTOR TAbsFTPClient.Create;
  BEGIN
    INHERITED Create;
    Passive:=False;
    Service:='ftp';
    FDataPort:='ftp-data';
  END;

  DESTRUCTOR TAbsFTPClient.Destroy;
  BEGIN
    IF Assigned(Socket) THEN Quit;
    INHERITED Destroy;
  END;

  PROCEDURE TAbsFTPClient.Connect(Host: STRING);
  BEGIN
    INHERITED Connect(Host);
    IF LastResponseCode<>220 THEN
      RAISE EAbsFTPClient.Create('Connect: Error connecting to server.', LastResponse);
  END;


  PROCEDURE TAbsFTPClient.User(Name: String);
  BEGIN
    SendCommand('USER '+Name);
    IF (LastResponseCode<>230) AND (LastResponseCode<>331) THEN
      RAISE EAbsFTPClient.Create('User', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Pass(Password: String);
  BEGIN
    SendCommand('PASS '+Password);
    IF (LastResponseCode<>230) AND (LastResponseCode<>202) THEN
      RAISE EAbsFTPClient.Create('Pass', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Acct(AccountInfo: String);
  BEGIN
    SendCommand('ACCT '+AccountInfo);
    IF (LastResponseCode<>230) AND (LastResponseCode<>202) THEN
      RAISE EAbsFTPClient.Create('Acct', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Cwd(Path: String);
  BEGIN
    SendCommand('CWD '+Path);
    IF LastResponseCode<>250 THEN
      RAISE EAbsFTPClient.Create('Cwd', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.CdUp;
  BEGIN
    SendCommand('CDUP');
    IF LastResponseCode<>200 THEN
      RAISE EAbsFTPClient.Create('CdUp', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.SMnt(Path: String);
  BEGIN
    SendCommand('SMNT '+Path);
    IF (LastResponseCode<>202) AND (LastResponseCode<>250) THEN
      RAISE EAbsFTPClient.Create('SMnt', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.ReIn;
  BEGIN
    SendCommand('REIN');
    IF (LastResponseCode<>120) AND (LastResponseCode<>220) THEN
      RAISE EAbsFTPClient.Create('ReIn', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Quit;
  BEGIN
    SendCommand('QUIT');
    IF LastResponseCode<>221 THEN
      RAISE EAbsFTPClient.Create('Quit', LastResponse);

// Free???
    Socket.Destroy;
    FSocket:=Nil;
  END;


  PROCEDURE TAbsFTPClient.Port(Port: String);
  BEGIN
    SendCommand('PORT '+Port);
    IF LastResponseCode<>200 THEN
      RAISE EAbsFTPClient.Create('Port', LastResponse);

    FDataIP:=NextWordDelim(Port,',');
    FDataIP:=FDataIP+'.'+NextWordDelim(Port,',');
    FDataIP:=FDataIP+'.'+NextWordDelim(Port,',');
    FDataIP:=FDataIP+'.'+NextWordDelim(Port,',');

    FDataPort:=Int2Str(StrToInt(NextWordDelim(Port,','))*256+StrToInt(Port));
  END;

  PROCEDURE TAbsFTPClient.Pasv;
  VAR
    S         : String;
    StartPos,
    EndPos    : Integer;
  BEGIN
    Passive:=True;
    SendCommand('PASV');
    IF LastResponseCode<>227 THEN
      RAISE EAbsFTPClient.Create('Pasv', LastResponse);

    StartPos:=Pos('(', LastResponse)+1;
    EndPos:=Pos(')', LastResponse);

    S:=Copy(LastResponse, StartPos, EndPos-Pos('(', LastResponse)-1);
    FDataIP:=NextWordDelim(S,',');
    FDataIP:=FDataIP+'.'+NextWordDelim(S,',');
    FDataIP:=FDataIP+'.'+NextWordDelim(S,',');
    FDataIP:=FDataIP+'.'+NextWordDelim(S,',');

    FDataPort:=Int2Str(StrToInt(NextWordDelim(S,','))*256+StrToInt(S));
  END;

  PROCEDURE TAbsFTPClient.Type_(AType: String);
  BEGIN
    SendCommand('TYPE '+AType);
    IF LastResponseCode<>200 THEN
      RAISE EAbsFTPClient.Create('Type', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Stru(Structure: String);
  BEGIN
    SendCommand('STRU '+Structure);
    IF LastResponseCode<>200 THEN
      RAISE EAbsFTPClient.Create('Stru', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Mode(AMode: String);
  BEGIN
    SendCommand('MODE '+AMode);
    IF LastResponseCode<>200 THEN
      RAISE EAbsFTPClient.Create('Mode', LastResponse);
  END;


  PROCEDURE TAbsFTPClient.Retr(RemoteFileName: String; WriteProc: TFTPWriteFileProc);
  VAR
    Act : Integer;
    Buf : ARRAY[0..1023] OF Byte;
  BEGIN
    IF FPassive THEN Pasv ELSE Port('');
    SendCommand('RETR '+RemoteFileName);
    IF (LastResponseCode<>125) AND (LastResponseCode<>150) THEN
      RAISE EAbsFTPClient.Create('Retr', LastResponse);

    IF Not FPassive THEN ConnectDataSocket;
// bin/txt mode?
    TRY
      REPEAT
        Act:=FDataSocket.Read(Buf, SizeOf(Buf));
        WriteProc(Buf, Act);
      UNTIL Act=0;
    EXCEPT
      ON e: ETCPClientSocket DO FDataSocket.Destroy;
      ELSE RAISE;
    END;

    CloseDataSocket;
    GetResponse(Nil);
    IF (LastResponseCode<>226) AND (LastResponseCode<>250) THEN
      RAISE EAbsFTPClient.Create('Retr (Getting data)', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Stor(FileName: String; ReadProc: TFTPReadFileProc);
  VAR
    Buf : ARRAY[0..1023] OF Byte;
    Act : Integer;
  BEGIN
    IF Passive THEN Pasv ELSE Port('');
    SendCommand('STOR '+FileName);
    IF (LastResponseCode<>125) AND (LastResponseCode<>150) THEN
      RAISE EAbsFTPClient.Create('Stor', LastResponse);

    IF Not Passive THEN ConnectDataSocket;

    WHILE ReadProc(Buf, SizeOf(Buf), Act) DO
      FDataSocket.Write(Buf, Act);

    CloseDataSocket;
    GetResponse(Nil);
    IF (LastResponseCode<>226) AND (LastResponseCode<>250) THEN
      RAISE EAbsFTPClient.Create('Stor (Saving data)', LastResponse);
  END;

  FUNCTION TAbsFTPClient.StoU(ReadProc: TFTPReadFileProc): String;
  VAR
    Buf : ARRAY[0..1023] OF Byte;
    Act : Integer;
  BEGIN
    IF Passive THEN Pasv ELSE Port('');
    SendCommand('STOU');
    IF (LastResponseCode<>125) AND (LastResponseCode<>150) THEN
      RAISE EAbsFTPClient.Create('', LastResponse);

    IF Not Passive THEN ConnectDataSocket;

    WHILE ReadProc(Buf, SizeOf(Buf), Act) DO
      FDataSocket.Write(Buf, Act);

    CloseDataSocket;
    GetResponse(Nil);
    IF (LastResponseCode<>226) AND (LastResponseCode<>250) THEN
      RAISE EAbsFTPClient.Create('StoU (Saving data)', LastResponse);
    StoU:=Copy(LastResponse, 5, Pos('"', LastResponse)-6);
  END;

  PROCEDURE TAbsFTPClient.Appe(FileName: String; ReadProc: TFTPReadFileProc);
  VAR
    Buf : ARRAY[0..1023] OF Byte;
    Act : Integer;
  BEGIN
    IF Passive THEN Pasv ELSE Port('');
    SendCommand('APPE '+FileName);
    IF (LastResponseCode<>125) AND (LastResponseCode<>150) THEN
      RAISE EAbsFTPClient.Create('Appe', LastResponse);

    IF Not Passive THEN ConnectDataSocket;

    WHILE ReadProc(Buf, SizeOf(Buf), Act) DO
      FDataSocket.Write(Buf, Act);

    CloseDataSocket;
    GetResponse(Nil);
    IF (LastResponseCode<>226) AND (LastResponseCode<>250) THEN
      RAISE EAbsFTPClient.Create('Appe (Saving data)', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Allo(Size, RecSize: LongInt);
  VAR
    S : String;
  BEGIN
    IF RecSize<>0 THEN S:=' R '+IntToStr(RecSize) ELSE S:='';
    SendCommand('ALLO '+IntToStr(Size)+S);
    IF (LastResponseCode<>200) AND (LastResponseCode<>202) THEN
      RAISE EAbsFTPClient.Create('Allo', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Rest(From: String);
  BEGIN
    SendCommand('REST '+From);
    IF LastResponseCode<>350 THEN
      RAISE EAbsFTPClient.Create('Rest', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.RnFr(FromName: String);
  BEGIN
    SendCommand('RNFR '+FromName);
    IF LastResponseCode<>350 THEN
      RAISE EAbsFTPClient.Create('RnFn', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.RnTo(ToName: String);
  BEGIN
    SendCommand('RNTN '+ToName);
    IF LastResponseCode<>250 THEN
      RAISE EAbsFTPClient.Create('RnTo', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Abor;
  BEGIN
    SendCommand('ABOR');
    IF (LastResponseCode<>225) AND (LastResponseCode<>226) THEN
      RAISE EAbsFTPClient.Create('Abor', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Dele(FileName: String);
  BEGIN
    SendCommand('DELE '+FileName);
    IF LastResponseCode<>250 THEN
      RAISE EAbsFTPClient.Create('Dele', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.RmD(Path: String);
  BEGIN
    SendCommand('RMD '+Path);
    IF LastResponseCode<>250 THEN
      RAISE EAbsFTPClient.Create('RmD', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.MkD(Path: String);
  BEGIN
    SendCommand('MKD '+Path);
    IF LastResponseCode<>257 THEN
      RAISE EAbsFTPClient.Create('MkD', LastResponse);
  END;

  FUNCTION TAbsFTPClient.PwD: String;
  BEGIN
    SendCommand('PWD');
    IF LastResponseCode<>257 THEN
      RAISE EAbsFTPClient.Create('PWD', LastResponse);
    PwD:=Copy(LastResponse, 5, Pos('"', LastResponse)-6);
  END;

  PROCEDURE TAbsFTPClient.List(FileSpec: String; DirList: TStringList);
  VAR
    S : String;
  BEGIN
    IF Passive THEN Pasv ELSE Port('');
    SendCommand('LIST '+FileSpec);
    IF Not Passive THEN ConnectDataSocket;
    IF (LastResponseCode<>125) AND (LastResponseCode<>150) THEN
      RAISE EAbsFTPClient.Create('List', LastResponse);


    TRY
      REPEAT
        FDataSocket.ReadLn(S);
        IF DirList<>nil THEN DirList.Add(S);
      UNTIL NOT FDataSocket.Connected;
    EXCEPT
      ON e: ETCPClientSocket DO FDataSocket.Destroy;
      ELSE RAISE;
    END;
    CloseDataSocket;

    GetResponse(Nil);
    IF (LastResponseCode<>226) AND (LastResponseCode<>250) THEN
      RAISE EAbsFTPClient.Create('List (Getting data)', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.NLst(FileSpec: String; DirList: TStringList);
  VAR
    S : String;
  BEGIN
    IF Passive THEN Pasv ELSE Port('');
    SendCommand('NLST '+FileSpec);
    IF (LastResponseCode<>125) AND (LastResponseCode<>150) THEN
      RAISE EAbsFTPClient.Create('NLst', LastResponse);

    IF Not Passive THEN ConnectDataSocket;

    TRY
      REPEAT
        FDataSocket.ReadLn(S);
        IF DirList<>nil THEN DirList.Add(S);
      UNTIL NOT FDataSocket.Connected;
    EXCEPT
      ON e: ETCPClientSocket DO FDataSocket.Destroy;
      ELSE RAISE;
    END;
    CloseDataSocket;

    GetResponse(Nil);
    IF (LastResponseCode<>226) AND (LastResponseCode<>250) THEN
      RAISE EAbsFTPClient.Create('NLst (Getting data)', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Site(Cmd: String);
  BEGIN
    SendCommand('SITE '+Cmd);
    IF (LastResponseCode<>200) AND (LastResponseCode<>202) THEN
      RAISE EAbsFTPClient.Create('', LastResponse);
  END;

  FUNCTION TAbsFTPClient.Syst: String;
  BEGIN
    SendCommand('SYST');
    IF LastResponseCode<>215 THEN
      RAISE EAbsFTPClient.Create('Syst', LastResponse);
    Syst:=Copy(LastResponse, 5, Length(LastResponse)-4);
  END;

  PROCEDURE TAbsFTPClient.Stat(FileSpec: String; StatInfo: TStringList);
  VAR
    S : String;
  BEGIN
    SendCommandEx('STAT '+FileSpec, StatInfo);
    IF (LastResponseCode<>211) AND (LastResponseCode<>212) AND (LastResponseCode<>213) THEN
      RAISE EAbsFTPClient.Create('Stat', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Help(Command: String; HelpInfo: TStringList);
  BEGIN
    SendCommandEx('HELP '+Command, HelpInfo);
    IF (LastResponseCode<>211) AND (LastResponseCode<>214) THEN
      RAISE EAbsFTPClient.Create('Help', LastResponse);
  END;

  PROCEDURE TAbsFTPClient.Noop;
  BEGIN
    SendCommand('NOOP');
    IF LastResponseCode<>200 THEN
      RAISE EAbsFTPClient.Create('Noop', LastResponse);
  END;


  FUNCTION TAbsFTPClient.ConnectDataSocket: Integer;
  VAR
    FSvrSocket : TTCPServerSocket;
  BEGIN
    IF NOT FPassive THEN
    BEGIN
      FSvrSocket:=TTCPServerSocket.Create(FDataPort);
      FSvrSocket.Listen;
      FDataSocket:=FSvrSocket.AcceptConnection;
      FSvrSocket.Destroy;
    END ELSE
    BEGIN
      FDataSocket:=TTCPClientSocket.Create;
      FDataSocket.Connect(FDataIP, FDataPort);
    END;
  END;

  FUNCTION TAbsFTPClient.CloseDataSocket: Integer;
  BEGIN
    IF FDataSocket<>NIL THEN
    BEGIN
      FDataSocket.Close;
      FDataSocket.Destroy;
      FDataSocket:=Nil;
    END;
  END;



{ TFTPClient }

  PROCEDURE TFTPClient.Connect(Host, Name, Password: STRING);
  BEGIN
    INHERITED Connect(Host);
    IF Name<>'' THEN User(Name);
    IF Password<>'' THEN Pass(Password);
  END;

  PROCEDURE TFTPClient.Port(Port: STRING);
  VAR
    FSvrSocket : TTCPServerSocket;
    PortNum, Ok: LongInt;
    S          : STRING;
  BEGIN
//!! Check RFC for port number usage
    Val(FDataPort, PortNum, Ok);
    IF (Ok=0) AND (PortNum>=10000) AND (PortNum<65000) THEN
    BEGIN
       FDataPort:=Int2Str(StrToInt(FDataPort)+1);
    END ELSE
       FDataPort:='10000';

    S:=SockClientIP;
    Ok:=Pos('.', S);
    WHILE (Ok>0) DO
    BEGIN
      S[Ok]:=',';
      Ok:=Pos('.', S);
    END;

    INHERITED Port(S+','+IntToStr(Hi(StrToInt(DataPort)))+','+IntToStr(Lo(StrToInt(DataPort))));
  END;

  PROCEDURE TFTPClient.Pasv;
  BEGIN
    INHERITED Pasv;
    ConnectDataSocket;
  END;

END.

