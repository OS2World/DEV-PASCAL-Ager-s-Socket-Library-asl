{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.29 $    $Date: 2002/09/29 21:58:49 $    $Author: sag $ }
{                                                                             }
{ OO interface to TCP sockets                                                 }
UNIT aslTCPSocket;

{$I aslDefine.Inc}

INTERFACE

USES SysUtils, VPUtils, Classes, CTypes, aslSocket, aslAbsSocket,
{$IFDEF OS2}
     OS2Socket, NetDB, SockIn, Utils;
{$ELSE}
     Winsock;
{$ENDIF}

CONST
  BufSize = 4096;

TYPE
  ETCPSocket = CLASS(EaslException);
  ETCPClientSocket = CLASS(EaslException);
  EBufTCPClientSocket = CLASS(EaslException);
  ETCPServerSocket = CLASS(EaslException);

  TTCPSocket = CLASS(TAbsSocket)
  PRIVATE
    FPeerIP    : STRING;
    FPeerName  : STRING;
  PUBLIC
    CONSTRUCTOR Create;

    PROPERTY PeerIP: STRING             READ FPeerIP;
    PROPERTY PeerName: STRING           READ FPeerName;
  END;

  TTCPClientSocket = CLASS(TTCPSocket)
  PRIVATE
    FOnReadLn  : TNotifyEvent;
    FOnWriteLn : TNotifyEvent;
    FLineSep   : STRING;
  PROTECTED
    PROCEDURE DoReadLn;
    PROCEDURE DoWriteLn;
  PUBLIC
    CONSTRUCTOR Create;
    PROCEDURE Connect(adr: STRING; PORT: STRING);
    PROCEDURE WaitForData; VIRTUAL;
    FUNCTION WaitForDataTime(Timeout: Word): Integer; VIRTUAL;
    FUNCTION Read(VAR Buf; len: Word): Integer; VIRTUAL;
    FUNCTION ReadLn(VAR S: STRING): Integer; VIRTUAL;
    FUNCTION Write(VAR Buf; len: Word): Integer;
    FUNCTION WriteLn(S: STRING): Integer;

    PROPERTY OnReadLn: TNotifyEvent   READ FOnReadLn  WRITE FOnReadLn;
    PROPERTY OnWriteLn: TNotifyEvent  READ FOnWriteLn WRITE FOnWriteLn;
    PROPERTY LineSep: STRING          READ FLineSep   WRITE FLineSep;
  END;

  TBufTCPClientSocket = CLASS(TTCPClientSocket)
  PRIVATE
    FReadBuf : ARRAY[0..BufSize] OF Char;
    FBufPos  : Integer;
    FBufEnd  : Integer;
  PUBLIC
    CONSTRUCTOR Create;
    PROCEDURE WaitForData; OVERRIDE;
    FUNCTION WaitForDataTime(Timeout: Word): Integer; OVERRIDE;
    FUNCTION Read(VAR Buf; len: Word): Integer; OVERRIDE;
    FUNCTION ReadLn(VAR S: STRING): Integer; OVERRIDE;
  END;

  TTCPServerSocket = CLASS(TTCPSocket)
  PUBLIC
    CONSTRUCTOR Create(Port: STRING);
    PROCEDURE Listen;
    PROCEDURE WaitForConnection;
    FUNCTION AcceptConnection: TTCPClientSocket;
  END;

IMPLEMENTATION

{ TTCPSocket }

  CONSTRUCTOR TTCPSocket.Create;
  BEGIN
    INHERITED Create;
    Protocol:='tcp';
    FPeerName:='';
    FPeerIP:='';
  END;


{ TTCPClientSocket }

  CONSTRUCTOR TTCPClientSocket.Create;
  BEGIN
    INHERITED Create;
    FOnReadLn:=NIL;
    FOnWriteLn:=NIL;
    FLineSep:=#13#10;
  END;

  PROCEDURE TTCPClientSocket.Connect(adr: STRING; PORT: STRING);
  VAR
    phe   : PHostEnt;
    sin   : sockaddr_in;
    PE    : PProtoEnt;
  BEGIN
    DoLogLine('Creating client socket...');

    PE:=SockGetProtoByName(Protocol);  // ERROR check
    INHERITED Socket(PF_INET, SOCK_STREAM, PE^.p_proto);
    IF SockErrNo<>0 THEN
      RAISE ETCPClientSocket.Create('Connect', IntToStr(SockErrNo));
    sin.sin_family:=AF_INET;
    sin.sin_port:=ResolvePort(Port);
    sin.sin_addr.s_addr:=SockInetAddr(Adr);
    IF sin.sin_addr.s_addr=INADDR_NONE THEN
    BEGIN
      DoLogLine(Format('%s does not look like ip. Trying to use it as host name ',[adr]));
      phe:=SockGetHostByName(adr);
      IF not Assigned(phe) THEN
        RAISE ETCPClientSocket.Create('Connect: Host name '+adr+' unknown.', IntToStr(SockErrNo));
      DoLogLine('resolved host name');
      Sin.sin_addr:=phe^.h_addr^^;
    END;
    DoLogLine(Format('trying to connect to %s:%s',[adr, PORT]));
    IF INHERITED Connect(sockaddr(sin), SizeOf(sin))<0 THEN
      RAISE ETCPSocket.Create('Connect: Cannot connect to '+adr+':'+Port, IntToStr(SockErrNo));
    FPeerName:=adr;
  END;

  PROCEDURE TTCPClientSocket.WaitForData;
  VAR
    Sock : Integer;
  BEGIN
    DoLogLine(Format('Waiting for data on socket %d',[SocketHandle]));
    Sock:=SocketHandle;
    IF INHERITED Select(@Sock,1,0,0,-1)<0 THEN
      RAISE ETCPClientSocket.Create('WaitForData', IntToStr(SockErrNo));
  END;

  FUNCTION TTCPClientSocket.WaitForDataTime(Timeout: Word): Integer;
  VAR
    Sock : Integer;
  BEGIN
    DoLogLine(Format('Waiting for data on socket %d for %d microseconds',[SocketHandle, Timeout]));
    Sock:=SocketHandle;
    Result:=Select(@Sock,1,0,0,Timeout);
    IF Result<0 THEN
      RAISE ETCPClientSocket.Create('WaitForDataTime', IntToStr(SockErrNo));
  END;

  FUNCTION TTCPClientSocket.Read(VAR Buf; len: Word): Integer;
  BEGIN
    DoLogLine(Format('Reading data on socket %d...',[SocketHandle]));
    Result:=Recv(buf,len,0);
    IF Result<0 THEN
      RAISE ETCPClientSocket.Create('Read', IntToStr(SockErrNo));
  END;

  FUNCTION TTCPClientSocket.ReadLn(VAR S: STRING): Integer;
  VAR
    c : Char;
  BEGIN
    DoLogLine(Format('Reading data on socket %d...',[SocketHandle]));
    S:='';
    REPEAT
      Result:=Recv(c, SizeOf(c), 0);
      IF (c<>#10) AND (c<>#13) THEN S:=S+c;
    UNTIL (c=#10) OR (Result<=0);
    IF Result<0 THEN
      RAISE ETCPClientSocket.Create('ReadLn', IntToStr(SockErrNo));
    Result:=Length(S);
    DoReadLn;
  END;

  FUNCTION TTCPClientSocket.Write(VAR Buf; len: Word): Integer;
  BEGIN
    DoLogLine(Format('Writing data to socket %d...',[SocketHandle]));
    Result:=Send(Buf, Len, 0);
    IF Result<0 THEN
      RAISE ETCPClientSocket.Create('Write', IntToStr(SockErrNo));
  END;

  FUNCTION TTCPClientSocket.WriteLn(S: STRING): Integer;
  BEGIN
    S:=S+FLineSep;
    Result:=Write(S[1],Length(S));
    DoWriteLn;
  END;

  PROCEDURE TTCPClientSocket.DoReadLn;
  BEGIN
    IF Assigned(FOnReadLn) THEN FOnReadLn(Self);
  END;

  PROCEDURE TTCPClientSocket.DoWriteLn;
  BEGIN
    IF Assigned(FOnWriteLn) THEN FOnWriteLn(Self);
  END;



{ TBufTCPClientSocket }

  CONSTRUCTOR TBufTCPClientSocket.Create;
  BEGIN
    INHERITED Create;
    FBufPos:=0; FBufEnd:=0;
  END;

  PROCEDURE TBufTCPClientSocket.WaitForData;
  BEGIN
    IF FBufPos=FBufEnd THEN INHERITED WaitForData;
  END;

  FUNCTION TBufTCPClientSocket.WaitForDataTime(Timeout: Word): Integer;
  BEGIN
    IF FBufPos=FBufEnd THEN Result:=INHERITED WaitForDataTime(Timeout) ELSE Result:=1;
  END;

  FUNCTION TBufTCPClientSocket.Read(VAR Buf; Len: Word): Integer;
  BEGIN
    IF FBufPos=FBufEnd THEN
    BEGIN
      FBufEnd:=INHERITED Read(FReadBuf, BufSize);
      FBufPos:=0;
      IF FBufEnd<0 THEN
      BEGIN
        FBufEnd:=0;
        Result:=-1;
        Exit;
      END;
    END;

    Move(Buf, FReadBuf[FBufPos], Min(FBufEnd, Len));
    FBufPos:=Min(FBufEnd, Len);
    Result:=FBufEnd-FBufPos;
  END;

  FUNCTION TBufTCPClientSocket.ReadLn(VAR S: STRING): Integer;
  VAR
    c : Char;
    i : Integer;
  BEGIN
    S:=''; i:=FBufEnd;
    REPEAT
      IF FBufPos=FBufEnd THEN i:=Read(c, 0);       // Just fill buffer
      c:=FReadBuf[FBufPos];
      Inc(FBufPos);
      IF (c<>#10) AND (c<>#13) AND (FBufEnd>0) THEN S:=S+c;
    UNTIL (c=#10) OR (i<=0) OR (FBufEnd=0);
    IF i<0 THEN
      RAISE EBufTCPClientSocket.Create('ReadLn', IntToStr(SockErrNo));
    Result:=Length(S);
  END;


{ TTCPServerSocket }

  CONSTRUCTOR TTCPServerSocket.Create(PORT: STRING);
  VAR
    sn : SockAddr_In;
    PE : PProtoEnt;
  BEGIN
    INHERITED Create;
    DoLogLine(Format('Creating master socket on port %s',[Port]));
    PE:=SockGetProtoByName(Protocol);  // ERROR check
    Socket(PF_INET, SOCK_STREAM, PE^.p_proto);
    IF SockErrNo<>0 THEN
      RAISE ETCPServerSocket.Create('Create', IntToStr(SockErrNo));
    DoLogLine(Format('master socket: %d',[SocketHandle]));
    sn.sin_family := AF_INET;
    sn.sin_addr.s_addr := INADDR_ANY;
    sn.sin_port:=ResolvePort(Port);
    DoLogLine('Binding socket');
    (* Bind the socket to the port *)
    IF bind(SockAddr(sn),SizeOf(sn))<0 THEN
      RAISE ETCPServerSocket.Create('Create', IntToStr(SockErrNo));
  END;

  PROCEDURE TTCPServerSocket.Listen;
  VAR
    rc : Integer;
  BEGIN
    DoLogLine(Format('Putting socket on port %d in listen state',[PortN]));
    rc:=SockListen(SocketHandle, 1);
    IF rc<0 THEN
      RAISE ETCPServerSocket.Create('Listen', IntToStr(SockErrNo));
  END;

  PROCEDURE TTCPServerSocket.WaitForConnection;
  VAR
    Sock, rc : Integer;
  BEGIN
    Sock:=SocketHandle;
    rc:=Select(@Sock,1,0,0,-1);
    IF rc<0 THEN
      RAISE ETCPServerSocket.Create('WaitForConnect', IntToStr(SockErrNo));
  END;

  FUNCTION TTCPServerSocket.AcceptConnection;
  VAR
    TmpSock: int;
    sad    : SockAddr_In;
    phe    : PHostEnt;
    TmpCs  : TTCPClientSocket;
    l      : ULong;
  BEGIN
    Result := nil;
    DoLogLine('Accepting connection...');
    l:=SizeOf(Sad);
    TmpSock:=accept(sockaddr(sad),l);
    IF TmpSock=INVALID_SOCKET THEN
      RAISE ETCPServerSocket.Create('AcceptConnection', IntToStr(SockErrNo));

    FPeerIP:=SockInetntoa(sad.sin_addr);
    phe:=GetHostByAddr(sad.sin_addr, SizeOf(sad.sin_addr), AF_INET);
    IF NOT Assigned(phe) THEN
    BEGIN
      DoLogLine(Format('TTCPServerSocket.AcceptConnection: gethostbyaddr() failed; error code %d',[SockErrNo]));
      FPeerName := '';
    END else
      FPeerName := StrPas(phe^.h_name);
    TmpCs:=TTCPClientSocket.Create;
//!! Maybe check in when socket handle is set?
    TmpCs.SocketHandle:=TmpSock;
    TmpCs.Connected:=True;
    Result:=TmpCs;
  END;

END.

