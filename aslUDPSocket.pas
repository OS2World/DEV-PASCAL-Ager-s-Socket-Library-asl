{ Ager's Socket Library (c) Copyright 1998-01 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.9 $    $Date: 2001/02/03 19:36:07 $    $Author: sag $ }
{                                                                             }
{ OO interface to UDP sockets                                                 }
UNIT aslUDPSocket;

{$I aslDefine.Inc}

INTERFACE

USES SysUtils, VPUtils, CTypes, aslSocket, aslAbsSocket,
{$IFDEF OS2}
     OS2Socket, NetDB, SockIn, Utils;
{$ELSE}
     Winsock;
{$ENDIF}

TYPE
  EUDPSocket = CLASS(EaslException);

  TUDPSocket = CLASS(TAbsSocket)
  PRIVATE
  PUBLIC
    CONSTRUCTOR Create(port:string);
    FUNCTION SendBroadcast(VAR buf;len:integer;port:string):integer;
    FUNCTION SendToAddr(VAR buf;len:integer;adr,port:string):integer;
    FUNCTION ReceiveFrom(VAR buf;len:integer;VAR rip,rport,rname:string):integer;
  END;


IMPLEMENTATION

{ TUDPSocket }

  CONSTRUCTOR TUDPSocket.Create;
  VAR
    sin: SockAddr_In;
    PE : PProtoEnt;
  BEGIN
    INHERITED Create;
    Protocol:='udp';

    DoLogLine('Creating udp socket');

    PE:=SockGetProtoByName(Protocol);  // !!! Error check
    Socket(PF_INET, SOCK_DGRAM, PE.p_proto);
    IF SockErrNo<>0 THEN
      RAISE EUDPSocket.Create('Create: socket()', IntToStr(SockErrNo));
    sin.sin_family:=AF_INET;
    sin.sin_addr.s_addr:=INADDR_ANY;
    sin.sin_port:=ResolvePort(Port);
    bind(SockAddr(sin), SizeOf(sin));            (* Bind the socket to the port *)
    IF SockErrNo<>0 THEN
      RAISE EUDPSocket.Create('Create: bind()', IntToStr(SockErrNo));
  END;

  FUNCTION TUDPSocket.SendBroadcast(VAR buf; len: integer; port: string): integer;
  VAR
    i,si: Integer;
    sin : SockAddr_In;
  BEGIN
    DoLogLine(Format('Sending broadcast on UDP socket %d...',[SocketHandle]));
    si:=SizeOf(integer);
    IF getsockopt(SOL_SOCKET,SO_BROADCAST,i,si)<>0 THEN   //=SOCKET_ERROR THEN
      RAISE EUDPSocket.Create('SendBroadcast: getsockopt()', IntToStr(SockErrNo));
    IF i=0 THEN
    BEGIN
      i:=1;
      IF setsockopt(SOL_SOCKET,SO_BROADCAST,i,si)<>0 THEN   //=SOCKET_ERROR THEN
        RAISE EUDPSocket.Create('SendBroadcast: setsockopt()', IntToStr(SockErrNo));
    END;
    sin.sin_family:=AF_INET;
    sin.sin_addr.s_addr:=INADDR_BROADCAST;
    sin.sin_port:=ResolvePort(Port);      //htons(GetPortByName(port,FProtocol));
    Result:=sendto(buf, len, 0, sockaddr(sin), SizeOf(sin));
    IF Result<0 THEN
      RAISE EUDPSocket.Create('SendBroadCast: sendto()', IntToStr(SockErrNo));
  END;

  FUNCTION TUDPSocket.SendToAddr(VAR buf; len: integer; adr,port: string): integer;
  VAR
    sin:SockAddr_In;
    phe:PHostEnt;
  BEGIN
    sin.sin_addr.s_addr:=SockInetAddr(Adr);
    IF sin.sin_addr.s_addr = INADDR_NONE THEN
    BEGIN
      DoLogLine(Format('%s does not look like ip. Trying to use it as host name ',[adr]));
      phe := Sockgethostbyname(adr);
      IF phe = nil THEN
        RAISE EUDPSocket.Create('SendToAdr:  Host name '+adr+' unknown', IntToStr(SockErrNo));
      DoLogLine('resolved host name ');
      Sin.sin_addr:=phe^.h_addr^^;
    END;
    DoLogLine(Format('Sending broadcast on UDP socket %s:%s on %d...',[adr,port,SocketHandle]));
    Broadcast:=True;
    sin.sin_family:=AF_INET;
    sin.sin_addr.s_addr:=INADDR_BROADCAST;
    sin.sin_port:=ResolvePort(port);
    Result:=sendto(buf,len,0,SockAddr(sin),SizeOf(sin));
    IF Result<0 THEN      //=SOCKET_ERROR THEN
      RAISE EUDPSocket.Create('SendToAdr', IntToStr(SockErrNo));
  END;

  FUNCTION TUDPSocket.ReceiveFrom(VAR buf; len: integer; VAR rip, rport, rname: string): integer;
  VAR
    sin:SockAddr_IN;
    ssin:integer;
    phe:PHostEnt;
  BEGIN
    ssin:=SizeOf(SockAddr_In);
    DoLogLine(Format('Receiving data on socket %d...',[SocketHandle]));
    Result:=recvfrom(buf,len,0,sockaddr(sin),ssin);
    IF Result<>0 THEN    //=SOCKET_ERROR THEN
      RAISE EUDPSocket.Create('ReceiveFrom', IntToStr(SockErrNo));
    rip:=SockInetntoa(sin.sin_addr);
    phe:=GetHostByAddr(sin.sin_addr, SizeOf(sin.sin_addr), AF_INET);
    IF NOT Assigned(phe) THEN
    BEGIN
      DoLogLine('TUDPSocket.ReceiveFrom gethostbyaddr() failed');
      rname := '';
    END else
      rname := StrPas(phe^.h_name);
    rport:=IntToStr(sin.sin_port);  // text name
    DoLogLine(Format('After proc SocketHandle is %d',[SocketHandle]));
  END;

END.

