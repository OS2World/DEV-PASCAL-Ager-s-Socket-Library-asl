{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.12 $       $Date: 2002/09/30 20:27:27 $    $Author: sag $ }
{                                                                             }
{ Platform independend socket routines                                        }
UNIT aslSocket;

{$I aslDefine.Inc}

INTERFACE

USES SysUtils,
{$IFDEF OS2}
     OS2Socket, sockin, netdb, Utils;
{$ELSE}
     Winsock;
{$ENDIF}


FUNCTION SockAccept(s: Integer; var name: sockaddr; var namelen: Integer): Integer; INLINE;
BEGIN
  Result:=accept(s, sockaddr(name), namelen);
END;

FUNCTION SockBind(s: Integer; var name: sockaddr; namelen: Integer): Integer; INLINE;
BEGIN
  Result:=bind(s, sockaddr(name), namelen);
END;

FUNCTION SockConnect(s: Integer; var name: sockaddr; namelen: Integer): Integer; INLINE;
BEGIN
  Result:=connect(s, sockaddr(name), namelen);
END;

{FUNCTION Sockgethostid: Integer; INLINE;
BEGIN
  Result:=gethostid;
END;}

FUNCTION SockGetPeerName(s: Integer; var name: sockaddr; namelen: Integer): Integer; INLINE;
BEGIN
  Result:=getpeername(s, sockaddr(name), namelen);
END;

FUNCTION SockGetSockName(s: Integer; var name: sockaddr; namelen: Integer): Integer; INLINE;
BEGIN
  Result:=getsockname(s, sockaddr(name), namelen);
END;

FUNCTION SockGetSockOpt(s: Integer; level: Integer; optname: Integer; var optval; var optlen: Integer): Integer; INLINE;
BEGIN
  Result:=getsockopt(s, level, optname, optval, optlen);
END;

FUNCTION SockIOCtl(s: Integer; cmd: Integer; var data; datalen: Integer): Integer; INLINE;
BEGIN
{$IFDEF OS2}
  Result:=OS2Socket.ioctl(s, cmd, data, datalen);
{$ELSE}
  Result:=ioctlsocket(s, cmd, u_long(data));
{$ENDIF}
END;

FUNCTION SockListen(s: Integer; backlog: Integer): Integer; INLINE;
BEGIN
  Result:=listen(s, backlog);
END;

{$IFDEF OS2}
FUNCTION SockRecvMsg(s: Integer; var msg: msghdr; flags: Integer): Integer; INLINE;
BEGIN
  Result:=recvmsg(s, msghdr(msg), flags);
END;
{$ENDIF}

FUNCTION SockRecv(s: Integer; var buf; len: Integer; flags: Integer): Integer; INLINE;
BEGIN
  Result:=recv(s, buf, len, flags);
END;

FUNCTION SockRecvFrom(s: Integer; var buf; len: Integer; flags: Integer; var name: sockaddr; var namelen: Integer): Integer; INLINE;
BEGIN
  Result:=recvfrom(s, buf, len, flags, sockaddr(name), namelen);
END;

FUNCTION SockSelect(s: pointer; noreads: Integer; nowrites: Integer; noexcepts: Integer; timeout: LongInt): Integer; INLINE;
{$IFDEF Win32}
VAR
  rfdset, wfdset, efdset: fd_set;
  tm   : timeval;
{$ENDIF}
BEGIN
{$IFDEF OS2}
  Result:=select(s, noreads, nowrites, noexcepts, timeout);
{$ELSE}
// This needs changing....
  rfdset.fd_count:=noreads;
  rfdset.fd_array[1]:=LongInt(s^);
  wfdset.fd_count:=nowrites;
  wfdset.fd_array[1]:=LongInt(s^);
  efdset.fd_count:=noexcepts;
  efdset.fd_array[1]:=LongInt(s^);
  tm.tv_sec:=0;
  tm.tv_usec:=Timeout;
  Result:=select(0, @rfdset, @wfdset, @efdset, @tm);
{$ENDIF}
END;

FUNCTION SockSend(s: Integer; var buf; len: Integer; flags: Integer): Integer; INLINE;
BEGIN
  Result:=send(s, buf, len, flags);
END;

{$IFDEF OS2}
FUNCTION SockSendMsg(s: Integer; var msg: msghdr; flags: Integer): Integer; INLINE;
BEGIN
  Result:=sendmsg(s, msghdr(msg), flags);
END;
{$ENDIF}

FUNCTION SockSendTo(s: Integer; var buf; len: Integer; flags: Integer; var name: sockaddr; namelen: Integer): Integer; INLINE;
BEGIN
  Result:=sendto(s, buf, len, flags, sockaddr(name), namelen);
END;

FUNCTION SockSetSockOpt(s: Integer; level: Integer; optname: Integer; var optval; optlen: Integer): Integer; INLINE;
BEGIN
  Result:=setsockopt(s, level, optname, optval, optlen);
END;

{FUNCTION SockInit: Integer; INLINE;
BEGIN
  Result:=sock_init;
END;}

FUNCTION SockErrNo: Integer; INLINE;
BEGIN
{$IFDEF OS2}
  Result:=sock_errno;
{$ELSE}
  Result:=WSAGetLastError;
{$ENDIF}
END;

FUNCTION SockSocket(domain: Integer; stype: Integer; protocol: Integer): Integer; INLINE;
BEGIN
  Result:=socket(domain, stype, protocol);
END;

FUNCTION SockClose(s: Integer): Integer; INLINE;
BEGIN
{$IFDEF OS2}
  Result:=soclose(s);
{$ELSE}
  Result:=closesocket(s);
{$ENDIF}
END;

FUNCTION SockAbort(s: Integer): Integer; INLINE;
BEGIN
{$IFDEF OS2}
  Result:=soabort(s);
{$ELSE}
  Result:=-1;
{$ENDIF}
END;

FUNCTION SockCancel(s: Integer): Integer; INLINE;
BEGIN
{$IFDEF OS2}
  Result:=so_cancel(s);
{$ELSE}
  Result:=-1;
{$ENDIF}
END;

{$IFDEF OS2}
FUNCTION SockReadV(s: Integer; var iov; iovcnt: Integer): Integer; INLINE;
BEGIN
  Result:=readv(s, iov, iovcnt);
END;

FUNCTION SockWriteV(s: Integer; var iov; iovcnt: Integer): Integer; INLINE;
BEGIN
  Result:=writev(s, iov, iovcnt);
END;
{$ENDIF}

FUNCTION SockShutdown(s: Integer; howto: Integer): Integer; INLINE;
BEGIN
  Result:=shutdown(s, howto);
END;

{FUNCTION Sockgetinetversion(ver: pchar): Integer; INLINE;
BEGIN
  Result:=getinetversion(ver: pchar);
END;}


FUNCTION bswap(u: SmallWord): SmallWord; INLINE;
BEGIN
{$IFDEF OS2}
  Result:=utils.bswap(u);
{$ELSE}
  Result:=ntohs(u)
{$ENDIF}
END;

PROCEDURE Sockpsock_errno(error: STRING);

FUNCTION SockGetServByName(Name: STRING; Proto: STRING): PServEnt;
FUNCTION SockGetHostByName(Name: STRING): PHostEnt;
FUNCTION SockGetProtoByName(Name: STRING): PProtoEnt;
FUNCTION SockInetAddr(Adr: STRING): LongInt;
FUNCTION SockInetNtoA(ina: in_addr): STRING;

FUNCTION SockClientIP: STRING;

IMPLEMENTATION

  PROCEDURE Sockpsock_errno(error: STRING);
  BEGIN
{$IFDEF OS2}
      Error:=Error+#0;
      psock_errno(@error[1]);
{$ELSE}
    WriteLn(error);
{$ENDIF}
  END;

  FUNCTION SockGetServByName(Name: STRING; Proto: STRING): PServEnt;
  BEGIN
    Name:=Name+#0; Proto:=Proto+#0;
    SockGetServByName:=PServEnt(GetServByName(@Name[1], @Proto[1]));
  END;

  FUNCTION SockGetHostByName(Name: STRING): PHostEnt;
  BEGIN
    Name:=Name+#0;
    SockGetHostByName:=PHostEnt(GetHostByName(@Name[1]));
  END;

  FUNCTION SockGetProtoByName(Name: STRING): PProtoEnt;
  BEGIN
    Name:=Name+#0;
    SockGetProtoByName:=PProtoEnt(GetProtoByName(@Name[1]));
  END;

  FUNCTION SockInetAddr(Adr: STRING): LongInt;
  BEGIN
    Adr:=Adr+#0;
    SockInetAddr:=inet_Addr(@Adr[1]);
  END;

  FUNCTION SockInetNtoA(ina: in_addr): STRING;
  VAR
    p: PChar;
  BEGIN
    p:=inet_ntoa(ina);
    SockINetNtoA:=StrPas(p);
  END;

  FUNCTION SockClientIP: STRING;
  VAR
    Name : ARRAY[0..40] OF Char;
    phe  : phostent;
    p    : PChar;
  BEGIN
    GetHostName(Name, SizeOf(Name));
    phe:=GetHostByName(Name);
    SockClientIP:=SockInetNtoA(phe^.h_addr^^);
  END;



{$IFDEF Win32}
VAR
  WSAD: WSAData;
{$ENDIF}

INITIALIZATION
{$IFDEF OS2}
  sock_init;
{$ELSE}
  IF WSAStartUp($0101, WSAD)<>0 THEN
;//    RAISE ESocketError.Create('WSAStartUp() failed');

FINALIZATION
  IF WSACleanup=SOCKET_ERROR THEN
;//    RAISE ESocketError.Create('WSACleanup() failed');
{$ENDIF}
END.

