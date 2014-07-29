{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.2 $    $Date: 2002/04/22 23:36:54 $    $Author: sag $ }
{                                                                             }
{ Concurrent Version System client class (CVSClient.pdf)                      }
UNIT aslCVSClient;

{$I aslDefine.Inc}

INTERFACE

USES VPUtils, Classes, SysUtils, aslAbsSocket, aslAbsClient;

TYPE
  EAbsCVSClient = CLASS(EaslException);

  TAbsCVSClient = CLASS(TAbsClient)
  PROTECTED
    FUNCTION  CryptPassword(Password: STRING): STRING;
    PROCEDURE GetResponse(Extra: TStringList); OVERRIDE;
  PUBLIC
    CONSTRUCTOR Create;
    PROCEDURE Connect(Host: STRING); OVERRIDE;

{ Standard commands }
    PROCEDURE PServerAuth(CVSRoot, User, Password: String);
    PROCEDURE Root(Path: STRING);
    PROCEDURE ValidResponses(Responses: STRING);
    PROCEDURE ValidRequests;
    PROCEDURE Directory(LocalDir, Repository: STRING);
    PROCEDURE MaxDotDot(Level: Integer);
    PROCEDURE StaticDirectory;
    PROCEDURE Sticky(TagSpec: STRING);
    PROCEDURE CheckInProg(Prog: STRING);
    PROCEDURE UpdateProg(Prog: STRING);
    PROCEDURE Entry(EntryLine: STRING);
    PROCEDURE Kopt(Option: STRING);
    PROCEDURE Modified(Filename: STRING);
    PROCEDURE IsModified(Filename: STRING);
    PROCEDURE Unchanged(Filename: STRING);
    PROCEDURE UseUnchanged;
    PROCEDURE Notify(Filename: STRING);
    PROCEDURE Questionable(Filename: STRING);
    PROCEDURE Case_;
    PROCEDURE Argument(Arg: STRING);
    PROCEDURE ArgumentX(Arg: STRING);
    PROCEDURE GlobalOption(Option: STRING);
    PROCEDURE GZipStream(Level: Integer);
    PROCEDURE KerberosEncrypt;
    PROCEDURE GssapiEncrypt;
    PROCEDURE GssapiAuthentication;
    PROCEDURE Set_(Variable, Value: STRING);
    PROCEDURE ExpandModules(ExpModules: TStringList);
    PROCEDURE ci(ServerResponse: TStringList);
    PROCEDURE diff(ServerResponse: TStringList);
    PROCEDURE tag(ServerResponse: TStringList);
    PROCEDURE status(ServerResponse: TStringList);
    PROCEDURE log(ServerResponse: TStringList);
    PROCEDURE admin(ServerResponse: TStringList);
    PROCEDURE history(ServerResponse: TStringList);
    PROCEDURE watchers(ServerResponse: TStringList);
    PROCEDURE editors(ServerResponse: TStringList);
    PROCEDURE annotate(ServerResponse: TStringList);
    PROCEDURE co(ServerResponse: TStringList);
    PROCEDURE export_(ServerResponse: TStringList);
    PROCEDURE rdiff(ServerResponse: TStringList);
    PROCEDURE rtag(ServerResponse: TStringList);
    PROCEDURE init(RootName: STRING; ServerResponse: TStringList);
    PROCEDURE update(ServerResponse: TStringList);
    PROCEDURE import(ServerResponse: TStringList);
    PROCEDURE add(ServerResponse: TStringList);
    PROCEDURE remove(ServerResponse: TStringList);
    PROCEDURE watchOn(ServerResponse: TStringList);
    PROCEDURE watchOff(ServerResponse: TStringList);
    PROCEDURE watchAdd(ServerResponse: TStringList);
    PROCEDURE watchRemove(ServerResponse: TStringList);
    PROCEDURE release(ServerResponse: TStringList);
    PROCEDURE noop(ServerResponse: TStringList);
    PROCEDURE updatePatches(ServerResponse: TStringList);
    PROCEDURE gzipFileContents(Level: Integer);
    PROCEDURE wrapperSendmercsOptions(ServerResponse: TStringList);

  END;

  TCVSClient = CLASS(TAbsCVSClient)
  PRIVATE
  PUBLIC
  END;

IMPLEMENTATION

{ TAbsCVSClient }

  CONSTRUCTOR TAbsCVSClient.Create;
  BEGIN
    INHERITED Create;
    Service:='cvspserver';
    Socket.LineSep:=#10;
    GetResponseAfterConnect:=False;
  END;

  PROCEDURE TAbsCVSClient.Connect(Host: STRING);
  BEGIN
    INHERITED Connect(Host);
  END;

  PROCEDURE TAbsCVSClient.GetResponse(Extra: TStringList);
  VAR
    Ok : Integer;
    S  : String;
  BEGIN
    FSocket.ReadLn(FLastResponse);
    DoLogLine('>'+FLastResponse);
    IF Extra<>nil THEN
    BEGIN
      Extra.Add(FLastResponse);   //!!! ??? Good idea??
      REPEAT
        FSocket.ReadLn(S);
        IF Extra<>nil THEN Extra.Add(S);
        DoLogLine('>'+S);
      UNTIL (S='ok') OR (S='error');
    END;
  END;

  PROCEDURE TAbsCVSClient.Root(Path: STRING);
  BEGIN
    SendCommandNoResponse('Root '+Path);
  END;

  PROCEDURE TAbsCVSClient.ValidResponses(Responses: STRING);
  BEGIN
    SendCommandNoResponse('Valid-responses '+Responses);
  END;

  PROCEDURE TAbsCVSClient.ValidRequests;
  BEGIN
    SendCommand('valid-requests');
  END;

  PROCEDURE TAbsCVSClient.Directory(LocalDir, Repository: STRING);
  BEGIN
    SendCommandNoResponse('Directory '+LocalDir);
    SendCommandNoResponse(Repository);
  END;

  PROCEDURE TAbsCVSClient.MaxDotDot(Level: Integer);
  BEGIN
    SendCommandNoResponse('Max-dotdot '+Int2Str(Level));
  END;

  PROCEDURE TAbsCVSClient.StaticDirectory;
  BEGIN
    SendCommandNoResponse('Static-directory');
  END;

  PROCEDURE TAbsCVSClient.Sticky(TagSpec: STRING);
  BEGIN
    SendCommandNoResponse('Sticky '+TagSpec);
  END;

  PROCEDURE TAbsCVSClient.CheckInProg(Prog: STRING);
  BEGIN
    SendCommandNoResponse('Checkin-prog '+Prog);
  END;

  PROCEDURE TAbsCVSClient.UpdateProg(Prog: STRING);
  BEGIN
    SendCommandNoResponse('Update-prog '+Prog);
  END;

  PROCEDURE TAbsCVSClient.Entry(EntryLine: STRING);
  BEGIN
    SendCommandNoResponse('Entry '+EntryLine);
  END;

  PROCEDURE TAbsCVSClient.Kopt(Option: STRING);
  BEGIN
    SendCommandNoResponse('Kopt '+Option);
  END;

  PROCEDURE TAbsCVSClient.Modified(Filename: STRING);
  BEGIN
//!!! More parameters....
    SendCommandNoResponse('Modified '+Filename);
  END;

  PROCEDURE TAbsCVSClient.IsModified(Filename: STRING);
  BEGIN
    SendCommandNoResponse('Is-modified '+Filename);
  END;

  PROCEDURE TAbsCVSClient.Unchanged(Filename: STRING);
  BEGIN
    SendCommandNoResponse('Unchanged '+Filename);
  END;

  PROCEDURE TAbsCVSClient.UseUnchanged;
  BEGIN
    SendCommandNoResponse('UseUnchanged');
  END;

  PROCEDURE TAbsCVSClient.Notify(Filename: STRING);
  BEGIN
//!! More parameters...
    SendCommandNoResponse('Notify '+Filename);
  END;

  PROCEDURE TAbsCVSClient.Questionable(Filename: STRING);
  BEGIN
    SendCommandNoResponse('Questionable '+Filename);
  END;

  PROCEDURE TAbsCVSClient.Case_;
  BEGIN
    SendCommandNoResponse('Case');
  END;

  PROCEDURE TAbsCVSClient.Argument(Arg: STRING);
  BEGIN
    SendCommandNoResponse('Argument '+Arg);
  END;

  PROCEDURE TAbsCVSClient.ArgumentX(Arg: STRING);
  BEGIN
    SendCommandNoResponse('Argumentx '+Arg);
  END;

  PROCEDURE TAbsCVSClient.GlobalOption(Option: STRING);
  BEGIN
    SendCommandNoResponse('Global_option '+Option);
  END;

  PROCEDURE TAbsCVSClient.GZipStream(Level: Integer);
  BEGIN
    SendCommandNoResponse('Gzip-straem '+Int2Str(Level));
  END;

  PROCEDURE TAbsCVSClient.KerberosEncrypt;
  BEGIN
    SendCommandNoResponse('Kerberos-encrypt');
  END;

  PROCEDURE TAbsCVSClient.GssapiEncrypt;
  BEGIN
    SendCommandNoResponse('Gssapi-encrypt');
  END;

  PROCEDURE TAbsCVSClient.GssapiAuthentication;
  BEGIN
    SendCommandNoResponse('Gssapi-authentication');
  END;

  PROCEDURE TAbsCVSClient.Set_(Variable, Value: STRING);
  BEGIN
    SendCommandNoResponse('Set '+Variable+'='+Value);
  END;

  PROCEDURE TAbsCVSClient.ExpandModules(ExpModules: TStringList);
  BEGIN
    SendCommandEx('expand-modules',ExpModules);
  END;

  PROCEDURE TAbsCVSClient.ci(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('ci',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.diff(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('diff',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.tag(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('tag',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.status(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('status',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.log(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('log',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.admin(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('admin',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.history(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('history',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.watchers(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('watchers',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.editors(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('editors',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.annotate(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('annotate',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.co(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('co',ServerResponse);
//!! Link to a file writer......
  END;

  PROCEDURE TAbsCVSClient.export_(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('export',ServerResponse);
//!! Link to a file writer......
  END;

  PROCEDURE TAbsCVSClient.rdiff(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('rdiff',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.rtag(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('rtag',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.init(RootName: STRING; ServerResponse: TStringList);
  BEGIN
    SendCommandEx('init '+RootName,ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.update(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('update',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.import(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('import',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.add(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('add',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.remove(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('remove',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.watchOn(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('watch-on',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.watchOff(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('watch-off',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.watchAdd(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('watch-add',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.watchRemove(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('watch-remove',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.release(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('release',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.noop(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('noop',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.updatePatches(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('update-patches',ServerResponse);
  END;

  PROCEDURE TAbsCVSClient.gzipFileContents(Level: Integer);
  BEGIN
    SendCommand('gzip-file-contents '+Int2Str(Level));
  END;

  PROCEDURE TAbsCVSClient.wrapperSendmercsOptions(ServerResponse: TStringList);
  BEGIN
    SendCommandEx('wrapper-sendme-rcsOptions',ServerResponse);
  END;





  PROCEDURE TAbsCVSClient.PServerAuth(CVSRoot, User, Password: STRING);
  BEGIN
    SendCommandNoResponse('BEGIN AUTH REQUEST');
    SendCommandNoResponse(CVSRoot);
    SendCommandNoResponse(User);
    SendCommandNoResponse('A'+CryptPassword(Password));
    SendCommand('END AUTH REQUEST');
//!!! Collect whole error response?
//    GetResponse(Nil);
    IF LastResponse<>'I LOVE YOU' THEN
      RAISE EAbsCVSClient.Create('PServerAuth', LastResponse);
  END;



  FUNCTION TAbsCVSClient.CryptPassword(Password: STRING): STRING;
  CONST
    Chars = '!"%&''()*+,-./0123456789:;<=>?ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz';
    Crypt : ARRAY[1..82] OF Byte = (120,53,109,72,108,70,64,76,67,116,74,68,87,
                                    111,52,75,119,49,34,82,81,95,65,112,86,118,110,122,105,
                                    57,83,43,46,102,40,89,38,103,45,50,42,123,91,35,
                                    125,55,54,66,124,126,59,47,92,71,115,56,
                                    121,117,104,101,100,59,73,99,63,94,93,39,37,61,48,
                                    58,113,32,90,44,98,60,51,33,97,62);
  VAR
    i : Integer;
  BEGIN
    Result:='';
    FOR i:=1 TO Length(Password) DO
      IF (Pos(Copy(Password,i,1),Chars)>0) THEN
        Result:=Result+Char(Crypt[Pos(Copy(Password,i,1),Chars)]);
  END;

END.

