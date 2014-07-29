{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.3 $    $Date: 1999/05/26 22:04:56 $    $Author: sag $ }
{                                                                             }
{ aslDecode (c) Copyright 1999 by Dwayne Heaton (dwayne@vmadd.demon.co.uk)    }
{                                                                             }
UNIT aslDecode;

{$I aslDefine.inc}

INTERFACE

USES Classes;

TYPE
  TTriplet = ARRAY[0..2] OF Byte;
  TKwartet = ARRAY[0..3] OF Byte;

  TAbsDecode = CLASS
  PRIVATE
    Triplets   : ARRAY[1..19] OF TTriplet;
    FileStream : TFileStream;
    Table      : ARRAY[0..64] OF Char;
    FAttachDir : STRING;
    FFilename  : STRING;
    Started    : Boolean;
    Finished   : Boolean;

    PROCEDURE Kwartet2Triplet(Kwartet: TKwartet; var Triplet: TTriplet);
    PROCEDURE DecodeLine(srcLine: STRING); VIRTUAL; ABSTRACT;

  PUBLIC
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;

    FUNCTION DecodeEof : Boolean;
    PROCEDURE Decode(s : STRING); VIRTUAL; ABSTRACT;

    PROPERTY Filename : STRING READ FFilename;
    PROPERTY AttachDir : STRING READ FAttachDir WRITE FAttachDir;
  END;

  T64Decode = CLASS(TAbsDecode)
  PRIVATE
    PROCEDURE DecodeLine(srcLine: STRING); OVERRIDE;

  PUBLIC
    CONSTRUCTOR Create(Filename : STRING);

    PROCEDURE Decode(s : String); OVERRIDE;
  END;

  TUUDecode = CLASS(TAbsDecode)
  PRIVATE
    PROCEDURE DecodeLine(srcLine: STRING); OVERRIDE;

  PUBLIC
    CONSTRUCTOR Create(Filename : STRING);

    PROCEDURE Decode(s : String); OVERRIDE;
  END;

  TXXDecode = CLASS(TUUDecode)
  PUBLIC
    CONSTRUCTOR Create(Filename : STRING);
  END;

Implementation

USES SysUtils, aslUtils;

{ TAbsDecode }

  PROCEDURE TAbsDecode.Kwartet2Triplet(Kwartet: TKwartet; VAR Triplet: TTriplet);
  BEGIN
    Triplet[0] :=  ((Kwartet[0]) SHL 2) + (((Kwartet[1]) AND $30) SHR 4);
    Triplet[1] := (((Kwartet[1]) AND $0F) SHL 4) + (((Kwartet[2]) AND $3C) SHR 2);
    Triplet[2] := (((Kwartet[2]) AND $03) SHL 6) + ((Kwartet[3]) AND $3F);
  END;

  CONSTRUCTOR TAbsDecode.Create;
  BEGIN
    INHERITED Create;
    FAttachDir := '';
    FFilename := '';
    Started := False;
    Finished := False;
  END;

  DESTRUCTOR TAbsDecode.Destroy;
  BEGIN
    If Assigned(FileStream) THEN FileStream.Destroy;
    FileStream := NIL;
    FFilename := '';
    FAttachDir := '';
    INHERITED Destroy;
  END;

  FUNCTION TAbsDecode.DecodeEof : Boolean;
  BEGIN
    Result := Finished;
  END;

{ T64Decode }

  CONSTRUCTOR T64Decode.Create(Filename : STRING);
  BEGIN
    INHERITED Create;
    FFilename := Filename;
    FileStream := TFileStream.Create(FFilename, fmCreate);
    Table := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
  END;

  PROCEDURE T64Decode.DecodeLine(srcLine: STRING);
  VAR
    BytesWritten : LongInt;
    Total        : LongInt;
    Cnt          : LongInt;
    kwart        : TKwartet;
    Trip         : Byte;
    i,j          : LongInt;

  BEGIN
    FillChar(Triplets, SizeOf(Triplets), 0);
    Trip := 0;
    Total := 0;
    WHILE srcLine <> '' DO
    BEGIN
      FillChar(kwart, SizeOf(kwart), 0);
      Cnt := 3;
      FOR j:=0 TO 3 DO
      BEGIN
        IF srcLine[j+1] = '=' THEN
        BEGIN
          Dec(Cnt, 1);
          srcLine[j+1] := Table[0];
        END;
        kwart[j] := Pos(srcLine[j+1], Table)-1;
      END;
      Delete(srcLine, 1, 4);
      Inc(Trip, 1);
      Kwartet2Triplet(kwart, Triplets[Trip]);
      Inc(Total, Cnt);
    END;
    BytesWritten := FileStream.Write(Triplets, Total);
  END;

  PROCEDURE T64Decode.Decode(s : STRING);
  BEGIN
    IF Finished THEN Exit;
    IF (NOT Started) AND (s <> '') THEN Started := True;
    IF NOT Started THEN Exit;
    IF s = '' THEN Finished := True;
    IF (Started) AND (NOT Finished) THEN DecodeLine(s);
    IF Pos('=', s) <> 0 THEN Finished := True;
  END;

{ TUUDecode }

  CONSTRUCTOR TUUDecode.Create(Filename : STRING);
  BEGIN
    INHERITED Create;
    Table := '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ ';
  END;

  PROCEDURE TUUDecode.DecodeLine(srcLine: STRING);
  VAR
    BytesWritten : LongInt;
    NumRead      : Byte;
    kwart        : TKwartet;
    Trip         : Byte;
    i,j          : LongInt;

  BEGIN
    NumRead := Pos(srcLine[1], Table)-1;
    Delete(srcLine, 1, 1);
    Trip := 0;
    BytesWritten := 0;
    FillChar(Triplets, SizeOf(Triplets), 0);
    FOR i := 1 TO NumRead DIV 3 DO
    BEGIN
      FillChar(kwart, SizeOf(kwart), 0);
      FOR j:=0 TO 3 DO kwart[j] := Pos(srcLine[j+1], Table)-1;
      Delete(srcLine, 1, 4);
      Inc(Trip, 1);
      Kwartet2Triplet(kwart, Triplets[Trip]);
    END;
    IF (NumRead MOD 3) > 0 THEN
    BEGIN
      FOR j:=1 TO 3 DO srcLine := srcLine + Table[0];
      FOR j:=0 TO 3 DO kwart[j] := Pos(srcLine[j+1], Table)-1;
      Kwartet2Triplet(kwart, Triplets[Trip+1]);
    END;
    BytesWritten := FileStream.Write(Triplets, (Trip*3)+(NumRead MOD 3));
  END;

  PROCEDURE TUUDecode.Decode(s : STRING);
  BEGIN
    IF Copy(s, 1, 6) = 'begin ' THEN
    BEGIN
      Started := True;
      FFilename := ExtractFilename(ExtractWords(3, 1, s));
      FileStream := TFileStream.Create(FAttachDir + FFilename, fmCreate);
      Exit;
    END;
    IF Not Started THEN Exit;
    IF Finished THEN Exit;

    IF s = 'end' THEN Finished := True;

    IF (Started) AND (NOT Finished) THEN DecodeLine(s);
  END;

{ TXXDecode }

  CONSTRUCTOR TXXDecode.Create(Filename : STRING);
  BEGIN
    INHERITED Create(Filename);
    Table := '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz ';
  END;

END.
