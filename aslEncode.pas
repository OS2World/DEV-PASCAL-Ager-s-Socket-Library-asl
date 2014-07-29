{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.2 $    $Date: 1999/05/26 22:04:56 $    $Author: sag $ }
{                                                                             }
{ aslEncode (c) Copyright 1999 by Dwayne Heaton (dwayne@vmadd.demon.co.uk)    }
{                                                                             }
UNIT aslEncode;

{$I aslDefine.inc}

INTERFACE

USES Classes;

TYPE
  TTriplet = ARRAY[0..2] of Byte;
  TKwartet = ARRAY[0..3] of Byte;

  TAbsEncode = CLASS
  PRIVATE
    Triplets   : ARRAY[1..18] OF TTriplet;
    FBlockSize : Byte;
    FileStream : TFileStream;
    Table      : ARRAY[0..64] OF Char;

    PROCEDURE Triplet2Kwartet(Triplet : TTriplet; VAR Kwartet : TKwartet);
    PROCEDURE Encodeline(VAR dstLine : STRING; VAR BytesRead : LongInt);
    FUNCTION EncodeProgress : Integer;

  PUBLIC
    CONSTRUCTOR Create(Filename : STRING);
    DESTRUCTOR Destroy; OVERRIDE;

    FUNCTION EncodeEof : Boolean; VIRTUAL; ABSTRACT;
    FUNCTION Encode : STRING; VIRTUAL; ABSTRACT;

    PROPERTY Progress : Integer READ EncodeProgress;
  END;

  T64Encode = CLASS(TAbsEncode)
  PUBLIC
    CONSTRUCTOR Create(Filename : STRING);

    FUNCTION EncodeEof : Boolean; OVERRIDE;
    FUNCTION Encode : STRING; OVERRIDE;
  END;

  TUUEncode = CLASS(TAbsEncode)
  PRIVATE
    FirstLine : Boolean;
    DoLast    : Boolean;
    DoneLast  : Boolean;
    FFilename : STRING;

  PUBLIC
    CONSTRUCTOR Create(Filename : STRING);

    FUNCTION EncodeEof : Boolean; OVERRIDE;
    FUNCTION Encode : STRING; OVERRIDE;
  END;

  TXXEncode = CLASS(TUUEncode)
  PUBLIC
    CONSTRUCTOR Create(Filename : STRING);
  END;

  FUNCTION MakeUniqueID : STRING;

IMPLEMENTATION

USES SysUtils;

{ TAbsEncode }

  PROCEDURE TAbsEncode.Triplet2Kwartet(Triplet: TTriplet; VAR Kwartet: TKwartet);
  BEGIN
    Kwartet[0] := (Triplet[0] SHR 2);
    Kwartet[1] := ((Triplet[0] SHL 4) AND $30) + ((Triplet[1] SHR 4) AND $0F);
    Kwartet[2] := ((Triplet[1] SHL 2) AND $3C) + ((Triplet[2] SHR 6) AND $03);
    Kwartet[3] := (Triplet[2] AND $3F);
  END;

  PROCEDURE TAbsEncode.Encodeline(VAR dstLine : STRING; VAR BytesRead : LongInt);
  VAR
    tCnt : Integer;
    kwar : TKwartet;

  BEGIN
    FillChar(Triplets, SizeOf(Triplets), #0);
    dstLine := '';
    BytesRead := FileStream.Read(Triplets, FBlockSize);
    FOR tCnt := 1 TO (BytesRead+2) DIV 3 DO
    BEGIN
      Triplet2Kwartet(Triplets[tCnt], kwar);
      dstLine:=dstLine+Table[kwar[0]]+Table[kwar[1]]+Table[kwar[2]]+Table[kwar[3]];
    END;
  END;

  FUNCTION TAbsEncode.EncodeProgress : Integer;
  BEGIN
    Result := Round(100*FileStream.Position/FileStream.Size);
  END;

  CONSTRUCTOR TAbsEncode.Create(Filename : STRING);
  BEGIN
    INHERITED Create;
    FileStream := TFileStream.Create(FileName, fmOpenRead+fmShareDenyNone);
  END;

  DESTRUCTOR TAbsEncode.Destroy;
  BEGIN
    FileStream.Destroy;
    FileStream:=Nil;
    INHERITED Destroy;
  END;

{ T64Encode [Base64 Encoding (Mime)] }

  CONSTRUCTOR T64Encode.Create(Filename : STRING);
  BEGIN
    INHERITED Create(Filename);
    FBlockSize := 18 * SizeOf(TTriplet);
    Table := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
  END;

  FUNCTION T64Encode.EncodeEof : Boolean;
  BEGIN
    Result := FileStream.Position >= FileStream.Size;
  END;

  FUNCTION T64Encode.Encode : STRING;
  VAR
    BytesRead : LongInt;
    dstLine   : STRING;
    tCnt      : Integer;

  BEGIN
    Encodeline(dstLine, BytesRead);
    IF BytesRead <> FBlockSize THEN
    BEGIN
      tCnt := BytesRead MOD 3;
      IF tCnt IN [1, 2] THEN dstLine[Length(dstLine)] := '=';
      IF tCnt IN [1] THEN dstLine[Length(dstLine)-1] := '=';
    END;
    Result := dstLine;
  END;

{ TUUEncode [UU Encoding] }

  CONSTRUCTOR TUUEncode.Create(Filename : STRING);
  BEGIN
    INHERITED Create(Filename);
    FBlockSize := 15 * SizeOf(TTriplet);
    Table := '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ ';
    FirstLine := True;
    DoLast := False;
    DoneLast := False;
    FFilename := Filename;
  END;

  FUNCTION TUUEncode.EncodeEof : Boolean;
  BEGIN
    Result := DoneLast;
  END;

  FUNCTION TUUEncode.Encode : STRING;
  VAR
    BytesRead : LongInt;
    dstStr    : STRING;

  BEGIN
    IF NOT FirstLine THEN
    BEGIN
      IF NOT DoLast THEN
      BEGIN
        Encodeline(dstStr, BytesRead);
        Result := Table[BytesRead] + dstStr;
        IF BytesRead = 0 THEN DoLast := True;
      END ELSE
      BEGIN
        Result := 'end';
        DoneLast := True;
      END;
    END ELSE
      Result := 'begin 0644 ' + ExtractFilename(FFileName);
    FirstLine := False;
  END;

{ TXXEncode [XX Encoding] }

  CONSTRUCTOR TXXEncode.Create(Filename : STRING);
  BEGIN
    INHERITED Create(Filename);
    Table := '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz ';
  END;

{ Misc Functions }

  FUNCTION MakeUniqueID : STRING;
  VAR
    i : Integer;

  BEGIN
    Randomize;
    Result := '';
    FOR i := 1 TO 8 DO Result := Concat(Result, IntToStr(Random(9)));
  END;

END.
