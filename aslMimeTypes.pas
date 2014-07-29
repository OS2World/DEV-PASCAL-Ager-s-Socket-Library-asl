{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.3 $    $Date: 2002/04/22 23:26:31 $    $Author: sag $ }
{                                                                             }
{ aslMimeTypes (c) Copyright 1999 by Dwayne Heaton (dwayne@vmadd.demon.co.uk) }
{                                                                             }
Unit aslMimeTypes;

{$I aslDefine.inc}

Interface

Type
   TMimeDef = Record
      MimeType   : String[40];
      Extensions : ShortString;
   End;
   PMimeDef = ^TMimeDef;

   TMimeMgr = Class
   Private
{$IFDEF LOADSAVE}
      FFilename  : String;
      FMaxType   : Integer;
{$ENDIF}
      FMimeTypes : Array[1..2000] of PMimeDef;
      FNumTypes  : LongInt;

      Procedure SortTypes;
{$IFDEF LOADSAVE}
      Procedure LoadTypes;
      Procedure SaveTypes;
{$ENDIF}

   Public
{$IFDEF LOADSAVE}
      Constructor Create(Filename : String);
{$ELSE}
      Constructor Create;
{$ENDIF}
      Destructor  Destroy; OVERRIDE;

      Function Add(MimeType, Extension : String) : Boolean;
      Function IndexByType(MimeType : String) : LongInt;
      Function IndexByExt(Extension : String) : LongInt;
      Function MimeType(Filename : String) : String;
      Function Count: LongInt;
      Function Entry(n : LongInt) : TMimeDef;
   End;

Implementation

Uses SysUtils, aslUtils;

Function TMimeMgr.Add(MimeType, Extension : String) : Boolean;
Var
   i : LongInt;
   j : LongInt;
   s : String;

Begin
   Result := False;
   For i := 1 To FNumTypes Do
      If CompareText(FMimeTypes[i]^.MimeType, MimeType) = 0 Then
      Begin
         For j := 1 to WordCnt(Extension) Do
         Begin
            s := ExtractWords(j, 1, Extension);
            If Pos(s, FMimeTypes[i]^.Extensions) = 0 Then
            Begin
               FMimeTypes[i]^.Extensions := FMimeTypes[i]^.Extensions + ' ' + s;
               Result := True;
            End;
         End;
         Exit;
      End;

   Inc(FNumTypes, 1);
   GetMem(FMimeTypes[FNumTypes], SizeOf(TMimeDef));
   FMimeTypes[FNumTypes]^.MimeType := MimeType;
   FMimeTypes[FNumTypes]^.Extensions := Extension;

{$IFDEF LOADSAVE}
   If Length(MimeType) > FMaxType Then FMaxType := Length(MimeType);
{$ELSE}
   SortTypes;
{$ENDIF}
   Result := True;
End;

Function TMimeMgr.IndexByType(MimeType : String) : LongInt;
Var
   i : LongInt;

Begin
   For i := 1 to FNumTypes do
      If CompareText(MimeType, FMimeTypes[i]^.MimeType) = 0 Then
      Begin
         Result := i;
         Exit;
      End;
   Result := 0;
End;

Function TMimeMgr.IndexByExt(Extension : String) : LongInt;
Var
   i : LongInt;

Begin
   While Pos('.', Extension) <> 0 Do Delete(Extension, Pos('.', Extension), 1);
   Extension := LowerCase(Extension)+' ';
   For i := 1 to FNumTypes do
      If Pos(Extension, Lowercase(FMimeTypes[i]^.Extensions)+' ') <> 0 Then
      Begin
         Result := i;
         Exit;
      End;
   Result := 0;
End;

Function TMimeMgr.MimeType(Filename : String) : String;
Var
   i : LongInt;

Begin
   While Pos('.', Filename) <> 0 Do Delete(Filename, 1, Pos('.', Filename));
   i := IndexByExt(Filename);
   If i <> 0 Then
      Result := FMimeTypes[i]^.MimeType
   Else
      Result := 'application/octet-stream';
End;

Function TMimeMgr.Count : LongInt;
Begin
   Result := FNumTypes;
End;

Function TMimeMgr.Entry(n : LongInt) : TMimeDef;
Begin
   If (n<0) or (n>FNumTypes) Then Raise ERangeError.Create('Index out of range');
   Result := FMimeTypes[n]^;
End;

Procedure TMimeMgr.SortTypes;
Var
   i : LongInt;
   c : Boolean;
   m : PMimeDef;

Begin
   c := true;
   While c Do
   Begin
      c := false;
      For i := 1 To FNumTypes-1 Do
         If CompareText(FMimeTypes[i]^.MimeType, FMimeTypes[i+1]^.MimeType) > 0 Then
         Begin
            m := FMimeTypes[i];
            FMimeTypes[i] := FMimeTypes[i+1];
            FMimeTypes[i+1] := m;
            c := true;
         End;
   End;
End;

{$IFDEF LOADSAVE}
Procedure TMimeMgr.LoadTypes;
Var
   f : text;
   s : String;

Begin
   Assign(f, FFilename);
{$I-}
   Reset(f);
{$I+}
   If IOResult <> 0 Then Exit;

   While not Eof(f) Do
   Begin
      Readln(f, s);
      Add(ExtractWords(1, 1, s), ExtractWords(2, WordCnt(s)-1, s));
   End;
   Close(f);
   SortTypes;
End;

Procedure TMimeMgr.SaveTypes;
Var
   f : text;
   i : LongInt;

Begin
   SortTypes;
   Assign(f, FFilename);
{$I-}
   Rewrite(f);
{$I+}
   If IOResult <> 0 Then Exit;

   For i := 1 to FNumTypes do
      With FMimeTypes[i]^ Do
         Writeln(f, Format('%-*s  %s', [FMaxType, MimeType, Extensions]));
   Close(f);
End;
{$ENDIF}

{$IFDEF LOADSAVE}
Constructor TMimeMgr.Create(Filename : String);
{$ELSE}
Constructor TMimeMgr.Create;
{$ENDIF}
Begin
   Inherited Create;

{$IFDEF LOADSAVE}
   FFilename := Filename;
   FMaxType := 0;
{$ENDIF}

   FNumTypes := 0;
   Add('application/mac-binhex40', 'hqx');
   Add('application/msword', 'doc');
   Add('application/octet-stream', 'bin com dms exe lha lzh class');
   Add('application/pdf', 'pdf');
   Add('application/postscript', 'ai eps ps');
   Add('application/powerpoint', 'ppt');
   Add('application/rtf', 'rtf');
   Add('application/wordperfect5.1', 'wpd');
   Add('application/x-dvi', 'dvi');
   Add('application/x-javascript', 'js');
   Add('application/x-koan', 'skp skd skt skm');
   Add('application/x-latex', 'latex');
   Add('application/x-mif', 'mif');
   Add('application/x-netcdf', 'nc cdf');
   Add('application/x-stuffit', 'sit');
   Add('application/x-tar', 'tar');
   Add('application/x-tcl', 'tcl');
   Add('application/x-tex', 'tex');
   Add('application/x-texinfo', 'texinfo texi');
   Add('application/zip', 'zip');
   Add('audio/basic', 'au snd');
   Add('audio/midi', 'mid midi kar rmi');
   Add('audio/mpeg', 'mpga mp2 mp3');
   Add('audio/wav', 'wav');
   Add('audio/x-aiff', 'aif aiff aifc');
   Add('audio/x-pn-realaudio', 'ram');
   Add('audio/x-pn-realaudio-plugin', 'rpm');
   Add('audio/x-realaudio', 'ra');
   Add('image/gif', 'gif');
   Add('image/ief', 'ief');
   Add('image/jpeg', 'jpg jpe jpeg');
   Add('image/png', 'png');
   Add('image/tiff', 'tif tiff');
   Add('image/x-bitmap', 'bmp');
   Add('image/x-cmu-raster', 'ras');
   Add('image/x-portable-anymap', 'pnm');
   Add('image/x-portable-bitmap', 'pbm');
   Add('image/x-portable-graymap', 'pgm');
   Add('image/x-portable-pixmap', 'ppm');
   Add('image/x-rgb', 'rgb');
   Add('image/x-xbitmap', 'xbm xpm');
   Add('image/x-xwindowdump', 'xwd');
   Add('text/css', 'css');
   Add('text/html', 'htm html');
   Add('text/plain', 'txt');
   Add('text/richtext', 'rtx');
   Add('text/tab-separated-values', 'tsv');
   Add('text/x-setext', 'etx');
   Add('text/x-sgml', 'sgm sgml');
   Add('text/xml', 'xml dtd');
   Add('video/mpeg', 'mpg mpe mpeg');
   Add('video/quicktime', 'qt mov');
   Add('video/x-msvideo', 'avi');
   Add('video/x-sgi-movie', 'movie');
End;

Destructor TMimeMgr.Destroy;
Var
   i : LongInt;

Begin
{$IFDEF LOADSAVE}
   SaveTypes;
{$ENDIF}
   For i:=1 to FNumTypes Do FreeMem(FMimeTypes[i], SizeOf(TMimeDef));
   Inherited Destroy;
End;

End.
