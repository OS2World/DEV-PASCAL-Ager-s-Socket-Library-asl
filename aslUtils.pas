{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.3 $    $Date: 2002/07/18 22:17:50 $    $Author: sag $ }
{                                                                             }
{ Misc. usefull functions/procedures                                          }
{ Parts copyright 99 by Dwayne Heaton (dwayne@vmadd.demon.co.uk)              }
UNIT aslUtils;

{$I aslDefine.Inc}

INTERFACE

USES SysUtils;

FUNCTION NextWord(VAR s: STRING): STRING;
FUNCTION NextWordDelim(VAR s: STRING; Delim: Char): STRING;
Function WordCnt(s : String) : Integer;
Function ExtractWords(StartWord, NoWords : Integer; s : String) : String;

IMPLEMENTATION

  FUNCTION NextWord(VAR s: STRING): STRING;
  VAR
    p  : Byte;
  BEGIN
    p:=Pos(' ',s);
    IF p>0 THEN
    BEGIN
      NextWord:=Copy(s, 1, p-1);
      Delete(s, 1, p);
    END ELSE
    BEGIN
      NextWord:=s;
      s:='';
    END;
    s:=TrimLeft(s);
  END;

  FUNCTION NextWordDelim(VAR s: STRING; Delim: Char): STRING;
  VAR
    p  : Byte;
  BEGIN
    p:=Pos(Delim,s);
    IF p>0 THEN
    BEGIN
      NextWordDelim:=Copy(s, 1, p-1);
      Delete(s, 1, p);
    END ELSE
    BEGIN
      NextWordDelim:=s;
      s:='';
    END;
    s:=TrimLeft(s);
  END;

  Function LocWord(StartAT, WordNo : Integer; s : String) : Integer;
  Var
     W           : Integer;
     SpaceBefore : Boolean;

  Begin
     LocWord := 0;
     If (s = '') or (WordNo < 1) or (StartAT > Length(s)) Then Exit;
     SpaceBefore := True;
     W := 0;
     Dec(StartAT, 1);
     While (W < Wordno) and (StartAT <= Length(s)) Do
     Begin
        StartAT := Succ(StartAT);
        If SpaceBefore and Not (s[StartAT] in [#9, ' ']) Then
        Begin
           Inc(W, 1);
           SpaceBefore := False;
        End Else
           If Not SpaceBefore and (s[StartAT] in [#9, ' ']) Then
              SpaceBefore := True;
     End;
     If W = WordNo Then
        LocWord := StartAT
     Else
        LocWord := 0;
  End;

  Function WordCnt(s : String) : Integer;
  Var
     W,
     i           : Integer;
     SpaceBefore : Boolean;

  Begin
     WordCnt := 0;
     If s = '' Then Exit;
     SpaceBefore := True;
     W := 0;
     For i := 1 to Length(s) Do
     Begin
        If SpaceBefore and Not (s[i] in [#9, ' ']) Then
        Begin
           Inc(W, 1);
           SpaceBefore := False;
        End Else
           If Not SpaceBefore and (s[i] in [#9, ' ']) Then
              SpaceBefore := True;
     End;
     WordCnt := W;
  End;

  Function ExtractWords(StartWord, NoWords : Integer; s : String) : String;
  Var
     Start,
     Finish : Integer;

  Begin
     ExtractWords := '';
     If s = '' Then Exit;
     Start := LocWord(1, StartWord, s);
     If Start = 0 Then Exit;
     Finish := LocWord(Start, Succ(NoWords), s);
     If Finish = 0 Then Finish := Length(s) + 1;
     Repeat
        Dec(Finish, 1);
     Until Not (s[Finish] in [#9, ' ']);
     ExtractWords := Copy(s, Start, (Finish-Start) + 1);
  End;

END.

