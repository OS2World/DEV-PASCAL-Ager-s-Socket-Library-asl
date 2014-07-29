{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.8 $    $Date: 2002/10/15 19:28:23 $    $Author: sag $ }
{                                                                             }
{ Global definitions used by asl                                              }
UNIT aslGlobals;

{$I aslDefine.Inc}

INTERFACE

CONST
  aslRelease = '5'
{$IFDEF VirtualPascal}
  {$IFDEF OS2}
                  +' - VP/2'
  {$ENDIF}
  {$IFDEF Win32}
                  +' - VP/Win32'
  {$ENDIF}
{$ENDIF}
  ;

  aslCopyright = '(c) 1998-2002 by Soren Ager';
  aslPartOf    = 'Part of Ager''s Socket Library.';

IMPLEMENTATION

END.

