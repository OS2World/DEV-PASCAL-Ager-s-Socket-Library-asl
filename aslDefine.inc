{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.4 $    $Date: 2002/05/26 18:50:07 $    $Author: sag $ }
{                                                                             }
{ Defines and compiler switches used by asl                                   }

{$B-,T-,V-,X+}
{&Delphi+,Use32+}
{$M 32768}

{$IFDEF Debug}
  {&Optimize-,SmartLink-,ZD+,Frame+}
  {$D+,LocInfo+,R+,S+,I+,Q+,W+}
{$ENDIF}

{ Removed directives }
{ ,H-,Open32-,J+,P+,,,Z-,OrgName-, }
{ AlignCode+,AlignData+,AlignRec-,Asm-,Delphi+,G3+}

