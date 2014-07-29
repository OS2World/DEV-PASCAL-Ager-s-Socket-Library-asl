{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.5 $    $Date: 1999/01/13 17:19:15 $    $Author: sag $ }
{                                                                             }
{ Program to test that the basic units can be compiled :-)                    }
PROGRAM Compile;

USES CTypes,
{$IFDEF OS2}
     nerrno, netdb, OS2Socket, sockin, types, utils;
{$ELSE}
     Winsock;
{$ENDIF}

BEGIN
END.

