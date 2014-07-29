{ Ager's Socket Library (c) Copyright 1998-99 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.1 $    $Date: 1999/03/25 06:31:13 $    $Author: sag $ }
{                                                                             }
{ Misc. usefull classes                                                       }
UNIT aslClasses;

{$I aslDefine.Inc}

INTERFACE

USES VPSysLow, VPUtils;

TYPE
  TThread = CLASS
  PRIVATE
    FThreadID        : LongInt;
    FTerminated      : Boolean;
    FSuspended       : Boolean;
    FFreeOnTerminate : Boolean;
    FFinished        : Boolean;
    FReturnValue     : Integer;

    PROCEDURE SetSuspended(Value: Boolean);
  PROTECTED
    PROCEDURE DoTerminate; VIRTUAL;
    PROCEDURE Execute; VIRTUAL; ABSTRACT;

    PROPERTY ReturnValue: Integer READ FReturnValue WRITE FReturnValue;
    PROPERTY Terminated: Boolean  READ FTerminated;
  PUBLIC
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;
    PROCEDURE Resume;
    PROCEDURE Suspend;
    PROCEDURE Terminate;
    FUNCTION  WaitFor: LongInt;

    PROPERTY FreeOnTerminate: Boolean READ FFreeOnTerminate WRITE FFreeOnTerminate;
    PROPERTY Suspended: Boolean       READ FSuspended       WRITE SetSuspended;
    PROPERTY ThreadID: LongInt        READ FThreadID;
  END;

IMPLEMENTATION

{ TThread }

  FUNCTION ThreadFunc(P: Pointer): LongInt;
  VAR
    FreeThread: Boolean;
  BEGIN
    TThread(P).Execute;
    FreeThread:=TThread(P).FFreeOnTerminate;
    Result:=TThread(P).FReturnValue;
    TThread(P).FFinished:=True;
    TThread(P).DoTerminate;
    IF FreeThread THEN TThread(P).Free;
    EndThread(Result);
  END;


  CONSTRUCTOR TThread.Create;
  BEGIN
    INHERITED Create;
    FTerminated:=False;
    FSuspended:=False;
    FFreeOnTerminate:=True;
    FFinished:=False;
    FReturnValue:=0;
    FThreadID:=VPBeginThread(ThreadFunc, 32768, Pointer(Self));
  END;

  DESTRUCTOR TThread.Destroy;
  BEGIN
    IF NOT FFinished AND NOT Suspended THEN
    BEGIN
      Terminate;
      WaitFor;
    END;
    INHERITED Destroy;
  END;

  PROCEDURE TThread.DoTerminate;
  BEGIN
  END;

  PROCEDURE TThread.SetSuspended(Value: Boolean);
  BEGIN
    IF Value<>FSuspended THEN
      IF Value THEN Suspend ELSE Resume;
  END;

  PROCEDURE TThread.Suspend;
  BEGIN
    FSuspended:=True;
    SuspendThread(FThreadID);
  END;

  PROCEDURE TThread.Resume;
  BEGIN
    IF ResumeThread(FThreadID)=0 THEN FSuspended:=False;
  END;

  PROCEDURE TThread.Terminate;
  BEGIN
    FTerminated:=True;
  END;

  FUNCTION TThread.WaitFor: LongInt;
  BEGIN
    WHILE NOT FFinished DO
      SysCtrlSleep(1000);
    WaitFor:=0;
  END;

END.

