{ Ager's Socket Library (c) Copyright 1998-02 by Soren Ager (sag@poboxes.com) }
{                                                                             }
{ $Revision: 1.1 $    $Date: 2002/10/02 01:13:10 $    $Author: sag $ }
{                                                                             }
{ HTML message example                                                        }

// Soren Ager's SMTP Client, with HTML additions (and some other minor stuff) By Kevin G. McCoy, IDK, Inc.
// This is a test/demo program showing the added features

// I needed to send automated HTML-formatted Email out but Ager's library did not support MS-Style HTML.
// I added some methods and fields to Ager's classes and came up with the following. You still have to
// generate the actual HTML code, but it is now made easy to stuff it into an Email message.  I typically use
// Netscape Composer to create the HTML.

// I also added message priority (MS uses it for return-receipts), and X-Mailer used to identify the application
// that sent the mail. X-Mailer is mostly a vanity thing, but you could use it to identify the version of your
// mailer if you have older application versions out in the field all sending you mail.

PROGRAM SMTPSendHTML;

{$I aslDefine.Inc}
{&PMType VIO}

USES
{$IFDEF Debug}
  HeapChk,
{$ENDIF}
  Application, aslGlobals, aslSMTPCLient;

TYPE
  TSMTPClientApp = CLASS(TaslApplication)
  PRIVATE
    Server : STRING;
    From   : STRING;
    To_    : STRING;
    Domain : STRING;
    Subject: STRING;
  PUBLIC
    PROCEDURE ShowCopyright; OVERRIDE;
    PROCEDURE Run; OVERRIDE;
    PROCEDURE SendTheMessage;
  END;


  PROCEDURE TSMTPClientApp.ShowCopyright;
  BEGIN
    INHERITED ShowCopyright;
    WriteLn('HTML v1.0 additions by Kevin McCoy, IDK, Inc.');
    WriteLn('kgmccoy@idk-inc.com');
  END;

  PROCEDURE TSMTPClientApp.Run;
  BEGIN
    INHERITED Run;
    SendTheMessage;
  END;

  PROCEDURE TSMTPClientApp.SendTheMessage;
  VAR
    Cli : TSMTPCLient;
    Msg : TSMTPHTMLMessage;
  BEGIN
    Msg:=TSMTPHTMLMessage.Create;
    Msg.AddTo('kate@idk-inc.com');
    Msg.AddTo('kgmccoy@idk-inc.com');
    Msg.From:='kgmccoy@idk-inc.com';
    Msg.Subject:='Test Message';
    Msg.Mailer := 'HyperAtomicMailer v17.1 New Improved Turbo Deluxe Platinum Limited Edition'; // added KGM - now you can identify your mailer app

    // The usual message body text
    // This part gets sent as plain text for old-style email readers.
    Msg.AddMsgBody('This is a test message.');
    Msg.AddMsgBody('    Blah de blah blah blah!');
    Msg.AddMsgBody('    Blah de blah blah blah!');
    Msg.AddMsgBody('    Blah de blah blah blah!');
    Msg.AddMsgBody('    Blah de blah blah blah!');
    Msg.AddMsgBody('');
    Msg.AddMsgBody('Best regards,');
    Msg.AddMsgBody('');
    Msg.AddMsgBody('The Email Robot');

    // Now we add the HTML for more recent Email readers... This is MS Outlook/Outlook Express compatible.
    // Use Netscape or some other HTML generator to create the HTML; Paste it in line by line and add Pascal variables...
    Msg.AddHTMLBody('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">');
    Msg.AddHTMLBody('<HTML><HEAD>');
    Msg.AddHTMLBody('<META http-equiv=Content-Type content="text/html; charset=iso-8859-1">');
    Msg.AddHTMLBody('<META content="MSHTML 6.00.2715.400" name=GENERATOR>');
    Msg.AddHTMLBody('<STYLE></STYLE>');
    Msg.AddHTMLBody('</HEAD>');
    Msg.AddHTMLBody('<body text="#FFFFFF" bgcolor="#660066" link="#FFFF00" vlink="#FFFF77" alink="#FF0077">');
    Msg.AddHTMLBody('<font face="Courier New,Courier">This is a test message.</font>');
    Msg.AddHTMLBody('<br>&nbsp;');
    Msg.AddHTMLBody('<font face="Courier New,Courier">Here are some links:</font>');
    Msg.AddHTMLBody('<br>&nbsp;');
    Msg.AddHTMLBody('<p>&nbsp;<a href="http://stats.distributed.net/ogr-25/psummary.php3?id=17415">My OGR Stats</a>');
    Msg.AddHTMLBody('<br>&nbsp<a href="http://stats.distributed.net/rc5-64/psummary.php3?id=17415">My RC5 Stats</a>');
    Msg.AddHTMLBody('<br>&nbsp;');
    Msg.AddHTMLBody('<p><font face="Courier New,Courier">Blah de blah blah blah!</font>');
    Msg.AddHTMLBody('<br>&nbsp;');
    Msg.AddHTMLBody('<font face="Courier New,Courier">Blah de blah blah blah!</font>');
    Msg.AddHTMLBody('<br>&nbsp;');
    Msg.AddHTMLBody('<font face="Courier New,Courier">Blah de blah blah blah!</font>');
    Msg.AddHTMLBody('<br>&nbsp;');
    Msg.AddHTMLBody('<font face="Courier New,Courier">Blah de blah blah blah!</font>');
    Msg.AddHTMLBody('<br>&nbsp;');
    Msg.AddHTMLBody('<p><font face="Courier New,Courier">Best regards,</font>  ');
    Msg.AddHTMLBody('<p><font face="Courier New,Courier">The Email Robot</font>  ');
    Msg.AddHTMLBody('</body>');

    Cli:=TSMTPClient.Create;
    Domain := 'idk-inc.com';
    Server := 'mail.west.net';
    Cli.Connect(Server, Domain); // connect to the SMTP server
    Cli.SendHTMLMsg(Msg); // send the message.  HTML enabled Email readers will "see" the HTML.  Non-HTML Email readers will see the plain AddMsgBody text.

    Cli.Destroy;
    Msg.Destroy;
  END;

VAR
  App: TSMTPCLientApp;

BEGIN
  App := TSMTPCLientApp.Create('SMTPSendHTML');
  App.Run;
  App.Destroy;
END.

