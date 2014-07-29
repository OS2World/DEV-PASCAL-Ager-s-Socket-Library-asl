              Ager's Socket Library (asl), Release 5 15/10-2002
              =================================================


What is it:
-----------

  asl is a free socket (tcp/ip) library for Virtual Pascal v2.0+ (it supports
both OS/2 and Win32). It includes classes to:
  - use TCP and UDP sockets
  - handle the SMTP, POP3, NNTP, FTP and CVS protocols (as client)
  - write servers (SMTP and POP3 already included)
  - encode and decode using uu, xx or base64
  - handle messages in text and html format

  These classes are the first I write using the new Delphi class model, so
my code might not be optimal in some places ;-)

  I wrote this library for a number of reasons. The first reason is that I
could not find any other tcp/ip libraries that worked quite as I wanted
them to. Secondly I wanted to learn how the new Dephi class model worked.
And third I wanted to learn how to program sockets and the various
Internet protocols.

  The basic socket hierarcy is loosely based om some source (socks.zip) for
Delphi written by akorud@polynet.lviv.ua (sorry who ever you are, but your
"real" name was not mentioned).

  Note that this release is not as complete (it should be fully functional)
as I would have liked it to be, but there seems to be a lot of demand for a
socket library - so I decided to release it.



Installation:
-------------

  To install asl, do the following:

  1) Unpack it to an empty directory (incl. directories) e.g. c:\asl. The
     drive must support long filnames (HPFS, NTFS, FAT32 etc.)

  2) Add the directory (c:\asl) to the start of your include and unit
     directories in VP and add os2.lib (c:\asl\lib.os2) to the library path.

  3) You are now ready to use asl - it is recommended for firsttime users to
     take a look at the example programs first (c:\asl\OOExample).



History:
--------

  Major changes in release 5:
   - Working ftp client
   - Added html message support (thanks Kevin)

  Major changes in release 4:
   - Again this is mostly a bugfix release
   - There is also a new abstract CVS client

  Major changes in release 3:
   - This is mostly a bugfix release
   - There is also the start of an FTP client - but is is not working yet!

  Major changes in release 2:
   - Added support for writing servers
   - Added support for encoding and decoding

  For a complete list of changes, please see the included ChangeLog



Hierarchy:
----------

  TAbsSocket                          aslAbsSocket.pas
   |
   +- TTCPSocket                      aslTCPSocket.pas
   |  |
   |  +- TTCPClientSocket             --"--
   |  |   |
   |  |   +- TBufTCPClientSocket      --"--
   |  |
   |  +- TTCPServerSocket             --"--
   |
   +- TUDPSocket                      aslUDPSocekt.pas


  TAbsClient                          aslAbsClient.pas
   |
   +- TAbsSMTPClient                  aslSMTPClient.pas
   |   |
   |   +- TSMTPClient                 --"--
   |
   +- TAbsPOP3Client                  aslPOP3Client.pas
   |   |
   |   +- TPOP3Client                 --"--
   |
   +- TAbsNNTPClient                  aslNNTPClient.pas
   |
   +- TAbsCVSClient                   aslCVSClient.pas
   |
   +- TAbsFTPClient                   aslFTPClient.pas
       |
       +- TFTPClient                  --"--


  The TAbsXXXXClient classes are a raw implementation of the commands used by
the protocol (with errorchecking), whereas TXXXXClient has more logic build in.


  TAbsServerManager                   aslAbsServer.pas

  TAbsServer                          aslAbsServer.pas
   |
   +- TAbsTextServer                  --"--
       |
       +- TPOP3Server                 aslPOP3Server.pas
       |
       +- TSMTPServer                 aslSMTPServer.pas


Design:
-------

            +- Client -------------------+ +- Server -------------------+
            | aslAbsClient               | | aslAbsServer               |
            +----------------------------+ +----------------------------+
            +-----------------------------------------------------------+
Base        | aslAbsSocket                                              |
objects     |   aslTCPSocket                                            |
            |   aslUDPSocket                                            |
            +-----------------------------------------------------------+
            +-----------------------------------------------------------+
Abstraction | aslSocket.pas                                             |
layer       | CTypes.pas                                                |
            +-----------------------------------------------------------+
            +- Windows ------------------+ +- OS/2 ---------------------+
Low         | winsock.pas                | | OS2Socket.pas   types.pas  |
level       |                            | | ioctl.pas       sockin.pas |
interface   |                            | | nerrno.pas      netdb.pas  |
            |                            | | utils.pas                  |
            +----------------------------+ +----------------------------+



Documentation:
--------------

  Sorry, but at the moment the only documentation is the source code itself
and the example programs.



License:
--------

  Ager's Socket Library (asl) is copyright (c) 1998-2002 by Soren Ager. You
are free to use asl in your programs under under the following conditions:

- The following text must be shown with your own copyright message in the
  program and documentation:
  "Portions copyrighted 1998-2002 by Soren Ager"

- You may ONLY distribute the whole unmodified archive.

- If you make ANY modifications to the source you HAVE to send all the changes
  to me for inclusion in a future release.



Thanks to:
----------
  Kevin G. McCoy - the html messages support and bug reports
  Dwayne Heaton  - for the encoding and decoding routines.
  Robert Boeck   - bug reports and some fixes.



Contact:
--------

  If you have questions/ideas/fixes or other comments, fell free to contact me:
Soren Ager <sag@poboxes.com>
  The latest version of this library is always available at:
http://home10.inet.tele.dk/sag/asl.html

