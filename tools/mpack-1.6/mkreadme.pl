#!/usr/bin/perl

$systype = $ARGV[0] || die "usage: $0 systype\n";
if ($ARGV[1]) {
    $sysname = $ARGV[1];
} else {
    $sysname = $systype;
}

open(VERSION, "version.h") || die "can't read version.h: $!\n";
while (<VERSION>) {
    $version = $1 if /^#define MPACK_VERSION "(.*)"\n/;
}
die "Can't find version number in version.h\n" if !defined($version);

$hdr = "mpack/munpack version $version for $sysname\n";

print " " x ((75-length($hdr))/2), $hdr;

$dooutput = 1;
while (<DATA>) {
    if (/^=/) {
	$dooutput = /\W$systype\W/;
    } elsif (/^!/) {
	$dooutput = !/\W$systype\W/;
    } elsif ($dooutput) {
	s/^\.//;
	print;
    }
}

exit 0;
    
__END__

Mpack and munpack are utilities for encoding and decoding
(respectively) binary files in MIME (Multipurpose Internet Mail
Extensions) format mail messages.  For compatibility with older forms
of transferring binary files, the munpack program can also decode
messages in split-uuencoded format.  The Macintosh version can also
decode messages in split-BinHex format.

=arc
The Archimedes port interworks with Marcel (ANT's RISC OS MIME-compliant
MUA) to preserve filetypes, timestamps and access permissions.  munpack
should correctly unpack any Marcel message.  At present, mpack doesn't
include the filetype, etc. information.  This may be implemented in a
future release.

!
The canonical FTP site for this software is ftp.andrew.cmu.edu:pub/mpack/
Binaries are no longer provided. The pc, os2, amiga and archimedes ports
have been removed. The mac version probably doesn't compile anymore, but
is still included (MacOS X users can use the unix version...)

This MIME implementation is intended to be as simple and portable as
possible.  For a slightly more sophisticated MIME implementation, see
the program MetaMail, available via anonymous FTP to
thumper.bellcore.com, in directory pub/nsb


Decoding MIME messages:

=unix
First, you have to compile the munpack program.  See the instructions
in the section "Compilation" below.  If, after reading the
instructions, you are still unsure as to how to compile munpack,
please try to find someone locally to help you.

!
To decode a MIME message, first save it to a text file.  If possible,
!mac
save it with all headers included.  Munpack can decode some MIME files
=mac
save it with all headers included.  Mpack can decode some MIME files
!
when the headers are missing or incomplete, other files it cannot
decode without having the information in the headers.  In general,
messages which have a statement at the beginning that they are in MIME
format can be decoded without the headers.  Messages which have been
split into multiple parts generally require all headers in order to be
reassembled and decoded.

Some LAN-based mail systems and some mail providers (including America
Online, as of the writing of this document) place the mail headers at
the bottom of the message, instead of at the top of the message.  If
you are having problems decoding a MIME message on such a system, you
need to convert the mail back into the standard format by removing the
system's nonstandard headers and moving the standard Internet headers
to the top of the message (separated from the message body with a
blank line).

!mac
There must be exactly one message per file.  Munpack cannot deal with
multiple messages in a single file, to decode things correctly it must
know when one message ends and the next one begins.

To decode a message, run the command:

	munpack file

where "file" is the name of the file containing the message.  More than
one filename may be specified, munpack will try to decode the message in
each file.  For more information on ways to run munpack, see the section
"Using munpack" below.

=mac
There must be exactly one message per file.  Mpack cannot deal with
multiple messages in a single file, to decode things correctly it must
know when one message ends and the next one begins.

The Macintosh version of mpack/munpack is a single standalone
application.  A text file may be decoded either by drag & drop, or by
choosing the "Decode Files..." item from the application's File menu.
Non-text files may be encoded either by drag & drop, or by choosing
the "Encode Files..." item from the application's File menu.

The Macintosh version of mpack/munpack supports the new MacMIME
standard (RFC 1740).  This allows cross-platform transport of
Macintosh files to any MIME-capable machine, and also preserves
Macintosh specific file attributes between two Macintoshes.  Mpack
will use MacMIME for any unrecognized Macintosh file, and regular MIME
for standard MIME types.

For more details and descriptions of the preferences, see the "Help
Using Mpack..." menu item in mpack which can be found under the help
menu in systems 7 and above, and under the apple menu in older
systems.
!

Reporting bugs:

Bugs and comments should be reported to mpack-bugs@andrew.cmu.edu.
When reporting bugs or other problems, please include the following
information:

  * The version number of Mpack
  * The platform (Unix, PC, OS/2, Mac, Amiga, Archimedes)
  * The EXACT output of any unsuccessful attempts.
  * If having a problem decoding, the first couple of lines
    of the input file.


Compilation:

=unix
Mpack uses autoconf and automake on unix.
refer to INSTALL for more information

=pc
The pc sources have been compiled with Microsoft C version 7.0.  The
project files "mpack.mak" and "munpack.mak" are for mpack.exe and
munpack.exe, respectively.

=os2
The os2 sources have been compiled with IBM Cset++ version 2.1.  The
makefiles "mpack.os2" and "munpack.os2" are for mpack.exe and
munpack.exe, respectively. Both makefiles also include statements to
compile the sources with EMX 0.9a from Eberhard Mattes.

=amiga
The amiga sources have been compiled with SAS/C version 6.3. To compile
with SAS/C, use the command "smake".

=arc
The RISC OS sources have been compiled with Acorn C version 4.0 and
UnixLib 3.6e (ftp-able from micros.hensa.ac.uk).

=mac
Mpack was compiled with THINK C 6.0 with the 4-byte int option turned
on (and the ANSI-small library compiled with the 4-byte int option)
and prototype enforcement turned off.  Included with this distribution
should be the files "macproj.hqx" which is a BinHex4 version of the
THINK C 6.0 project file, and "macrsrc.hqx" which is a BinHex4 version
of the resources file.

Mpack checks for the existence of "Internet Config", and if it is
available, mpack uses it to translate MIME types to and from Macintosh
type/creator codes.  Included is the file "macICglue.hqx" which is a
BinHex4 version of Internet Config's MPW object file library.  This
needs to be linked with the application.

Using mpack:

See the "Help Using Mpack..." menu item in the application.

!mac
Using mpack:

Mpack is used to encode a file into one or more MIME format messages.
The program is invoked with:

	mpack [options] -o outputfile file

=unix os2 amiga
or 

	mpack [options] file address...

or

	mpack [options] -n newsgroups file

!mac
Where "[options]" is one or more optional switches described below.
"-o outputfile" is also described below. "file" is the name of the
!mac unix os2 amiga
file to encode.
=unix os2 amiga
file to encode, "address..." is one or more e-mail address to mail the
resulting messages to and "newsgroups" is a comma-separated list of
newsgroups to post the resulting messages to.

=amiga
There is extra flexibility in the options and addresses.  Use "mpack
?" for a list of options; a second "?" will print more information.
AmigaDOS 2.0 or later is required to run mpack.

!mac
The possible options are:

     -s subject
          Set the Subject header field to Subject.   By default,
          mpack will prompt for the contents of the subject
          header.

     -d descriptionfile
          Include the contents of the file descriptionfile in an
          introductory section at the beginning of the first
          generated message.

     -m maxsize
          Split the message (if necessary) into partial messages,
          each not exceeding maxsize characters.  The default
          limit is the value of the SPLITSIZE environment 
	  variable, or no limit if the environment variable
          does not exist.  Specifying a maxsize of 0 means there
          is no limit to the size of the generated message.

     -c content-type
          Label the included file as being of MIME type
          content-type, which must be a subtype of application,
          audio, image, or video.  If this switch is not given,
          mpack examines the file to determine its type.

     -o outputfile
          Write the generated message to the file outputfile.  If
          the message has to be split, the partial messages will
!mac arc
          instead be written to the files outputfile.01,
          outputfile.02, etc.

=arc
          instead be written to the files outputfile/01,
          outputfile/02, etc.

=os2
     -f username
	  Specifies the user name to be used for mail or news. If
	  this option is ommitted, the environment variable LOGNAME
	  will be used to determine the user name.

!mac
The environment variables which control mpack's behavior are:

     SPLITSIZE
          Default value of the -m switch.  Default "0".

=unix
     TMPDIR
	  Directory to store temporary files.  Default "/tmp".

=pc os2 amiga
     HOSTNAME
          Fully qualified domain name to use in generated message-ids.
	  Default "random-pc" or "random-amiga".

=arc
     HOSTNAME
          Fully qualified domain name to use in generated message-ids.
	  Default "random-arc".

=os2 amiga
     DOMAINNAME
          If $HOSTNAME does not have a '.' in it, this is appended to
          $HOSTNAME to get the domain to use. Default is
          "random-domain".

=amiga
     POSTNEWS
	  Command to use to post news. Compatible with Dillon UUCP usage.

     SENDMAIL
	  Command to use to send mail. Compatible with Dillon UUCP usage.

=os2
     POSTNEWS
	  Command to use to post news. Compatible with UUPC inews.

     SENDMAIL
	  Command to use to send mail. Compatible with IBM sendmail.

     LOGNAME
	  User name for sending mail or posting news. Defaults to
	  "postmaster".

!mac

Using munpack:

Munpack is used to decode one or more messages in MIME or
split-uuencoded format and extract the embedded files.  The program is
invoked with:

	munpack [options] filename...

!mac pc arc
which reads the messages in the files "filename...".  Munpack may also
be invoked with just:
=pc arc
which reads the messages in the files "filename...".  Wildcards are
accepted.  Munpack may also be invoked with just:
!mac

	munpack [options]

which reads a message from the standard input.

=amiga
Munpack does not accept wildcards.  Under AmigaDOS prior to 2.0, only
filenames are allowed.  Under AmigaDOS 2.0 or later, the options
described later are supported, with extra flexibility.  Use 
"munpack ?" for a list of options; a second "?" will print more
information.

!mac
If the message suggests a file name to use for the imbedded part, that
name is cleaned of potential problem characters and used for the
output file.  If the suggested filename includes subdirectories, they
will be created as necessary.  If the message does not suggest a file
name, the names "part1", "part2", etc are used in sequence.

If the imbedded part was preceded with textual information, that
information is also written to a file. The file is named the same as
the imbedded part, with any filename extension replaced with
!mac pc os2 arc
".desc"
=pc os2
".dsc"
=arc
"/dsc"
!mac

The possible options are:

     -f
          Forces the overwriting of existing files.  If a message
          suggests a file name of an existing file, the file will be
=unix amiga
          overwritten.  Without this flag, munpack appends ".1", ".2",
          etc to find a nonexistent file.
=pc os2
          overwritten.  Without this flag, munpack replaces any
          filename extension with ".1", ".2", etc to find a
          nonexistent file.
=arc
          overwritten.  Without this flag, munpack appends "/1", "/2",
          etc to find a nonexistent file.
!mac

     -t
	  Also unpack the text parts of multipart messages to files.
	  By default, text parts that do not have a filename parameter
	  do not get unpacked.

     -q
          Be quiet--suppress messages about saving partial messages.

     -C directory
          Change the current directory to "directory" before reading
          any files.  This is useful when invoking munpack
          from a mail or news reader.
=os2

     -e
          Writes the MIME content type into the extended attribute 
          "MIME-TYPE". This extended attribute can be used by any
          software that post processes the decoded files. The 
          following is an example program how to read this extended 
          attribute in Rexx:

---------------------------cut here---------------------------------
/* This Rexx program demonstrates how to read the MIME-type        *
 * parameter out of the EA of the decoded file. Note, that the     *
 * options '-e' must be used to write this EA.                     */

call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
call SysLoadFuncs

parse ARG name

if SysGetEA(name, "MIME-TYPE", "TYPEINFO") = 0 then
   parse var typeinfo 4 type
else 
   type = "undefined"

say "MIME content-type is" type
---------------------------cut here---------------------------------
!mac

The environment variables which control munpack's behavior are:

=unix
     TMPDIR
          Root of directory to store partial messages awaiting 
          reassembly.  Default is "/var/tmp".   Partial messages
	  are stored in subdirectories of $TMPDIR/m-prts-$USER/

=pc os2
     TMP
          Root of directory to store partial messages awaiting
          reassembly.  Default is "\tmp".  Partial messages are
          stored in subdirectories of $TMP\parts\

=arc
     Wimp$ScrapDir
          Root of directory to store partial messages awaiting
          reassembly.  Default is "/tmp" which UnixLib should do
          something sensible with.  Partial messages are
          stored in subdirectories of <Wimp$ScrapDir>.parts

=amiga
     METAMAIL_P_DIR [amiga version only]
          Root of directory to store partial messages awaiting 
          reassembly.  Default is "t:".   Partial messages are
	  stored in subdirectories of METAMAIL_P_DIR.

     USER, USERNAME
	 Subdirectory of METMAIL_P_DIR where mime parts are stored. USER
	 is checked first, then USERNAME. Default "anonymous".

If the shared library netsupport.library is available, it will be used
to get the value of the environment variables. This allows them to be
set in uulib:config file instead of in the environment. If this
library not present, then a warning that the library was not found is
printed. You may ignore this warning.
!

Acknowledgements:

Written by John G. Myers, jgm+@cmu.edu

The mac version was written by Christopher J. Newman, chrisn+@cmu.edu

Send all bug reports to mpack-bugs@andrew.cmu.edu 

Thanks to Nathaniel Borenstein for testing early versions of mpack and
for making many helpful suggestions.


PGP signature:

The mpack 1.6 distribution is not pgp signed.

Legalese:

(C) Copyright 1993,1994 by Carnegie Mellon University
All Rights Reserved.

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Carnegie Mellon
University not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.  Carnegie Mellon University makes no representations about
the suitability of this software for any purpose.  It is provided "as
is" without express or implied warranty.

CARNEGIE MELLON UNIVERSITY DISCLAIMS ALL WARRANTIES WITH REGARD TO
THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS, IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY BE LIABLE
FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

Portions of this software are derived from code written by Bell
Communications Research, Inc. (Bellcore) and by RSA Data Security,
Inc. and bear similar copyrights and disclaimers of warranty.

