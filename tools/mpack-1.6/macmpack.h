/* macmpack.h -- resources for mac interface to mpack
 */
/* (C) Copyright 1995 by Carnegie Mellon University
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of Carnegie
 * Mellon University not be used in advertising or publicity
 * pertaining to distribution of the software without specific,
 * written prior permission.  Carnegie Mellon University makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * CARNEGIE MELLON UNIVERSITY DISCLAIMS ALL WARRANTIES WITH REGARD TO
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY BE LIABLE
 * FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
 * OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */
/* (C) Copyright 1993-1995 by Christopher J. Newman
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Christopher J. Newman not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Christopher J. Newman makes no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * CHRISTOPHER J. NEWMAN DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 * SHALL CHRISTOPHER J. NEWMAN BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 */

#define progDLOG	128
#define decodeDLOG	129
#define warnALRT	131
#define errorALRT	133
#define prefsDLOG	134
#define dstatDLOG	135
#define sendDLOG	136

#define prefsID		128
#define IDnaID		1000

#define mainMBAR	128

#define textWIND	128
#define helpWIND	129

#define helpTEXT	128
#define helpSTYL	128

/* file menu */
#define iEncode		1
#define iDecode		2
#define iInsert		3
#define iClose		4
#define iPrefs      6
#define iQuit		8

/* help menu */
#define iHelp       1

/* progress dialog items */
#define iWorkText	1
#define iProgress	3

/* decode dialog items */
#define iAdd		3
#define iRemove		4
#define iFileList	5
#define iFileScroll	6

/* replace alert */
#define iNewname	1
#define iReplace	2

/* preferences dialog */
#define iHost       3
#define iAuto		6
#define iData		7
#define iSingle		8
#define iDouble		9
#define iTextEncode 10
#define iQuitFinish	11
#define iEmailAddr	12
#define iMailServer	15
#define iSet		16

/* decode status dialog */
#define iStatus		2
#define iStatScroll	3

/* encode/send dialog */
#define iSubj       4
#define iEmail      5
#define iSavefile	6
#define iEmailto    7
#define iLimit      8
#define iPartLimit  9
#define iBar        11
#define iDescEdit	3

/* mpack prefs folder */
extern struct pref_folder {
	short refnum;
	FSSpec fspec;
	unsigned char prefs[257];
} *pfolder;

/* mpack preferences */
extern struct mpack_preferences {
	short encoding;
	short extract_text;
	short quit_finished;
	short reserved[4];
	char internet_host[2];	/* C string */
} **mpack_prefs;

/* encodings */
#define EN_AUTO   0
#define EN_DATA   1
#define EN_DOUBLE 2
#define EN_SINGLE 3

/* shared routines */
FILE *Macopen(FILE *, Str255, short, long, short, short, SignedByte);
void maccleanup(void);
void MapTypeCreator(char *, OSType);

/* used for chatting */
void statrefresh(void);
void stattext(Str255, unsigned char);
extern short didchat;

/* buffer for copy operations */
#define COPY_BUFSIZE 1024
extern char copy_buf[COPY_BUFSIZE];

/* watch cursor */
extern Cursor watch;
