/* macmpack.c -- Mac user interface to mpack routines
 *
 * (C) Copyright 1993-1995 by Carnegie Mellon University
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Carnegie Mellon University
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Carnegie
 * Mellon University makes no representations about the suitability of
 * this software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * CARNEGIE MELLON UNIVERSITY DISCLAIMS ALL WARRANTIES WITH REGARD TO
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY BE LIABLE
 * FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
 * OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 * NOTE: a good GUI requires a lot of work...  This needs more.
 */

#include <Folders.h>
#include <Script.h>
#include <GestaltEqu.h>

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include "version.h"
#include "part.h"
#include "macnapp.h"
#include "macmpack.h"
#include "macICTypes.h"
#include "macICAPI.h"
#include "macICKeys.h"

/* ThinkC's internal stdio functions: */
#include <ansi_private.h>

/* window types: */
#define DECODELIST	1
#define PREFWIN		2

/* save watch cursor */
Cursor watch;

/* preferences */
struct pref_folder *pfolder = NULL;
struct mpack_preferences **mpack_prefs = NULL;
static ICInstance icinst = NULL;

/* flag for active help window */
static WindowPtr helpw = NULL;

/* active decode status window */
static na_win *curstatwin = NULL;
short didchat;

/* MacTCP started: -1 = error, 1 = active, 0 = unknown */
static short tcpstart = 0;

/* this is used for opening TEXT files */
SFTypeList textList = { 'TEXT', 0, 0, 0 };

/* next two types are used in the dialog used to select files to decode
 */
typedef struct filelist {
	short vRefNum;
	long dirID;
	PCstr fname[65];
} filelist;
typedef struct listwin {
	na_win win;
	int count;
	filelist **hflist;
	ListHandle l;
} listwin;

/* this is the status window for decoding
 */
typedef struct statuswin {
	na_win win;
	RgnHandle urgn;			/* user region */
	Rect urect;				/* user rectangle */
	Rect frect;				/* frame rectangle */
	short row;				/* row at top of scroll area */
	short nrow;				/* rows of status text */
	long size, used;		/* bytes of status text & amount used */
	Handle text;			/* status text */
	ControlHandle sb;		/* scroll bar control */
	short height, ascent;	/* font height and ascent */
} statuswin;

/* this is for the encode window
 */
typedef struct encodewin {
	nate_win w;
	Boolean nateon;				/* cursor in Desc edit field */
	Boolean useemail;			/* sending email */
	long partsize;				/* max part size (0 = no limit) */
	OSType ftype;				/* type of file to encode */
	FSSpec fspec;				/* file to encode */
	FSSpec ofile;				/* output file */
} encodewin;

/* show progress
 */
typedef struct progresswin {
	natcp_win w;
	short percent;
} progresswin;

/* send mail
 */
typedef struct mailwin {
	progresswin w;
	Handle headers;		/* email headers */
	Handle envelope;	/* envelope */
	short state;		/* state */
	short remaining;	/* messages remaining */
	Boolean sending;	/* flag for active SMTP task */
	Boolean useemail;	/* sending email */
	Boolean gothost;	/* got the hostname */
	long partsize;		/* max part size (0 = no limit) */
	long dirID;			/* ID for temp dir */
	OSType ftype;		/* type of file to encode */
	FSSpec fspec;		/* file to be encoded */
	FSSpec ofile;		/* output file */
	FILE *dfile;		/* desc file */
	PCstr server[257];	/* SMTP server */
	PCstr subj[257];	/* subject */
	CInfoPBRec cpb;
} mailwin;

/* mailwin states */
#define MS_MACTCP	0	/* Starting MacTCP */
#define MS_GETHOST	1	/* Getting hostname */
#define MS_ENCODE	2	/* Do encoding */
#define MS_SENDING	3	/* Sending email */

/* some prototypes */
void warn(char *str);
void stattext(Str255, unsigned char);
static void do_decodefiles(na_win *);
static void addfile(listwin *, FSSpec *);
static void removefile(listwin *);
static short listclose(na_win *);
static short listmouse(na_win *, Point, short, short);
static short listctrl(na_win *, Point, short, short, ControlHandle);
static short listupdate(na_win *, Boolean);
static short listinit(na_win *,long *);
static short prefsctrl(na_win *, Point, short, short, ControlHandle);
static short prefsinit(na_win *, long *);
static void do_decode(FSSpec *);
static void do_encode(FSSpec *, OSType);
static short mainmenu(struct na_win*, WORD, WORD);

#define dwin ((listwin *) win)
#define swin ((statuswin *) win)
#define ewin ((encodewin *) win)
#define twin ((nate_win *) win)
#define prwin ((progresswin *) win)
#define mwin ((mailwin *) win)

/* Get a FILE* to a Macintosh file
 *******************************     ###############################
 * KLUDGE ALERT! KLUDGE ALERT! *     # KLUDGE ALERT! KLUDGE ALERT! #
 *******************************     ###############################
 * Mac files are specified by name/vRefNum/dirID combo, but the portable
 * portions of mpack use FILE* to do I/O.  We need a way to get an open FILE*
 * from a file specified by name/vRefNum/dirID.  Here we use the proper Macintosh
 * routines to open a file, then hack together a FILE* using ThinkC's internal
 * routines.  The major trouble is that we have no way to get at the FILE action
 * procedure (fp->proc), so we need a sample FILE* to be passed in.  Bleargh!
 *******************************     ###############################
 * KLUDGE ALERT! KLUDGE ALERT! *     # KLUDGE ALERT! KLUDGE ALERT! #
 *******************************     ###############################
 */
FILE *Macopen(FILE *sample, Str255 name, short vRefNum, long dirID,
				short binary_flag, short res_fork, SignedByte permission)
{
	FILE *fp = NULL;
	short refnum;
	long curEOF;
	OSErr err;
	
	if ((!res_fork && (err = HOpen(vRefNum, dirID, name, permission, &refnum)) == noErr)
		|| (res_fork && (err = HOpenRF(vRefNum, dirID, name, permission, &refnum)) == noErr)) {
		if ((fp = __getfile()) == NULL) {
			FSClose(refnum);
		} else {
			if (permission == fsWrPerm) {
				/* if we're writing to the file, truncate it */
				SetEOF(refnum, curEOF = 0);
			} else {
				GetEOF(refnum, &curEOF);
			}
			fp->refnum = refnum;
			fp->len = (fpos_t) curEOF;
			fp->binary = binary_flag;
			setvbuf(fp, NULL, _IOFBF, BUFSIZ);
			fp->proc = sample->proc;
		}
	}
	
	return (fp);
}


/* warn the user
 */
void warn(char *str)
{
	PCstr wstr[257];
	
	CtoPCstrncpy(wstr, str, 255);
	ParamText(P(wstr), NULL, NULL, NULL);
	NAalert(warnALRT);
}

/* yell at the user
 */
void yell(char *str)
{
	PCstr wstr[257];
	
	CtoPCstrncpy(wstr, str, 255);
	ParamText(P(wstr), NULL, NULL, NULL);
	NAalert(errorALRT);
}

/* chat with user
 */
chat(char *str)
{
	PCstr tmpstr[257];

	CtoPCstrcpy(tmpstr, str);
	stattext(P(tmpstr), 0);
}

/* returns NA_ALLCLOSED if appropriate, else NA_CLOSED
 */
static short alldone(na_win *win)
{
	if (win->next == NULL && win->afterp == NULL && (*mpack_prefs)->quit_finished
		&& RecoverHandle((Ptr) win) == (Handle) NAhead) {
		return (NA_ALLCLOSED);
	}
	
	return (NA_CLOSED);
}

/* update procedure for status dialog box
 */
static short statupdate(na_win *win, Boolean newsize)
{
	RgnHandle savergn;
	unsigned char *s;
	short row, top;
	Rect tmpr;
	
	FrameRect(&swin->frect);
	savergn = NewRgn();
	if (savergn) {
		GetClip(savergn);
		SetClip(swin->urgn);
	}
	
	/* redraw text area */
	HLock(swin->text);
	s = * (unsigned char **) swin->text;
	top = swin->urect.top;
	for (row = 0; row < swin->row; ++row) {
		s += s[1] + 2;
	}
	for (; row < swin->nrow && top + swin->height <= swin->urect.bottom; ++row) {
		MoveTo(swin->urect.left, top + swin->ascent);
		if (*s) TextFace(1);
		DrawString(s + 1);
		if (*s) TextFace(0);
		/* advance to next string */
		top += swin->height;
		s += s[1] + 2;
	}
	HUnlock(swin->text);
	
	if (savergn) {
		SetClip(savergn);
		DisposeRgn(savergn);
	}

	return (NA_NOTPROCESSED);
}

/* refresh status window
 */
void statrefresh()
{
	na_win *win = curstatwin;
	
	Draw1Control(swin->sb);
	statupdate(win, false);
}

/* add text to the status window
 */
void stattext(Str255 str, unsigned char bold)
{
	na_win *win = curstatwin;
	short i, len;
	unsigned char *s, *start;
	RgnHandle rgn;
	Rect tmpr;
	
	if (!win) return;
	didchat = 1;
	
	/* advance to next row */
	if (swin->height * (swin->nrow++ - swin->row)
		>= swin->urect.bottom - swin->urect.top) {
		SetCtlMax(swin->sb, ++swin->row);
		SetCtlValue(swin->sb, swin->row);
		if ((rgn = NewRgn()) != NULL) {
			tmpr = swin->urect;
			ScrollRect(&tmpr, 0, -swin->height, rgn);
			DisposeRgn(rgn);
		}
	}
	
	/* add the text */
	len = * (unsigned char *) str;
	if (swin->size - swin->used < len + 1) {
		SetHandleSize(swin->text, swin->size * 2);
		if (MemError() == 0) swin->size *= 2;
	}
	HLock(swin->text);
	s = start = * (unsigned char **) swin->text;
	for (i = 1; i < swin->nrow; ++i) {
		s += s[1] + 2;
	}
	if (len + 2 + s < start + swin->size) {
		*s = bold;
		memcpy(s + 1, str, len + 1);
		swin->used = s + len + 2 - start;
	}
	HUnlock(swin->text);
	statupdate(win, false);
}

/* scroll the status dialog
 */
static void statscroll(na_win *win, short rows)
{
	RgnHandle rgn;
	
	if ((rgn = NewRgn()) != NULL) {
		SetCtlValue(swin->sb, swin->row += rows);
		ScrollRect(&swin->urect, 0, - swin->height * rows, rgn);
		EraseRgn(rgn);
		DisposeRgn(rgn);
	}
	statupdate(win, false);
}

/* scroll bar procedure
 */
static pascal void statscollbar(ControlHandle ctrlh, short part)
{
	na_win *win = (na_win *) GetCRefCon(ctrlh);
	short max, new, page;
	
	max = GetCtlMax(ctrlh);
	page = (swin->urect.bottom - swin->urect.top) / swin->height - 1;
	switch (part) {
		case inUpButton:
			page = 1;
			/* fall through */
		case inPageUp:
			if (swin->row > 0) {
				statscroll(win, - (swin->row < page ? swin->row : page));
			}
			break;
		case inDownButton:
			page = 1;
			/* fall through */
		case inPageDown:
			if (swin->row < max) {
				statscroll(win, max - swin->row < page ? max - swin->row : page);
			}
			break;
		case inThumb:
			break;
	}
}

/* control procedure for status dialog box
 */
static short statctrl(na_win *win, Point p, short item, short mods, ControlHandle ctrlh)
{
	short value;
	
	if (ctrlh == swin->sb) {
		ctrlh = swin->sb;
		if (item != inThumb) {
			SetCRefCon(ctrlh, (long) win);
			TrackControl(ctrlh, p, statscollbar);
		} else {
			TrackControl(ctrlh, p, nil);
			value = GetCtlValue(ctrlh);
			if (value != swin->row) statscroll(win, value - swin->row);
		}
	} else if (item == iOk) {
		return (NA_REQCLOSE);
	}
	
	return (NA_NOTPROCESSED);
}

/* close procedure for status dialog box
 */
static short statclose(na_win *win)
{
	DisposeRgn(swin->urgn);
	DisposHandle(swin->text);
	DisposeControl(swin->sb);
	
	return (alldone(win));
}

/* init procedure for status dialog box
 */
static short statinit(na_win *win, long *data)
{
	Rect tmpr;
	FontInfo finfo;
	
	/* disable OK button while working */
	NAhiliteDItem(win->pwin, iOk, 255);
	
	/* set up status text area & font */
	if ((swin->urgn = NewRgn()) == NULL) return (NA_CLOSED);
	TextFont(geneva);
	TextSize(9);
	GetFontInfo(&finfo);
	swin->ascent = finfo.ascent;
	swin->height = finfo.ascent + finfo.descent + finfo.leading;
	NAgetDRect(win->pwin, iStatus, &swin->frect);
	swin->urect = swin->frect;
	InsetRect(&swin->urect, 2, 
		2 + ((swin->urect.bottom - swin->urect.top - 4) % swin->height) / 2);
	RectRgn(swin->urgn, &swin->urect);

	/* set up text storage */
	if ((swin->text = NewHandle(swin->size = 1024)) == NULL) {
		DisposeRgn(swin->urgn);
		return (NA_CLOSED);
	}
	**(char **)swin->text = '\0';
	
	/* set up scrollbar */
	NAgetDRect(win->pwin, iStatScroll, &tmpr);
	swin->sb = NewControl(win->pwin, &tmpr, "\p", true, 0, 0, 0, scrollBarProc, 0);
	if (!swin->sb) {
		DisposeRgn(swin->urgn);
		DisposHandle(swin->text);
		return (NA_CLOSED);
	}
	
	/* set up procedures */
	win->closep = statclose;
	win->ctrlp = statctrl;
	win->updatep = statupdate;
	
	/* keep window locked until decoding is done */
	++win->locks;
	curstatwin = win;
	
	return (NA_NOTPROCESSED);
}

/* process the files in the file list
 */
static void do_decodefiles(na_win *win)
{
	int count = dwin->count;
	filelist *fl;
	FILE *dfile, *tmpf;
	extern long _ftype, _fcreator;
	long ticks;
	int result;
	
	MapTypeCreator("text/plain", 0);
	SetCursor(&watch);
	if (NAwindow(0, NA_DIALOGWINDOW | NA_TITLEBAR | NA_DEFBUTTON | NA_USERESOURCE
		| NA_CLOSEBOX | NA_HASCONTROLS,
		0, dstatDLOG, 0, sizeof (statuswin), statinit) == NA_CLOSED) {
		warn("Not enough memory to decode");
		return;
	}
	MoveHHi((Handle) dwin->hflist);
	HLock((Handle) dwin->hflist);
	fl = *dwin->hflist;
	tmpf = tmpfile();
	while (count--) {
		stattext(fl->fname, 1);
		didchat = 0;
		if (dfile = Macopen(tmpf, fl->fname, fl->vRefNum, fl->dirID, 0, 0, 1)) {
			result = handleMessage(part_init(dfile), "text/plain",
				0, (*mpack_prefs)->extract_text);
			if (result != 0 || didchat <= 0) {
				if (didchat < 0) {
					chat("Decoding cancelled");
				} else {
					chat("Found nothing to decode");
				}
			}
			fclose(dfile);
		} else {
			chat("Couldn't find source file");
		}
		++fl;
	}
	fclose(tmpf);
	HUnlock((Handle) dwin->hflist);
	NAhiliteDItem(curstatwin->pwin, iOk, 0);
	NAunlockWindow(curstatwin);
	curstatwin = NULL;
	SetCursor(&arrow);
	DisposHandle((Handle) dwin->hflist);
}

/* return non-zero if two filenames have the same prefix
 */
static int fprefixMatch(char *base, PCstr *match)
{
	PCstr temp[257];
	char *scan;
	short prefixlen;
	
	PtoPCstrcpy(temp, base);
	scan = C(temp) + PCstrlen(temp) - 1;
	while (isdigit(*scan) && scan > C(temp)) --scan;
	prefixlen = scan - C(temp) + 1;
	if (strncmp(C(temp), C(match), prefixlen)) return (0);
	scan = C(match) + prefixlen;
	while (isdigit(*scan)) ++scan;
	
	return (!*scan);
}

/* do the add of a file to a list
 */
static void addit(listwin *dw, short vRefNum, long dirID, char *fname)
{
	long size = GetHandleSize((Handle) dw->hflist) / sizeof (filelist);
	filelist *fl;
	char *bp;
	Cell c;
	int i;
	PCstr fbuf[42];

	if (size == dw->count) {
		SetHandleSize((Handle) dw->hflist, (++size * sizeof (filelist)));
		if (MemError() != noErr) return;
	}
	MoveHHi((Handle) dw->hflist);
	HLock((Handle) dw->hflist);
	fl = *dw->hflist;
	for (i = dw->count; i; --i, ++fl) {
		if (fl->vRefNum == vRefNum && fl->dirID == dirID &&
			*fl->fname == *fname && !strncmp(C(fl->fname), C(fname), *fl->fname)) {
			break;
		}
	}
	if (!i) {
		fl->vRefNum = vRefNum;
		fl->dirID = dirID;
		PtoPCstrcpy(fl->fname, fname);
		SetPt(&c, 0, dw->count);
		LAddRow(1, ++dw->count, dw->l);
		LSetCell((Ptr) C(fname), (short) Pstrlen(fname), c, dw->l);
	}
	HUnlock((Handle) dw->hflist);
}

/* add file set to file list
 */
static void addfile(dw, fspec)
	listwin *dw;
	FSSpec *fspec;
{
	CInfoPBRec cipbr;
	HFileInfo *fpb = (HFileInfo *)&cipbr;
	PCstr fbuf[42];
	short idx, foundone = 0;
	long procid;
	
	/* remove working directory stuff */
	if (fspec->parID == 0) {
		GetWDInfo(fspec->vRefNum, &fspec->vRefNum, &fspec->parID, &procid);
	}

	/* loop through directory */
	for (idx = 1; ; ++idx) {
		fpb->ioVRefNum = fspec->vRefNum;
		fpb->ioNamePtr = P(fbuf);
		fpb->ioDirID = fspec->parID;
		fpb->ioFDirIndex = idx;
		if (PBGetCatInfoSync(&cipbr)) break;
		SetClen(fbuf);
		
		if (!(fpb->ioFlAttrib & 16) && fprefixMatch((char *)fspec->name, fbuf)) {
			addit(dw, fspec->vRefNum, fspec->parID, (char *) P(fbuf));
			foundone = 1;
		}
	}
	if (!foundone) {
		addit(dw, fspec->vRefNum, fspec->parID, (char *) fspec->name);
	}
}

/* remove file from file list
 */
static void removefile(dw)
	listwin *dw;
{
	filelist *fl;
	int count;
	Cell c;
	
	c.h = c.v = 0;
	if (LGetSelect(TRUE, &c, dw->l)) {
		MoveHHi((Handle) dw->hflist);
		HLock((Handle) dw->hflist);
		fl = *dw->hflist + c.v;
		count = dw->count - c.v;
		while (--count) {
			fl[0] = fl[1];
			++fl;
		}
		HUnlock((Handle) dw->hflist);		
		--dw->count;
		LDelRow(1, c.v, dw->l);
	}
}

/* close list window
 */
static short listclose(win)
	na_win *win;
{
	LDispose(dwin->l);
	
	return (alldone(win));
}

/* mouse procedure
 */
static short listmouse(na_win *win, Point p, short type, short mods)
{
	Cell c;
	
	if (!(type & 1)) {
		LClick(p, mods, dwin->l);
		c.h = c.v = 0;
		NAhiliteDItem((DialogPtr)win->pwin, iRemove, LGetSelect(TRUE, &c, dwin->l) ? 0 : 255);
	}
	
	return (NA_NOTPROCESSED);
}

/* control procedure
 */
static short listctrl(na_win *win, Point p, short item, short mods, ControlHandle ctrlh)
{
	StandardFileReply reply;

	switch (item) {
		case iAdd:
			NAgetFile(NULL, 1, textList, &reply);
			if (reply.sfGood) {
				if (!dwin->count) {
					NAhiliteDItem((DialogPtr)win->pwin, iOk, 0);
				}
				addfile(dwin, &reply.sfFile);
			}
			return (NA_PROCESSED);
			
		case iRemove:
			removefile(dwin);
			NAhiliteDItem((DialogPtr)win->pwin, iRemove, 255);
			if (!dwin->count) {
				NAhiliteDItem((DialogPtr)win->pwin, iOk, 255);
			}
			return (NA_PROCESSED);

		case iOk:
			win->afterp = do_decodefiles;
		case iCancel:
			return (NA_REQCLOSE);
	}
	
	return (NA_NOTPROCESSED);
}

/* update the list window
 */
static short listupdate(na_win *win, Boolean resize)
{
	Rect r;
	
	NAgetDRect(win->pwin, iFileList, &r);
	FrameRect(&r);
	LUpdate(win->pwin->visRgn, dwin->l);
	
	return (NA_NOTPROCESSED);
}

/* initialize the list window
 */
static short listinit(win, data)
	na_win *win;
	long *data;
{
	FSSpec *fspec = (FSSpec *) data;
	Rect r, zrect;
	Point p;
	Handle hand;
	short type;
	
	GetDItem((DialogPtr)win->pwin, iFileList, &type, &hand, &r);
	InsetRect(&r, 1, 1);
	zrect.top = zrect.bottom = zrect.left = p.h = p.v = 0;\
	zrect.right = 1;
	dwin->l = LNew(&r, &zrect, p, 0, win->pwin, 0, 0, 0, 1);
	if (!dwin->l) return (NA_CLOSED);
	(*dwin->l)->selFlags = lOnlyOne;
	dwin->hflist = (filelist **) NewHandle(sizeof (filelist));
	if (!dwin->hflist) {
		LDispose(dwin->l);
		return (NA_CLOSED);
	}
	dwin->count = 0;
	addfile(dwin, fspec);
	win->closep = listclose;
	win->updatep = listupdate;
	win->ctrlp = listctrl;
	win->mousep = listmouse;
	win->type = DECODELIST;
	NAhiliteDItem((DialogPtr)win->pwin, iRemove, 255);
	ShowWindow(win->pwin);
	LDoDraw(TRUE, dwin->l);
	
	return (NA_NOTPROCESSED);
}

/* Decode procedure: first get a file, then open decode window
 */
static void do_decode(FSSpec *fspec)
{
	StandardFileReply infile;
	na_win **wh, *wp;
	
	if (!fspec) {
		NAgetFile(NULL, 1, textList, &infile);
		if (!infile.sfGood) return;
		fspec = &infile.sfFile;
	} else {
		/* file supplied by drag & drop, look for existing decode window: */
		for (wh = NAhead; wh && (*wh)->type != DECODELIST; wh = (*wh)->next);
		if (wh && (wp = NAlockWindow(wh)) != NULL) {
			addfile((listwin *) wp, fspec);
			NAunlockWindow(wp);
			return;
		}
	}
	NAwindow(0, NA_DIALOGWINDOW | NA_USERESOURCE | NA_DEFBUTTON |
		NA_HASCONTROLS | NA_CLOSEBOX, NULL, decodeDLOG, (long *) fspec,
		sizeof (listwin), listinit);
}

/* Map MIME type to/from Macintosh file types
 */
void MapTypeCreator(char *contenttype, OSType type)
{
	extern long _ftype, _fcreator;
	PCstr tstr[257];
	Handle h;
	ICAttr attr;
	long size = 0;
	Ptr map;
	ICMapEntry *ment;
	unsigned char *scan, *end, *pstr;
	short mapcount, i, foundit = 0;
	OSType temp;
	
	if (!type) CtoPCstrncpy(tstr, contenttype, 255);
	
	/* first try a lookup via Internet Config */
	if (icinst && ICBegin(icinst, icReadOnlyPerm) == noErr) {
		if (ICGetPref(icinst, kICMapping, &attr, nil, &size) == noErr
			&& size > 0 && (map = NewPtr(size)) != nil) {
			if (ICGetPref(icinst, kICMapping, &attr, map, &size) == noErr) {
				scan = (unsigned char *) map;
				end = scan + size;
				while (scan < end) {
					ment = (ICMapEntry *) scan;
					pstr = scan + ment->fixed_length;
					scan += ment->total_length;
					if (type && ment->file_type != type) continue;
					pstr += *pstr + 1; /* skip over extension */
					pstr += *pstr + 1; /* skip over creator app name */
					pstr += *pstr + 1; /* skip over post app name */
					if (type) {
						PtoPCstrcpy((PCstr *) contenttype, (char *) pstr);
						foundit = 1;
						break;
					} else if (EqualString(P(tstr), pstr, false, true)) {
						_ftype = ment->file_type;
						_fcreator = ment->file_creator;
						foundit = 1;
						break;
					}
				}
			}
			DisposPtr(map);
		}
		ICEnd(icinst);
	}
	
	/* if we didn't find it, try our quick&dirty mappings */
	if (!foundit) {
		if (type) {
			mapcount = CountResources('TyCr');
			for (i = 1; i <= mapcount; ++i) {
				h = GetIndResource('TyCr', i);
				if (h && **(OSType **)h == type) {
					GetResInfo(h, &i, &temp, P(contenttype));
					if (ResError() == noErr) break;
				}
			}
			SetClen((PCstr *) contenttype);
		} else {
			h = GetNamedResource('TyCr', P(tstr));
			if (h) {
				_ftype = (*(OSType **)h)[0];
				_fcreator = (*(OSType **)h)[1];
			} else {
				_ftype = '????';
				_fcreator = 'mPAK';
			}
		}
	}
}

/* get internet config string prefs
 */
static short getICprefs(na_win *win, PCstr *eaddr, PCstr *smtphost)
{
	char *scan, *end;
	ICAttr attr;
	long size;
	ICError err = noErr;
	
	*C(eaddr) = '\0';
	SetPlen(eaddr);
	*C(smtphost) = '\0';
	SetPlen(smtphost);
	if (icinst && ICBegin(icinst, icReadOnlyPerm) == noErr) {
		size = 256;
		if (ICGetPref(icinst, kICEmail, &attr, (Ptr) eaddr, &size) == noErr
			&& win && (attr & ICattr_locked_mask)) {
			NAenableDItem(win->pwin, iEmailAddr, 0);
		}
		SetClen(eaddr);
		size = 256;
		if (ICGetPref(icinst, kICSMTPHost, &attr, (Ptr) smtphost, &size) == noErr
			&& win && (attr & ICattr_locked_mask)) {
			NAenableDItem(win->pwin, iMailServer, 0);
		}
		SetClen(smtphost);
		ICEnd(icinst);
	} else {
		HLock((Handle) mpack_prefs);
		end = (char *) (*mpack_prefs) + GetHandleSize((Handle) mpack_prefs);
		scan = (*mpack_prefs)->internet_host;
		while (scan < end && *scan++);
		if (scan < end) CtoPCstrcpy(eaddr, scan);
		while (scan < end && *scan++);
		if (scan < end) CtoPCstrcpy(smtphost, scan);
		HUnlock((Handle) mpack_prefs);
	}
}

/* copy desc file, with word-wrap
 */
static short copydesc(FILE *out, TEHandle hTE)
{
	char c;
	short i, count, word, col, reset;
	char **htxt;
	
	count = (*hTE)->teLength;
	htxt = (char **) (*hTE)->hText;
	for (i = word = col = 0; i < count; ++i) {
		c = (*htxt)[i];
		reset = i - word == 80 || c == '\r';
		if (reset || c == ' ') {
			while (word < i) putc((*htxt)[word], out), ++word;
		}
		if (reset || ++col == 80) {
			putc('\r', out);
			c = (*htxt)[word];
			if (c == ' ' || c == '\r') ++word;
			col = 0;
		}
	}
	while (word < i) putc((*htxt)[word], out), ++word;
	rewind(out);
}

/* start up MacTCP callback
 */
static void mytcpinit(short status)
{
	static DialogPtr dialog = NULL;
	GrafPtr save;
	Rect box;
	
	if (status < 0) {
		tcpstart = -1;
	} else if (status == 0) {
		tcpstart = 1;
	} else {
		if (!dialog && status < 100) {
			dialog = GetNewDialog(progDLOG, nil, (WindowPtr) -1);
			NAsetIText(dialog, iWorkText, "\pWaiting for MacTCP to finish.  Press \021. to stop.");
		}
		if (dialog) {
			GetPort(&save);
			SetPort(dialog);
			NAgetDRect(dialog, iProgress, &box);
			FrameRect(&box);
			InsetRect(&box, 1, 1);
			box.right = box.left + (box.right - box.left) * status / 100;
			FillRect(&box, qd.dkGray);
			SetPort(save);
			if (status == 100) {
				DisposDialog(dialog);
				dialog = NULL;
			}
		}
	}
}

/* update the progress bar
 */
static short progressupdate(na_win *win, Boolean newsize)
{
	Rect box;
	
	if (prwin->percent >= 0) {
		NAgetDRect(win->pwin, iProgress, &box);
		FrameRect(&box);
		InsetRect(&box, 1, 1);
		if (prwin->percent) {
			box.right = box.left + (box.right - box.left) * prwin->percent / 100;
			FillRect(&box, qd.dkGray);
		} else {
			EraseRect(&box);
		}
	}
	
	return (NA_NOTPROCESSED);
}

/* handle the cancel button
 */
static short progressctrl(na_win *win, Point p, short item, short mods,
	ControlHandle ctrlh)
{
	return (item == iCancel ? NA_REQCLOSE : NA_NOTPROCESSED);
}

/* close progress window
 */
static short progressclose(na_win *win)
{
	NAmodalMenus(0);
	
	return (NA_CLOSED);
}

/* make/go directory under prefs and return directory number
 */
static OSErr prefsubdir(PCstr *name, long *dirID)
{
	CInfoPBRec cipbr;
	DirInfo *dpb = &cipbr.dirInfo;
	long subdir, dir;
	short vref = pfolder->fspec.vRefNum;
	OSErr err;
	
	err = DirCreate(vref, dir = pfolder->fspec.parID, P(name), &subdir);
	if (err == dupFNErr) {
		dpb->ioVRefNum = vref;
		dpb->ioNamePtr = P(name);
		dpb->ioDrDirID = dir;
		dpb->ioFDirIndex = 0;
		if ((err = PBGetCatInfoSync(&cipbr)) != noErr) return (err);
		subdir = dpb->ioDrDirID;
	} else if (err != noErr) {
		return (err);
	}
	*dirID = subdir;
	
	return (noErr);
}

/* smtp status task
 */
static void smtpstat(void *wh, short code, short err, long num, char *errstr)
{
	na_win *win, **winh;
	char msg[256];
	OSErr oserr = noErr;
	
	/* verify win is valid */
	for (winh = NAhead; winh && winh != wh; winh = (*winh)->next);
	if (!winh) return;
	
	/* handle SMTP callback */
	win = NAlockWindow((na_win **) wh);
	if (code == NASMTP_progress) {
		prwin->percent = err;
		progressupdate(win, false);
	} else if (code == NASMTP_badaddr) {
		sprintf(msg, "Invalid address: <%s>.  Email will be sent to valid recipients.",
			errstr);
		yell(msg);
	} else {
		switch (code) {
			case NASMTP_nomem:
				yell("Not enough memory to send email");
				break;
			case NASMTP_tcpfail:
				yell("Failed to connect to mail host");
				break;
			case NASMTP_temperr:
			case NASMTP_permerr:
				sprintf(msg, "Delivery failed: %s", errstr);
				yell(msg);
				break;
			default:
				yell("Mail delivery failed.");
			case NASMTP_completed:
				break;
		}
		mwin->sending = false;
		oserr = HDelete(mwin->fspec.vRefNum, mwin->fspec.parID, mwin->fspec.name);
	}
	if (oserr != noErr && oserr != fnfErr) {
		if (mwin->remaining) ++mwin->cpb.hFileInfo.ioFDirIndex;
		yell("Unable to remove temporary email file.");
	}
	NAunlockWindowh((na_win **) wh, win);
}

/* Get the email hostname
 */
static void mailhost(void *user, na_tcp s, short status, long size, char *data)
{
	struct mpack_preferences *mp;
	char *ihost;
	na_win *win, **winh;
	long len, oldsize;
	
	/* first make sure our window still exists */
	for (winh = NAhead; winh && winh != user; winh = (*winh)->next);
	if (!winh) return;
	win = NAlockWindow(winh);
	
	/* check for errors */
	if (status != NATCP_connect) {
		warn("Failed to get hostname from MacTCP");
	} else {
		mwin->gothost = true;
		if (data[size - 1] == '.') --size;
		
		/* update internet_host preference */
		len = strlen((*mpack_prefs)->internet_host);
		oldsize = GetHandleSize((Handle) mpack_prefs);
		if (len < size) {
			SetHandleSize((Handle) mpack_prefs, oldsize + (size - len));
			if (MemError() != noErr) return;
		}
		HLock((Handle) mpack_prefs);
		mp = *mpack_prefs;
		ihost = mp->internet_host;
		memmove(ihost + size + 1, ihost + len + 1,
			oldsize - len - 1 - ((char *) ihost - (char *) mp));
		memcpy(ihost, data, size);
		ihost[size] = '\0';
		HUnlock((Handle) mpack_prefs);
	}
	NAunlockWindowh(winh, win);
}

/* clean up mail task
 */
static short mailclose(na_win *win)
{
	if (mwin->dfile != NULL) fclose(mwin->dfile);
	if (mwin->envelope) DisposeHandle(mwin->envelope);
	if (mwin->headers) DisposeHandle(mwin->headers);
	NAmodalMenus(0);
	
	return (alldone(win));
}

/* send email
 */
static short mailtask(na_win *win)
{
	short vrefnum, encoding, refnum, result;
	long procid;
	FILE *tmpf, *fp, *resfork;
	OSErr err;
	CInfoPBRec cipbr;
	HFileInfo *fpb = (HFileInfo *)&cipbr;
	PCstr tstr[257], mtype[257], fname[257];
	extern long _ftype, _fcreator;

	switch (mwin->state) {
		case MS_MACTCP:
			if (tcpstart < 0) {
				yell("Couldn't find MacTCP");
				return (NA_REQCLOSE);
			}
			if (tcpstart == 0) break;
			++mwin->state;
			NAsetIText(win->pwin, iWorkText, "\pGetting Hostname");
			mwin->gothost = false;
			NATCPgethost(mailhost, (void *) GetWRefCon(win->pwin));
			/* fall through */
		case MS_GETHOST:
			if (!mwin->gothost) break;
			++mwin->state;
			/* fall through */
		case MS_ENCODE:
			NAsetIText(win->pwin, iWorkText, "\pEncoding file");
			
			/* get temp output filename for email */
			if (mwin->useemail) {
				mwin->ofile.vRefNum = pfolder->fspec.vRefNum;
				memcpy(mwin->ofile.name, "\pemail", 6);
				if (prefsubdir("\poutgoing-email", &mwin->ofile.parID) != noErr) {
					yell("Failed to write encoded file");
					return (NA_REQCLOSE);
				}
			}
			
			/* set file type */
			SetCursor(&watch);
			MapTypeCreator((char *) mtype, mwin->ftype);
			
			/* Determine the correct encoding */
			encoding = (*mpack_prefs)->encoding;
			fpb->ioVRefNum = mwin->fspec.vRefNum;
			fpb->ioNamePtr = mwin->fspec.name;
			fpb->ioDirID = mwin->fspec.parID;
			fpb->ioFDirIndex = 0;
			if (PBGetCatInfoSync(&cipbr) != noErr) {
				SetCursor(&arrow);
				yell("File disappeared before being encoded!");
				return (NA_REQCLOSE);
			}
			if (encoding == EN_AUTO) {
				encoding = EN_DOUBLE;
				if (!fpb->ioFlRLgLen && *mtype != '\0') encoding = EN_DATA;
			}
			if (!fpb->ioFlLgLen) encoding = EN_SINGLE;
			
			/* do applesingle/appledouble encoding */
			tmpf = tmpfile();
			fp = Macopen(tmpf, mwin->fspec.name, mwin->fspec.vRefNum, mwin->fspec.parID,
				strcmp(C(mtype), "text/plain") ? 1 : 0, 0, fsRdPerm);
			if (!fp) {
				fclose(tmpf);
				SetCursor(&arrow);
				yell("Couldn't save encoded file");
				return (NA_REQCLOSE);
			}
			if (encoding == EN_DATA) {
				fclose(tmpf);
				tmpf = NULL;
			} else {
				/* open resource fork & output file for applesingle/double encoding */
				resfork = Macopen(tmpf, mwin->fspec.name, mwin->fspec.vRefNum,
					mwin->fspec.parID, 1, 1, 1);
				if (encode_applefile(tmpf, fpb, resfork, encoding == EN_SINGLE ? fp : NULL) < 0) {
					SetCursor(&arrow);
					yell("Couldn't save encoded file");
					return (NA_REQCLOSE);
				}
				rewind(tmpf);
				if (encoding == EN_SINGLE) {
					fp = tmpf;
					tmpf = NULL;
					strcpy(C(mtype), "application/applefile");
					SetPlen(mtype);
				}
			}
			
			/* generate output files */
			_fcreator = 'mPAK';
			_ftype = 'TEXT';
			GetVol(0, &vrefnum);
			err = OpenWD(mwin->ofile.vRefNum, mwin->ofile.parID, 0, &refnum);
			SetVol(0, err == noErr ? refnum : mwin->ofile.vRefNum);
			PtoPCstrcpy(tstr, (char *) mwin->ofile.name);
			PtoPCstrcpy(fname, (char *) mwin->fspec.name);
			result = encode(fp, tmpf, C(fname), mwin->dfile, C(mwin->subj), NULL,
				mwin->partsize, PCstrlen(mtype) ? C(mtype) : NULL, C(tstr));
			if (err == noErr) CloseWD(refnum);
			SetVol(0, vrefnum);
			if (tmpf) fclose(tmpf);
			fclose(fp);
			if (mwin->dfile) {
				fclose(mwin->dfile);
				mwin->dfile = NULL;
			}
			SetCursor(&arrow);
			if (!mwin->useemail) return (NA_REQCLOSE);
			prwin->percent = 0;
			progressupdate(win, false);
			++mwin->state;
			
			/* count files */
			mwin->cpb.dirInfo.ioVRefNum = mwin->ofile.vRefNum;
			mwin->cpb.dirInfo.ioDrDirID = mwin->dirID = mwin->ofile.parID;
			mwin->cpb.dirInfo.ioFDirIndex = -1;
			if (PBGetCatInfoSync(&mwin->cpb) != noErr) {
				return (NA_CLOSED);
			}
			mwin->remaining = mwin->cpb.dirInfo.ioDrNmFls;
			mwin->cpb.dirInfo.ioFDirIndex = 1;
			/* fall through */
		case MS_SENDING:
			if (mwin->sending) break;
			if (!mwin->remaining) return (NA_REQCLOSE);
			sprintf(C(tstr), "Email parts remaining to submit: %d", mwin->remaining--);
			SetPlen(tstr);
			NAsetIText(win->pwin, iWorkText, tstr);
			prwin->percent = 0;
			progressupdate(win, false);
			mwin->cpb.hFileInfo.ioDirID = mwin->dirID;
			mwin->cpb.hFileInfo.ioNamePtr = (StringPtr) &mwin->fspec.name;
			if (PBGetCatInfoSync(&mwin->cpb) != noErr) {
				yell("Email disappeared before submission!");
				return (NA_REQCLOSE);
			}
			mwin->sending = true;
			mwin->fspec.vRefNum = mwin->cpb.hFileInfo.ioVRefNum;
			mwin->fspec.parID = mwin->dirID;
			NASMTPsubmit(smtpstat, C(mwin->server), &mwin->fspec,
				mwin->headers, mwin->envelope,
				NASMTP_crtrans, (void *) GetWRefCon(win->pwin));
			break;
	}
	
	return (NA_NOTPROCESSED);
}

/* Following routine stolen from Mark Crispin's c-client library:
 *
 * Write current time in RFC 822 format
 * Accepts: destination string
 *
 * This depends upon the ReadLocation() call in System 7 and the
 * user properly setting his location/timezone in the Map control
 * panel.
 * 2/95 - I added support for dlsDelta & compatibility checking
 */
void rfc822_date(char *string)
{
	long tz, tzm;
	time_t ti = time (0);
	struct tm *t = localtime (&ti);
	MachineLocation loc;
	
	/* output date */
	strcpy(string, "Date: ");
	string += 6;
	strftime (string,1024,"%a, %d %b %Y %H:%M:%S ",t);
	/* now output time zone, if we can get it */
	tz = 0;
	if (Gestalt(gestaltScriptMgrVersion, &tz) == noErr && tz >= 200) {
		ReadLocation(&loc);		/* get location/timezone */
		/* get sign-extended time zone */
		tz = (loc.gmtFlags.gmtDelta & 0x00ffffff) |
			((loc.gmtFlags.gmtDelta & 0x00800000) ? 0xff000000 : 0);
		tz /= 60;			/* get timezone in minutes */
		tzm = tz % 60;		/* get minutes from the hour */
		sprintf (string += strlen(string),"%+03ld%02ld",
			tz/60,tzm >= 0 ? tzm : -tzm);
		if (!tzm && tz <= -240 && tz >= -660) {
			string += strlen(string);
			if (loc.gmtFlags.dlsDelta & 0x80) {
				sprintf(string, " (%cDT)", "AECMPYHB"[- (tz / 60) - 3]);
			} else {
				sprintf(string, " (%cST)", "AECMPYHB"[- (tz / 60) - 4]);
			}
		}
	} else {
		sprintf(string + strlen(string), "+0000 (Local Time Zone Unknown)");
	}
}

/* init mail sending
 */
static short mailinit(na_win *win, long *data)
{
	encodewin *ew = (encodewin *) data;
	WindowPtr pwin = ew->w.winp.pwin;
	ControlHandle ctrlh;
	PCstr tstr[257], email[257];
	
	/* copy values from encode window */
	NAgetIText(pwin, iSubj, mwin->subj);
	NAgetIText(pwin, iEmailto, email);
	mwin->partsize = ew->partsize;
	mwin->useemail = ew->useemail;
	mwin->fspec = ew->fspec;
	mwin->ftype = ew->ftype;
	mwin->ofile = ew->ofile;

	/* copy desc file */
	mwin->dfile = NULL;
	if ((*ew->w.hTE)->teLength && (mwin->dfile = tmpfile()) != NULL) {
		copydesc(mwin->dfile, ew->w.hTE);
	}
	
	/* set procedures */
	win->taskp = mailtask;
	win->updatep = progressupdate;
	win->ctrlp = progressctrl;
	win->closep = mailclose;
	
	/* Customize Progress window, set up envelope & headers for email */
	prwin->percent = -1;
	NAgetDHandle(win->pwin, iCancel, &ctrlh);
	SetCTitle(ctrlh, "\pStop");
	NAmodalMenus(1);
	if (!mwin->useemail) {
		mwin->state = MS_ENCODE;
	} else {
		if (!tcpstart) NATCPinit(mytcpinit);
		NAsetIText(win->pwin, iWorkText, "\pLooking for MacTCP");
		mwin->state = MS_MACTCP;

		/* create envelope, get server */
		getICprefs(NULL, tstr, mwin->server);
		if (PtrToHand(C(tstr), &mwin->envelope, PCstrlen(tstr) + 1) != noErr
			|| PtrAndHand(C(email), mwin->envelope, PCstrlen(email) + 1) != noErr) {
			if (mwin->envelope) DisposeHandle(mwin->envelope);
			return (NA_CLOSED);
		}
	
		/* create headers */
		if ((mwin->headers = NewHandle(1024)) == NULL) {
			DisposeHandle(mwin->envelope);
			return (NA_CLOSED);
		}
		HLock(mwin->headers);
		rfc822_date((char *) *mwin->headers);
		sprintf((char *) (*mwin->headers) + strlen((char *) (*mwin->headers)),
			"\r\nFrom: %s\r\nTo: %s\r\n", C(tstr), C(email));
		HUnlock(mwin->headers);
		SetHandleSize(mwin->headers, strlen((char *) (*mwin->headers)));
	}

	return (NA_NOTPROCESSED);
}

/* update the encode window
 */
static short encodeupdate(na_win *win, Boolean newsize)
{
	Rect btmp;
	ControlHandle ctrlh;
	
	/* draw double-line */
	NAgetDRect(win->pwin, iBar, &btmp);
	FrameRect(&btmp);
	
	/* draw disabled edittext boxes */
	NAgetDHandle(win->pwin, iLimit, &ctrlh);
	if (!GetCtlValue(ctrlh)) {
		NAhiliteDItem(win->pwin, iPartLimit, 255);
	}
	if (NAradioGet(win->pwin, iEmail, iSavefile) == iSavefile) {
		NAhiliteDItem(win->pwin, iEmailto, 255);
	}
	
	return (NATEupdatep(win, newsize));
}

/* select desc text
 */
static short seldesctext(na_win *win)
{
	win->activep = NATEactivep;
	win->idlep = NATEidlep;
	NATEactivep(win, true);
	ewin->nateon = true;
	SelIText(win->pwin, iDescEdit, 0, 0);
	TESetSelect(32767, 32767, twin->hTE);
}

/* encode control proc
 */
static short encodectrl(na_win *win, Point p, short item,
	short mods, ControlHandle ctrlh)
{
	short value;
	DialogPeek dpeek = (DialogPeek) win->pwin;
	char *scan;
	Boolean good;
	StandardFileReply reply;
	PCstr tstr[257];
	
	if (ctrlh == twin->vctrl) {
		return (NATEctrlp(win, p, item, mods, ctrlh));
	}
	switch (item) {
		case iOk:
			/* get part size */
			ewin->partsize = 0;
			NAgetDHandle(win->pwin, iLimit, &ctrlh);
			if (GetCtlValue(ctrlh)) {
				NAgetIText(win->pwin, iPartLimit, tstr);
				ewin->partsize = atol(C(tstr)) * 1000;
			}
			NAgetIText(win->pwin, iEmailto, tstr);
			ewin->useemail = NAradioGet(win->pwin, iEmail, iSavefile) == iEmail;
			if (ewin->useemail) {
				/* verify email address */
				if (!strchr(C(tstr), '@')) {
					yell("Invalid Email address, please re-enter");
					SelIText(win->pwin, iEmailto, 0, 32767);
					break;
				}
			} else {
				/* get output filename */
				PtoPCstrcpy(tstr, (char *) ewin->fspec.name);
				if (PCstrlen(tstr) > 23) {
					PCstrlen(tstr) = 23;
					SetClen(tstr);
				}
				strcat(C(tstr), ".mime");
				SetPlen(tstr);
				do {
					NAputFile(ewin->partsize ? "\pPart prefix" : "\pEmail file:",
						tstr, &reply);
					good = true;
					if (reply.sfGood
						&& EqualString(reply.sfFile.name,
						ewin->fspec.name, true, false)) {
						good = false;
						yell("The output filename must be different from the input filename");
					}
				} while (!good);
				if (!reply.sfGood) break;
				ewin->ofile = reply.sfFile;
			}
			if (NAwindow(0, NA_DIALOGWINDOW | NA_TITLEBAR | NA_HASTASK
				| NA_USERESOURCE | NA_MODAL, NULL, progDLOG,
				(long *) win, sizeof (mailwin), mailinit) == NA_CLOSED) {
				warn("Not enough memory to proceed");
				break;
			}
		case iCancel:
			return (NA_REQCLOSE);
		case iEmail:
		case iSavefile:
			NAradioSet(win->pwin, iEmail, iSavefile, item);
			NAenableDItem(win->pwin, iEmailto, item == iEmail ? 1 : 0);
			NAhiliteDItem(win->pwin, iEmailto, item == iEmail ? 0 : 255);
			if (item == iEmail || dpeek->editField == iEmailto - 1) {
				SelIText(win->pwin, item == iEmail ? iEmailto : iSubj, 0, 32767);
			}
			break;
		case iLimit:
			SetCtlValue(ctrlh, value = !GetCtlValue(ctrlh));
			NAenableDItem(win->pwin, iPartLimit, value ? 1 : 0);
			NAhiliteDItem(win->pwin, iPartLimit, value ? 0 : 255);
			if (value || dpeek->editField == iPartLimit - 1) {
				SelIText(win->pwin, value ? iPartLimit : iSubj, 0, 32767);
			}
			break;
		case iDescEdit:
		case iSubj:
		case iEmailto:
		case iPartLimit:
			if (!ewin->nateon && dpeek->editField == iDescEdit - 1) {
				seldesctext(win);
			}
			break;
	}
	if (ewin->nateon && dpeek->editField != iDescEdit - 1) {
		win->activep = NULL;
		win->idlep = NULL;
		NATEactivep(win, false);
		ewin->nateon = false;
	}
	
	return (NA_NOTPROCESSED);
}

/* encode key proc
 */
static short encodekey(na_win *win, long c, short mods)
{
	if (!(mods & cmdKey)) {
		if (ewin->nateon && c != '\t' && c != '\n' && c != '\3' && c != '\033') {
			return (NATEkeyp(win, c, mods));
		}
	}
	
	return (NA_NOTPROCESSED);
}

/* menu proc for encode window
 */
static short encodemenu(na_win *win, WORD menu, WORD item)
{
	StandardFileReply descfile;
	MenuHandle mf = NAmenuh(mFile);
	short result = NA_NOTPROCESSED;
	short refnum;
	long size;
	Ptr text;
	Boolean success;
	
	switch (menu) {
		case 0:
			EnableItem(mf, iInsert);
			/* fall through */
		case mEdit:
			result = ewin->nateon ? NATEmenup(win, menu, item)
				: NAdialogMenu(win, menu, item);
			break;
		case mFile:
			if (item != iInsert) break;
			result = NA_PROCESSED;
			NAgetFile(NULL, 1, textList, &descfile);
			if (!descfile.sfGood) break;
			if (HOpen(descfile.sfFile.vRefNum, descfile.sfFile.parID,
				descfile.sfFile.name, fsRdPerm, &refnum) != noErr) {
				warn("Failed to open file");
				break;
			}
			text = NULL;
			success = GetEOF(refnum, &size) == noErr && (text = NewPtr(size)) != NULL
				&& FSRead(refnum, &size, text) == noErr;
			if (success) {
				TEInsert(text, size, twin->hTE);
				TESelView(twin->hTE);
				NATEsetscroll(win, false, (Rect*) NULL, (Rect*) NULL);
			} else {
				warn("Failed to read file");
			}
			if (text) DisposPtr(text);
			FSClose(refnum);
			break;
	}
	if (menu != 0) DisableItem(mf, iInsert);
	
	return (result);
}

/* mouse proc for encode window
 */
static short encodemouse(na_win *win, Point p, short type, short mods)
{
	if (p.v >= twin->topoff && !ewin->nateon) seldesctext(win);
	
	return (NATEmousep(win, p, type, mods));
}

/* close the encode window
 */
static short encodeclose(na_win *win)
{
	NATEclosep(win);
	
	return (NA_CLOSED);
}

/* init the encode window
 */
static short encodeinit(na_win *win, long *data)
{
	StandardFileReply *sf = (StandardFileReply *) data;
	Rect rtmp, btmp;
	FontInfo finfo;
	
	/* copy data */
	ewin->fspec = sf->sfFile;
	ewin->ftype = sf->sfType;
	
	/* set sizing limits */
	NAgetDRect(win->pwin, iBar, &btmp);
	rtmp = win->pwin->portRect;
	win->minw = win->maxw = rtmp.right - rtmp.left;
	win->minh = btmp.bottom + 64;
	twin->topoff = btmp.bottom;
	
	/* init text area */
	TextFont(monaco);
	TextSize(9);
	GetFontInfo(&finfo);
	NATEinit(win, NATE_NOHSCROLL, 80 * finfo.widMax + 2, NULL, 0);
	ewin->nateon = 0;
	TextFont(0);
	TextSize(0);
	
	/* set control values */
	NAradioSet(win->pwin, iEmail, iSavefile, iSavefile);
	if (tcpstart < 0) NAhiliteDItem(win->pwin, iEmail, 255);
	NAenableDItem(win->pwin, iEmailto, 0);
	NAenableDItem(win->pwin, iPartLimit, 0);
	NAsetIText(win->pwin, iSubj, ewin->fspec.name);
	SelIText(win->pwin, iSubj, 0, 32767);
	SetWTitle(win->pwin, ewin->fspec.name);
	ShowWindow(win->pwin);
	
	/* set window procedures */
	win->updatep = encodeupdate;
	win->closep = encodeclose;
	win->keyp = encodekey;
	win->ctrlp = encodectrl;
	win->mousep = encodemouse;
	win->menup = encodemenu;
	win->idlep = NULL;
	win->activep = NULL;
	
	return (NA_NOTPROCESSED);
}

/* Encode procedure: first get a file, then open encode save window
 */
static void do_encode(FSSpec *fspec, OSType ftype)
{
	StandardFileReply infile;

	if (!fspec) {
		NAgetFile(NULL, -1, NULL, &infile);
		if (!infile.sfGood) return;
	} else {
		infile.sfFile = *fspec;
		infile.sfType = ftype;
	}
	NAwindow(NULL, NA_DIALOGWINDOW | NA_TITLEBAR | NA_GROWBOX | NA_USERESOURCE
		| NA_DEFBUTTON | NA_HASCONTROLS,
		NULL, sendDLOG, (long *) &infile, sizeof (encodewin), encodeinit);
}

/* Open a file via drag&drop
 */
static short openfile(short message, FSSpec *fspec, FInfo *finfo)
{	
	if (message != appOpen) return (-1);
	
	/* open file */
	if (finfo->fdType == 'TEXT') {
		do_decode(fspec);
	} else {
		do_encode(fspec, finfo->fdType);
	}
	
	return (0);
}

#define hwinfo ((nate_win *)win)

/* help close procedure
 */
static short helpclose(na_win *win)
{
	helpw = NULL;

	return (NATEclosep(win));
}

/* help window procedure
 */
static short helpwindow(na_win *win, long *data)
{
	Rect		rtemp, vtemp;
	Handle		h, hs;
	long		len;
	TEHandle	hTE;
	
	rtemp = win->pwin->portRect;
	vtemp = rtemp;
	vtemp.right = vtemp.left + (hwinfo->docwidth = 475);
	win->mousep = NATEmousep;
	win->idlep = NATEidlep;
	win->menup = NATEmenup;
	win->activep = NATEactivep;
	win->updatep = NATEupdatep;
	win->ctrlp = NATEctrlp;
	win->closep = helpclose;
	win->cursorRgn = NewRgn();
	hwinfo->vctrl = hwinfo->hctrl = NULL;
	
	TEAutoView(true, hTE = hwinfo->hTE = TEStylNew(&vtemp, &rtemp));
	h = GetResource('TEXT', helpTEXT);
	hs = GetResource('styl', helpSTYL);
	len = GetHandleSize(h);
	HLock(h);
	TEStylInsert(*h, len, (StScrpHandle) hs, hTE);
	HUnlock(h);
	TESetSelect(0, 0, hTE);
	hwinfo->lheight = TEGetHeight((*hTE)->nLines, 0, hTE) / (*hTE)->nLines;
	ShowWindow(helpw = win->pwin);
	
	return (NA_NOTPROCESSED);
}

/* Set the hostname: TCP callback
 */
static void sethost(void *user, na_tcp s, short status, long size, char *data)
{
	PCstr host[65];
	Rect box;
	na_win *win, **winh;
	
	/* first make sure our window still exists */
	for (winh = NAhead; winh && (*winh)->type != PREFWIN; winh = (*winh)->next);
	if (!winh || (*winh)->child != user) return;
	win = NAlockWindow((na_win **) user);
	
	/* check for errors */
	if (status != NATCP_connect) {
		warn("Failed to get hostname from MacTCP");
	} else {
		if (data[size - 1] == '.') --size;
		PCstrlen(host) = size;
		memcpy(C(host), data, size);
		NAsetIText((*winh)->pwin, iHost, host);
		SelIText((*winh)->pwin, iHost, 0, 32767);
	}
	prwin->percent = 100;
	progressupdate(win, false);
	NAunlockWindowh(winh, win);
}

/* if TCP is active, get hostname
 */
static short settask(na_win *win)
{
	if (tcpstart == 0 && !prwin->percent) {
		NAsetIText(win->pwin, iWorkText, "\pLooking for MacTCP");
		prwin->percent = 1;
		progressupdate(win, false);
		NATCPinit(mytcpinit);
	} else if (tcpstart == 1 && prwin->percent < 50) {
		NAsetIText(win->pwin, iWorkText, "\pLooking up Internet hostname");
		prwin->percent = 50;
		progressupdate(win, false);
		NATCPgethost(sethost, (void *) GetWRefCon(win->pwin));
	}
	progressupdate(win, false);
	if (tcpstart == -1) {
		warn("MacTCP not available");
		NAhiliteDItem((*win->parent)->pwin, iSet, 255);
	}
	
	return (tcpstart == -1 || prwin->percent == 100 ? NA_CLOSED : NA_NOTPROCESSED);
}

/* set the Internet host via MacTCP
 */
static short setinit(na_win *win, long *data)
{
	win->taskp = settask;
	win->updatep = progressupdate;
	win->closep = progressclose;
	NAmodalMenus(1);
	
	return (NA_NOTPROCESSED);
}

/* preference control procedure
 */
static short prefsctrl(na_win *win, Point p, short item, short mods, ControlHandle ctrlh)
{
	PCstr tmpstr[257];
	short encoding, extract_text, quit_finished, result = NA_NOTPROCESSED;
	ControlHandle ctrl;
	char *scan, *end;
	short changed, len, i, useic;
	static short prefitem[3] = { iHost, iEmailAddr, iMailServer };
	
	switch (item) {
		case iOk:
			HLock((Handle) mpack_prefs);
			changed = 0;
			encoding = NAradioGet(win->pwin, iAuto, iDouble) - iAuto;
			NAgetDHandle(win->pwin, iTextEncode, &ctrl);
			extract_text = GetCtlValue(ctrl);
			NAgetDHandle(win->pwin, iQuitFinish, &ctrl);
			quit_finished = GetCtlValue(ctrl);
			if (encoding != (*mpack_prefs)->encoding
				|| extract_text != (*mpack_prefs)->extract_text
				|| quit_finished != (*mpack_prefs)->quit_finished) {
				changed = 1;
			}
			if (changed) {
				(*mpack_prefs)->encoding = encoding;
				(*mpack_prefs)->extract_text = extract_text;
				(*mpack_prefs)->quit_finished = quit_finished;
				ChangedResource((Handle) mpack_prefs);
				changed = 0;
			}
			len = 1;
			scan = (*mpack_prefs)->internet_host;
			end = (char *) *mpack_prefs + GetHandleSize((Handle) mpack_prefs);
			for (i = 0; i < 3; ++i) {
				NAgetIText(win->pwin, prefitem[i], P(tmpstr));
				SetClen(tmpstr);
				len += PCstrlen(tmpstr);
				if (scan == end || strcmp(C(tmpstr), scan)) {
					changed = 1;
				}
				while (scan < end && *scan++);
			}
			if (changed) {
				HUnlock((Handle) mpack_prefs);
				/* update the preferences resource */
				SetHandleSize((Handle) mpack_prefs, sizeof (struct mpack_preferences)
					+ len);
				HLock((Handle) mpack_prefs);
				scan = (*mpack_prefs)->internet_host;
				useic = icinst && ICBegin(icinst, icReadWritePerm) == noErr;
				for (i = 0; i < 3; ++i) {
					NAgetIText(win->pwin, prefitem[i], P(tmpstr));
					SetClen(tmpstr);
					strcpy(scan, C(tmpstr));
					scan += PCstrlen(tmpstr) + 1;
					if (i && useic) {
						ICSetPref(icinst, i == 1 ? kICEmail : kICSMTPHost,
							ICattr_no_change, (Ptr) P(tmpstr), PCstrlen(tmpstr) + 1);
					}
				}
				if (useic) ICEnd(icinst);
				ChangedResource((Handle) mpack_prefs);
			}
			HUnlock((Handle) mpack_prefs);
		case iCancel:
			result = NA_REQCLOSE;
			NAmodalMenus(0);
			break;
		case iAuto:
		case iData:
		case iSingle:
		case iDouble:
			NAradioSet(win->pwin, iAuto, iDouble, item);
			break;
		case iTextEncode:
		case iQuitFinish:
			SetCtlValue(ctrlh, !GetCtlValue(ctrlh));
			break;
		case iSet:
			NAwindow(0, NA_DIALOGWINDOW | NA_TITLEBAR | NA_HASTASK | NA_USERESOURCE
				| NA_MODAL | NA_CHILDWINDOW,
				NULL, progDLOG, NULL, sizeof (progresswin), setinit);
			break;
	}
	
	return (result);
}

/* update preferences dialog
 */
static short prefsupdate(na_win *win, Boolean newsize)
{
	Handle hn;
	Rect box;
	short type;
	
	/* draw disabled items */
	GetDItem(win->pwin, iEmailAddr, &type, &hn, &box);
	if (type == statText) NAhiliteDItem(win->pwin, iEmailAddr, 255);
	GetDItem(win->pwin, iMailServer, &type, &hn, &box);
	if (type == statText) NAhiliteDItem(win->pwin, iMailServer, 255);
	
	return (NA_NOTPROCESSED);
}

/* initialize preferences dialog
 */
static short prefsinit(na_win *win, long *data)
{
	PCstr tmpstr[257], eaddr[257];
	ControlHandle ctrl;
	
	win->type = PREFWIN;
	win->ctrlp = prefsctrl;
	win->menup = NAdialogMenu;
	win->updatep = prefsupdate;
	HLock((Handle) mpack_prefs);
	strcpy(C(tmpstr), (*mpack_prefs)->internet_host);
	HUnlock((Handle) mpack_prefs);
	SetPlen(tmpstr);
	NAsetIText(win->pwin, iHost, P(tmpstr));
	SelIText(win->pwin, iHost, 0, 32767);
	getICprefs(win, eaddr, tmpstr);
	NAsetIText(win->pwin, iEmailAddr, P(eaddr));
	NAsetIText(win->pwin, iMailServer, P(tmpstr));
	NAradioSet(win->pwin, iAuto, iDouble, (*mpack_prefs)->encoding + iAuto);
	NAsetIval(win->pwin, iTextEncode, (*mpack_prefs)->extract_text);
	NAsetIval(win->pwin, iQuitFinish, (*mpack_prefs)->quit_finished);
	if (tcpstart == -1) NAhiliteDItem(win->pwin, iSet, 255);
	NAmodalMenus(1);
	ShowWindow(win->pwin);
	
	return (NA_NOTPROCESSED);
}

/* Main menu procedure
 */
static short mainmenu(na_win *win, WORD menuid, WORD itemno)
{
	short status = NA_NOTPROCESSED;
	MenuHandle mh;
	PCstr version[32];

	switch (menuid) {
		case 0:
			NAenableMItem(mApple, iAbout);
			return (status);
		case mApple:
			if (itemno == iAbout) {
				CtoPCstrcpy(version, MPACK_VERSION);
				ParamText(P(version), NULL, NULL, NULL);
				return (NA_NOTPROCESSED);
			}
			break;
			
		case mFile:
			switch (itemno) {
				case iEncode:
					do_encode(NULL, 0);
					status = NA_PROCESSED;
					break;
					
				case iDecode:
					do_decode(NULL);
					status = NA_PROCESSED;
					break;

				case iClose:
					break;
					
				case iPrefs:
					status = NAwindow(0, NA_DIALOGWINDOW | NA_USERESOURCE
						| NA_MODAL | NA_DEFBUTTON | NA_TITLEBAR,
						NULL, prefsDLOG, (long *) NULL, 0, prefsinit);
					break;

				case iQuit:
					status = NA_REQCLOSEALL;
					break;
			}
			break;

		case mEdit:
			break;
		
		case mHelp:
			if (!helpw) {
				NAwindow(0, NA_USERESOURCE | NATEflags | NATE_READONLY | NA_SMARTSIZE,
					NULL, helpWIND, (long *) NULL, sizeof (nate_win), helpwindow);
			} else {
				SelectWindow(helpw);
			}
			break;
	}
	NAdisableMItem(mApple, iAbout);
	
	return (status);
}

/* make preferences folder/file
 *  returns -1 on failure.
 */
static short makepref()
{
	Handle hpref = NULL, htmpl;
	long dirID;
	short vRefNum;
	char *scan, *end;
	PCstr dname[257];
	CInfoPBRec cpb;
	DirInfo *dp = &cpb.dirInfo;
	ParamBlockRec pb;
	VolumeParam *vp = &pb.volumeParam;
	FInfo finfo;
	static unsigned char pname[] = "\pprefs";
	
	/* set up pref folder storage */
	pfolder = (struct pref_folder *) NewPtr(sizeof (struct pref_folder));
	if (!pfolder) return (-1);
	end = scan = (char *) pfolder->prefs + sizeof (pfolder->prefs) - 1;
	*scan = '\0';
	
	/* get pref folder */
	if (FindFolder(kOnSystemDisk, kPreferencesFolderType,
			kCreateFolder, &vRefNum, &pfolder->fspec.parID) != noErr) {
		return (-1);
	}
	
	/* create subfolder, if needed */
	PtoPCstrcpy(dname, (char *) "\pMpack");
	(void) DirCreate(vRefNum, pfolder->fspec.parID, P(dname), &dirID);
	
	/* get mpack prefs folder info */
	dp->ioNamePtr = P(dname);
	dp->ioVRefNum = vRefNum;
	dp->ioFDirIndex = 0;
	dp->ioDrDirID = pfolder->fspec.parID;
	if (PBGetCatInfoSync(&cpb) != noErr) return (-1);
	pfolder->fspec.parID = dirID = dp->ioDrDirID;
	pfolder->fspec.vRefNum = vRefNum;
	
	/* generate pathname */
	dp->ioFDirIndex = -1;
	for (;;) {
		*--scan = ':';
		if (scan - (char *) pfolder->prefs < 1 + PCstrlen(dname)) return (-1);
		scan -= PCstrlen(dname);
		memcpy(scan, C(dname), PCstrlen(dname));
		if ((dp->ioDrDirID = dp->ioDrParID) == 2) break;
		if (PBGetCatInfoSync(&cpb) != noErr) return (-1);
	}
	vp->ioVolIndex = 0;
	vp->ioNamePtr = P(dname);
	vp->ioVRefNum = vRefNum;
	if (PBGetVInfoSync(&pb) != noErr) return (-1);
	*--scan = ':';
	if (scan - (char *) pfolder->prefs < 16 + PCstrlen(dname)) return (-1);
	PtoPCstrcpy(pfolder->prefs, (char *) P(dname));
	CtoPCstrcat(pfolder->prefs, scan);
	
	/* Get/Create preferences file */
	HCreateResFile(vRefNum, dirID, pname);
	if (ResError() == noErr) {
		HGetFInfo(vRefNum, dirID, pname, &finfo);
		finfo.fdType = 'pref';
		finfo.fdCreator = 'mPAK';
		HSetFInfo(vRefNum, dirID, pname, &finfo);
		hpref = GetResource('mPRF', prefsID);
		DetachResource(hpref);
		htmpl = GetResource('TMPL', IDnaID);
		DetachResource(htmpl);
	}
	pfolder->refnum = HOpenResFile(vRefNum, dirID, pname, fsRdWrPerm);
	if (pfolder->refnum < 0) return (-1);
	if (hpref) {
		AddResource(hpref, 'mPRF', prefsID, "\p");
		AddResource(htmpl, 'TMPL', IDnaID, "\pIDna");
		ReleaseResource(htmpl);
	} else {
		hpref = GetResource('mPRF', prefsID);
	}
	if (!hpref) return (-1);
	mpack_prefs = (struct mpack_preferences **) hpref;
	
	return (0);
}

/* cleanup shared resources
 */
void maccleanup()
{
	if (pfolder) {
		CloseResFile(pfolder->refnum);
		DisposPtr((Ptr) pfolder);
	}
	if (icinst) ICStop(icinst);
	if (tcpstart == 1) NATCPdone(120); /* give 2 seconds to go away */
}

main()
{
	CursHandle cursH;

	if (NAinit(128, 2, openfile, mainmenu, 3, 1, 0, iClose) == 0) {
		/* set up preferences */
		if (makepref() < 0) {
			yell("Couldn't create preferences file");
		} else {
			/* set up internet config */
			if (ICStart(&icinst, 'mPAK') == noErr) {
				(void) ICFindConfigFile(icinst, 0, NULL);
			}
			/* save watch cursor */
			cursH = GetCursor(watchCursor);
			watch = **cursH;
			/* enter main loop, cleanup on exit */
			NAmainloop();
			maccleanup();
		}
	}
}
