/* macnapp.h -- general mac application library header
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
/* (C) Copyright 1990-1995 by Christopher J. Newman
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
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
 *
 * Author:	Chris Newman
 * Message:	This is a nifty program.
 */

#ifdef THINK_C
#define QD(x)	(x)
#ifndef NULL
#define NULL	0
#endif
#else
#define QD(x)	(qd.x)
#include <MacTypes.h>
#include <Quickdraw.h>
#include <Events.h>
#include <Controls.h>
#include <Windows.h>
#include <MemoryMgr.h>
#include <Menus.h>
#include <OSUtils.h>
#include <TextEdit.h>
#include <Dialogs.h>
#endif

/* dual pascal/C strings
 */
typedef unsigned char PCstr;
#define C(str)	((char*)(str) + 1)
#define P(str)	((unsigned char*)(str))

/* various machine types.
 */
typedef unsigned short WORD;
typedef unsigned char BYTE;

/* useful macros:
 */
#define HIWORD(x)	( (WORD) ( (x) >> 16) )
#define LOWORD(x)	( (WORD) (x) )

/* most window/menu procedures return short integers (see defines below)
 */
typedef short (*na_menup)(struct na_win *, WORD, WORD);
typedef short (*na_mousep)(struct na_win *, Point, short, short);
typedef short (*na_ctrlp)(struct na_win *, Point, short, short, ControlHandle);
typedef short (*na_activep)(struct na_win *, Boolean);
typedef short (*na_closep)(struct na_win *);
typedef void (*na_afterp)(struct na_win *);
typedef short (*na_updatep)(struct na_win *, Boolean);
typedef short (*na_keyp)(struct na_win *, long, short);
typedef short (*na_cursorp)(struct na_win *, Point);
typedef short (*na_miscp)(struct na_win *, EventRecord *);
typedef short (*na_idlep)(struct na_win *);
typedef short (*na_taskp)(struct na_win *);
typedef short (*na_resizep)(struct na_win *, Point, Rect *);
typedef short (*na_openp)(short, FSSpec *, FInfo *);
typedef short (*na_initp)(struct na_win *, long *);
typedef struct na_win {
	long			flags;		/* flags indicating various window settings (see below) */
	short			delay;		/* delay between main loop cycles (in ticks) */
	short			mousepix;	/* number of pixels mouse can move until drag starts */
	short			minw, minh;	/* minimum width and height for the window */
	short			maxw, maxh;	/* maximum width and height for the window */
	short			type;		/* current window type (negatives reserved) */
	BYTE			locks;		/* locks on this window structure */
	short			priority;	/* priority if there is a taskp (-1 = everytime) */
	RgnHandle		cursorRgn;	/* cursor region */
	RgnHandle		uncrsrRgn;	/* region cursor isn't in */
	WindowPtr		pwin;		/* window pointer */
	struct na_win	**next;		/* handle to next window in linked list */
	struct na_win	**task;		/* handle to next task in a linked list of active tasks */
	struct na_win	**child;	/* handle to child window list (NULL = no child) */
	struct na_win	**parent;	/* handle to parent window (NULL = toplevel) */
	na_menup		menup;		/* menu proc */
	na_mousep		mousep;		/* mouse proc */
	na_ctrlp		ctrlp;		/* dialog item/control proc */
	na_activep		activep;	/* activate proc */
	na_closep		closep;		/* close proc */
	na_afterp		afterp;		/* after proc */
	na_updatep		updatep;	/* update proc */
	na_keyp			keyp;		/* key proc */
	na_cursorp		cursorp;	/* cursor setting proc */
	na_miscp		miscp;		/* miscellaneous event proc (disk/net/app/driver) */
	na_idlep		idlep;		/* idle proc */
	na_taskp		taskp;		/* task proc */
	na_resizep		resizep;	/* resize proc */
	long			resid;		/* storage for window resource id or user data. */
	char			*data;		/* data pointer for user */
} na_win;
typedef struct nate_win {
	na_win			winp;
	TEHandle		hTE;		/* textedit handle for auto-textedit routines */
	ControlHandle	hctrl;		/* horizontal scroll bar for textedit */
	ControlHandle	vctrl;		/* vertical scroll bar for textedit */
	long			docwidth;	/* width of the document */
	short			lheight;	/* line height of text */
	short			topoff;		/* offset from top of window to textedit area */
} nate_win;
/* procedures types:
 *
 * // called for menu events when window is frontmost
 * // supercedes global menu function unless NA_NOTPROCESSED is returned.
 * // if menuid is 0, the procedure should enable/disable menus appropriately
 * // if menuid is 1, the procedure should disable enabled menus appropriately
 * short menu(winp, menuid, itemno)
 *	WORD menuid;			// resource id of the menu
 *	WORD itemno;			// item number of the menu item
 *
 * // called for mouse down/up/move events in the window
 * short mouse(winp, p, type, mods)
 *	Point p;				// location of mouse action
 *	short type;				// type of mouse action (see below)
 *	short mods;				// modifier keys
 *
 * // called for dialog events in dialog windows
 * // control events in windows with controls
 * // In a dialog window with no key procedure, ESC, command-., return, enter are
 * // mapped to iCancel and iOk.
 * short control(winp, p, itemHit, mods, ctrlh)
 *	Point p;				// point in window local coords
 *	short itemHit;			// the item/part code clicked
 *	short mods;				// modifier keys
 *	ControlHandle ctrlh;	// handle to the control
 *
 * // called when window is activated or deactivated
 * // close return values may be ignored on a deactivate
 * short activate(winp, on)
 *	Boolean on;				// true = activate, false = deactivate
 *
 * // called when a window close request has been made (close box/close menu item)
 * // called when any window function returns NA_REQCLOSE or NA_REQCLOSEALL
 * // respond with either NA_CLOSED or NA_NOTPROCESSED
 * short close(winp)
 *
 * // called after window closed & removed from lists, but before window object freed
 * void afterp(winp)
 *
 * // called on update events
 * short update(winp, newsize)
 *	Boolean newsize;		// true if r is a new window size
 *
 * // called on key/autokey events (menu keys are parsed out first)
 * short key(winp, c, mods)
 *	long c;					// ASCII value of the key (unless NA_RAWKEY option set)
 *	short mods;				// modifier keys
 *
 * // called when cursor moves into or out of a window
 * // use winp->flags & NA_CURSORON
 * // close request return values are ignored.
 * // return NA_PROCESSED only when the cursorRgn is changed
 * short cursor(winp, p)
 *	Point p;				// point where the cursor is
 *
 * // This is called for miscellaneous events (disk/network/driver/app1-3)
 * short miscp(winp, pevent)
 *	EventRecord *pevent;	// the event
 *
 * // called every time through the event loop when window active
 * short idle(winp)
 *
 * // called cyclicly with other tasks with each pass through event loop
 * // only used when task is installed
 * // the task procedure can only close it's own window.
 * short task(winp)
 *
 * // function called to resize the window 
 * // (parameters similar to GrowWindow function)
 * short resize(winp, where, rct)
 *  Point where;
 *  Rect *rct;
 *
 * // function passed to NAinit:
 * // should return 0 if OK, and -1 to stop opening files
 * short open(message, afile, fspec)
 *	short message;			// either appOpen or appPrint (in SegLoad.h)
 *  FSSpec *fspec;			// file to open/print
 *  Finfo *finfo;			// finder info
 *
 * // function passed to NAwindow:
 * // returns standard window status
 * short init(winp, datap)
 *	na_win	*winp;			// pointer to new window structure
 *	long	*datap;			// pointer to data passed through NAwindow
 */

/* niftyapp globals */
extern na_win **NAhead;			/* handle to the head of the window tree */
extern na_win **NAtask;			/* handle to the head of the window task list */
extern na_win **NActask;		/* handle to the current task */
extern na_win *NAwin;			/* pointer to current window (NULL = no current window) */
extern na_menup NAmenup;		/* pointer to default menu procedure */
extern MenuHandle **NAmenus;	/* list of menu handles */
extern short NAnewitem;			/* item number of the new item on the file menu (0 = not avail) */
extern short NAcloseitem;		/* item number of the close item on the file menu (0 = not avail) */
extern short NAappleitems;		/* the number of (user) items at the top of the apple menu */
extern short NAhelpitems;		/* the number of (user) help items */
extern short NAhelpcount;   	/* the number of (system) items at the top of the help menu */
extern Boolean NAhasedit;		/* true if application has an edit menu */
extern long NAdelay;			/* the wait next event delay */
extern SysEnvRec NAsysenv;		/* mac system environment */
extern Boolean NAinBack;		/* true if application is backgrounded */
extern long NAmousetime;		/* time of last mouse up event */
extern short NAlastmouse;		/* kind of last mouse event */
extern Cursor NAibeam;			/* the ibeam cursor */
extern long NAgestaltBits;  	/* 0 = gestalt not on system */

/* globals for your convenience */
extern RgnHandle NAfullRgn, NAnullRgn;

/* niftyapp definitions */

/* default resource id for niftyapp windows */
#define NA_CLIPWINDOW	1000
#define NA_ABOUTDLOG  	1000
#define NA_PREFSDLOG  	1001
#define NA_HELPSTR		1000
/* default item numbers for OK & cancel */
#define iOk				1
#define iCancel			2
/* default ids for APPLE, FILE, EDIT, and HELP menus */
#define mApple			128
#define mFile			129
#define mEdit			130
#define mHelp			((WORD)-16490)
/* default item numbers for about & edit menu */
#define iAbout			1
#define iUndo			1
#define iCut			3
#define iCopy			4
#define iPaste			5
#define iClear			6
#define iSelAll			7
#define iClipboard		9
/* new window positions */
#define NA_HFULLSCN		0x0000
#define NA_HQUARTERSCN	0x0001
#define NA_HHALFSCN		0x0002
#define NA_H3QUARTERSCN	0x0003
#define NA_VFULLSCN		0x0000
#define NA_VQUARTERSCN	0x0004
#define NA_VHALFSCN		0x0008
#define NA_V3QUARTERSCN	0x000C
#define NA_CENTERSCN	0x0000
#define NA_TOPSCN		0x0010
#define NA_BOTTOMSCN	0x0020
#define NA_LEFTSCN		0x0040
#define NA_RIGHTSCN		0x0080
#define NA_TITLEOFFSET	0x0100
/* new window flags */
#define NA_PLAINWIN		0x00000000L	/* plain window -- no title, simple border */
#define NA_COLORWINDOW	0x00000001L	/* allow color in the window */
#define NA_DIALOGWINDOW	0x00000002L	/* open as a dialog */
#define NA_TITLEBAR		0x00000004L	/* have title bar */
#define NA_GROWBOX		0x00000008L	/* have a grow box (enables automatic drawing) */
#define NA_ZOOMBOX		0x00000010L	/* have a zoom box */
#define NA_CLOSEBOX		0x00000020L	/* have a close box (enables close menu item) */
#define NA_SHADOWBORDER	0x00000040L	/* have a shadow border (for dialogs) */
#define NA_DOUBLEBORDER	0x00000080L	/* have a double (dialog) border */
#define NA_ROUNDBORDER	0x000000c0L	/* have rounded corners and black title bar */
#define NA_CHILDWINDOW	0x00000100L	/* open as a child window of current window */
#define NA_NOTVISIBLE	0x00000200L	/* do not make window visible on open */
#define NA_BEHIND		0x00000400L	/* open window behind current window */
#define NA_HASCONTROLS	0x00000800L	/* process/draw/kill controls automatically */
#define NA_HASTASK		0x00001000L	/* install window in background task list */
#define NA_USERESOURCE	0x00002000L	/* use res parameter as WIND/DLOG/wctb/dctb resource */
#define NA_CURSORON		0x00004000L	/* true if application has set the cursor */
#define NA_MODAL		0x00008000L	/* set if window/dialog will be modal */
#define NA_DEFBUTTON	0x00010000L	/* show default button after init proc */
#define NA_COPYDATA		0x00020000L	/* data will by copied by NAwindow */
#define NA_SMARTSIZE	0x00040000L	/* window resizes & placements are saved in WIND res */
#define NA_FORCESIZE	0x00080000L	/* when a resource is used, re-size the window anyway */
#define NA_RAWKEY		0x00100000L	/* if set, key event fields aren't stripped */
#define NA_HILITECTRLS	0x00200000L	/* if set, hilite controls on activate/deactive */
#define NATE_FLAGS		0x0f000000L	/* flags reserved for NATE */
#define NA_USER_FLAG1	0x10000000L	/* flags reserved for users */
#define NA_USER_FLAG2	0x20000000L
#define NA_USER_FLAG3	0x40000000L
#define NA_USER_FLAG4	0x80000000L
/* niftyapp window/task types */
#define NA_CLIPTYPE		-1
#define NA_DEBUGTYPE	-2
#define NA_TCPTYPE		-3
#define NA_SMTPTYPE		-4
/* mouse click types */
#define NA_DOWN1		0
#define NA_UP1			1
#define NA_DOWN2		2
#define NA_UP2			3
#define NA_DOWNN		4
#define NA_UPN			5
#define NA_DRAG			6
#define NA_RELEASE		7
/* return values for window/menu procedures */
#define NA_ALLCLOSED	-4		/* all windows are to be destroyed & exit app immediately */
#define NA_REQCLOSEALL	-3		/* request to close all windows & exit app */
#define NA_CLOSED		-2		/* current window is ready to close (used by closep/taskp) */
#define NA_REQCLOSE		-1		/* request to close current window */
#define NA_NOTPROCESSED	0		/* use any default handler available */
#define NA_PROCESSED	1		/* do nothing more */
#define NA_USERINTERACT 2		/* user interaction pending -- don't do tasks */
/* Gestalt bits */
#define NA_HASAEVENTS	0x00000001L	/* Apple events supported */
#define NA_HASFSSPEC    0x00000002L /* FSSpec calls supported */
#define NA_HASSTDFILE   0x00000004L /* New standard file available */

/* niftyapp basic macros */

#define NAunlockWindow(winp)		{if (!--(winp)->locks) HUnlock((Handle) GetWRefCon((winp)->pwin));}
#define NAunlockWindowh(winh, winp)	{if (!--(winp)->locks) HUnlock((Handle) winh);}
#define NAisDAWindow(pWnd)			(( (WindowPeek) pWnd)->windowKind < 0)
#define NAmenuh(menu)				((*NAmenus)[(menu) - mApple])
#define NAenableMItem(menu, item)	EnableItem(NAmenuh(menu), item)
#define NAdisableMItem(menu, item)	DisableItem(NAmenuh(menu), item)
#define NAcheckItem(menu, item, c)	CheckItem(NAmenuh(menu), item, c)
#define NAgetDHandle(dlg, it, hn)	{short ty; Rect r; GetDItem(dlg, it, &ty, (Handle *) (hn), &r);}
#define NAgetDRect(dlg, it, rct)	{short ty; Handle hn; GetDItem(dlg, it, &ty, &hn, (rct));}
#define NAsetIval(dlg, it, val)		{short ty; Rect r; Handle hn; GetDItem(dlg, it, &ty, &hn, &r); SetCtlValue((ControlHandle)hn, (val));}
#define NAsetInum(dlg, it, val)		NAsetIText(dlg, it, longtoPCstr(val))
#define NAalert(resid)				Alert(resid, NAfilterProc)

/* niftyapp basic procedures */

/* initialize the Macintosh managers and niftyapp internal variables.
 * optionally set up a menu bar & menu procedure.
 * Returns 0 if OK, negative if an error occured, 1 if print files requested & app should quit
 * short minK;			// minimum K needed to execute
 * short masters;		// number of times to call MoreMasters()
 * na_proc *openp;		// open file procedure -- called for each application in the startup list
 * na_proc *menup;		// pointer to a menu procedure (NULL = no menu handling)
 * short nummenu;		// number of menus (starting at mApple == 128)
 * short numapple;		// number of apple menu items
 * short newitem;		// item number of new item (mFile)
 * short closeitem;		// item number of close item (mFile)
 */
short NAinit(short, short, na_openp, na_menup, short, short, short, short);

/* call the main loop procedure
 */
void NAmainloop(void);

/* create a rectangle based on the screen size
 *	short position;		// see above
 */
Rect *NAscreenrect(short);

/* create a new window structure
 * returns window status result, up to the caller to pass on NA_ALLCLOSED
 *	Rect *rpos;			// placement rectangle
 *	long flags;			// flags determining type of window
 *	char *title;		// window title (C string may not be NULL unless NA_USERESOURCE)
 *	short res;			// resource number of WIND/DLOG/wctb/dctb/DITL
 *	long *initdata;		// window data (may be NULL)
 *	long datasize;		// bytes of window data
 *	na_proc *initp;		// procedure to initialize the window functions, etc.
 */
short NAwindow(Rect *, long, char *, short, long *, long, na_initp);

/* create & add a new task to the task list, given pointer to task procedure,
 * and data size
 */
na_win **NAaddtask(na_taskp, long);

/* standard init procedure for an about box -- stops all background tasks, however */
short NAabout(na_win*, long*);

/* standard button flash procedure used by shell for keypresses */
void NAflashButton(DialogPtr, short);

/* draw the default button */
void NAdefaultButton(DialogPtr);

/* filter proc for modal dialogs which handles ESC and command-. */
pascal Boolean NAfilterProc(DialogPtr, EventRecord *, short *);

/* re-calculate cursor region information (after changing winp->cursorRgn) */
void NAcalcCursor(na_win*);

/* this saves a window's dimensions into a 'WIND' resource with appropriate resid */
void NAsaveWin(na_win*);

/* This is available to access window structures other than the current window
 * best for looking at parent window or child window(s).
 */
na_win *NAlockWindow(na_win**);

/* This is available, but the user should only call it is severe cases */
short NAcloseWindow(na_win*, short);

/* this is for closing all windows, the user should only call it from main
 * usually NAhead is the first parameter.
 */
short NAcloseWindows(na_win**, short);

/* this is for sending an event directly to the misc procedure of all windows.
 * usually NAhead is the first parameter.
 */
short NAallWindows(na_win**, EventRecord*);


/* niftyapp clipboard library:
 * NAclipboard: true = window on, false = window off, 'TEXT' or 'PICT'
 */
void NAclipboard(Boolean, ResType);


/* niftyapp debug library:
 */
void NAdebug(char *, ...);
#ifdef DEBUG
#define NADEBUG(x) NAdebug x
#else
#define NADEBUG(x)
#endif


/* niftyapp dialog library:
 */
/* select a radio button
 * returns number from 0 to firstitem - lastitem: the button that's been pressed
 *	DialogPtr dialog;	// the dialog window
 *	short firstitem;	// the itemno of first radio button
 *	short lastitem;		// the itemno of last radio button
 *	short setting;		// the radio button to set (itemno to lastitem)
 */
short NAradioSet(DialogPtr, short, short, short);

/* get the itemno of the active radio button
 *	DialogPtr dialog;	// the dialog window
 *	short firstitem;	// the itemno of first radio button
 *	short lastitem;		// the itemno of last radio button
 */
short NAradioGet(DialogPtr, short, short);

/* enable/disable,hilite,show/hide an item in a dialog window */
void NAenableDItem(DialogPtr, short, Boolean);
void NAhiliteDItem(DialogPtr, short, short);
void NAvisibleDItem(DialogPtr, short, Boolean);

/* set/get the item text in a dialog item */
void NAsetIText(DialogPtr, short, PCstr*);
void NAgetIText(DialogPtr, short, PCstr*);

/* enable/disable modal menus for a moveable modal dialog box (1 = go modal) */
void NAmodalMenus(int);

/* handle edit menu for dialogs */
short NAdialogMenu(na_win *, WORD, WORD);


/* NATE (NiftyApp TextEdit) libraries
 */
#define NATEflags (NA_TITLEBAR | NA_GROWBOX | NA_ZOOMBOX | NA_CLOSEBOX \
	| NA_HASCONTROLS | NA_HILITECTRLS)
#define NATE_DEFAULT	0x00000000L
#define NATE_READONLY	0x01000000L
#define NATE_NOMOUSE    0x02000000L
#define NATE_NOHSCROLL	0x04000000L
#define NATE_NOVSCROLL	0x08000000L
void NATEinit(na_win*, long, short, Ptr, long); /* winp, flags, horizwidth, data, len */
short NATEinitp(na_win*, long*);
short NATEmousep(na_win*, Point, short, short);
short NATEidlep(na_win*);
short NATEactivep(na_win*, Boolean);
short NATEkeyp(na_win*, long, short);
short NATEmenup(na_win*, WORD, WORD);
short NATEupdatep(na_win*, Boolean);
short NATEctrlp(na_win*, Point, short, short, ControlHandle);
short NATEclosep(na_win*);

void NATEsetscroll(na_win*, Boolean, Rect*, Rect*);
void NATEappend(na_win*, char*, long);


/* Niftyapp file library
 */
/* get a file to open -- similar to StandardGetFile, but works on older systems
 * extra fields in "StandardFileReply" are only valid if NA_HASSTDFILE is set in
 * NAgestaltBits
 */
void NAgetFile(FileFilterProcPtr, short, SFTypeList, StandardFileReply *);
/* put a file to open -- similar to StandardPutFile, but works on older systems
 * extra fields in "StandardFileReply" are only valid if NA_HASSTDFILE is set in
 * NAgestaltBits
 */
void NAputFile(Str255, Str255, StandardFileReply *);


/* Niftyapp TCP library
 */
/* tcp stream descriptor */
typedef int na_tcp;
/* tcp init function 
 *  passed NATCP_connect for success, NATCP_nodriver/NATCP_nomem for failure
 *  passed 1 to 100 for progress waiting for MacTCP to finish cleanly
 */
typedef void na_tcpinitp(short);
/* TCP read/status callback for connection, or TCP window
 * void *user;   Context generic pointer (passed to NATCPopen)
 * na_tcp s;     TCP stream id
 * short status; TCP status (see below)
 * long size;    size of buffer (or Macintosh error)
 * char *data;   data or NULL
 */
typedef void na_tcpreadp(void *, na_tcp, short, long, char *);
typedef void na_readp(na_win *, short, long, char *);
/* TCP window */
typedef struct natcp_win {
	na_win		winp;
	na_tcp		s;
	na_readp    *readp;
} natcp_win;
/* status values/bits */
#define NATCP_closing  0x08		/* other end of connection closed */
#define NATCP_urgent   0x04		/* in urgent mode */
#define NATCP_more     0x02		/* more data will follow immediately */
#define NATCP_data     0x01		/* data to read */
#define NATCP_connect  0x00		/* connection ready */
#define NATCP_noread   -1		/* non-fatal */
#define NATCP_nowrite  -2		/* fatal... */
#define NATCP_nodriver -3
#define NATCP_notcpbuf -4
#define NATCP_nomem    -5
#define NATCP_nohost   -6
#define NATCP_nocon    -7
#define NATCP_closed   -8		/* connection fully closed */
/* open flags */
#define NATCP_server	0x01	/* be a server */
/* functions */
void NATCPinit(na_tcpinitp *);
/* NATCPsettings: TCP buffer size, type of service, precedence, write buffer size */
void NATCPsettings(long, short, short, unsigned short);
short NATCPtask(na_win *);
/* NATCPopen: callback, context, host, port, flags */
na_tcp NATCPopen(na_tcpreadp *, void *, char *, long, short);
na_tcp NATCPwinopen(natcp_win *, char *, long, short); /* tcp window, host, port, flags */
short NATCPwrite(na_tcp, Ptr, long, short); /* tcp, buffer, length, dispose */
short NATCPputchar(na_tcp, char);
void NATCPclose(na_tcp);
void NATCPdone(long); /* number of 1/60 sec intervals to wait for MacTCP to finish cleanly */
/* returns passes NATCP_connect to readp on success with hostname as argument */
void NATCPgethost(na_tcpreadp *, void *);


/* niftyapp SMTP library
 */
/* status:
 * void *context;	user context
 * short code;		see below
 * short err;		SMTP error code or NATCP error code (NASMTP_tcpfail) or 0
 * long num;		macintosh TCP error code or address number or 0
 * char *errstr;	SMTP error string or NULL
 */
typedef void (*na_smtpstat)(void *, short, short, long, char *);
#define NASMTP_progress   2 /* progress: err = % done */
#define NASMTP_badaddr    1 /* address was not valid */
#define NASMTP_completed  0 /* success */
#define NASMTP_nomem     -1 /* not enough memory */
#define NASMTP_badargs   -2 /* input arguments invalid */
#define NASMTP_oserr     -3 /* OS failure (e.g. file error) */
#define NASMTP_tcpfail   -4 /* TCP connection to SMTP server failed */
#define NASMTP_conclosed -5 /* connection closed by other side before completion */
#define NASMTP_badprot   -6 /* other end sent unrecognizable protocol */
#define NASMTP_temperr   -7 /* SMTP persistant temporary failure */
#define NASMTP_permerr   -8 /* SMTP permanent failure */
/* Submit email: statf, server, fspec, headers, envelope, flags, context */
void NASMTPsubmit(na_smtpstat, char *, FSSpec *, Handle, Handle, short, void *);
#define NASMTP_crtrans	0x01 /* flag indicating translation of CR -> CRLF desired */

/* PC, C string libraries:
 */
#define SetClen(pcstr)	(*((pcstr) + *(pcstr) + 1) = '\0')
#define PCstrlen(pcstr)	(*(pcstr))
#define Pstrlen(pstr)	(* (unsigned char *) (pstr))

void PtoPCstrcpy(PCstr*, char*);
void CtoPCstrcpy(PCstr*, char*);
void PCtoPCstrcpy(PCstr*, PCstr*);
void PtoPCstrncpy(PCstr*, char*, short);
void CtoPCstrncpy(PCstr*, char*, short);
void PtoPCstrcat(PCstr*, char*);
void CtoPCstrcat(PCstr*, char*);
PCstr *PtoPCstr(char*);
PCstr *CtoPCstr(char*);
void SetPlen(PCstr*);
PCstr *longtoPCstr(long);
