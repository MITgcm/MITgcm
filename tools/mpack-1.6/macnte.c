/* macnte.c -- TextEdit Utilities for nifty application library
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
 *
 * Author:	Christopher J. Newman
 * Message:	This is a nifty program.
 */

#include "macnapp.h"
#ifndef THINK_C
#include <Fonts.h>
#include <Scrap.h>
#include <ToolUtils.h>
#endif

#define DOCWIDTH 2047
#define teinfo	((nate_win *)winp)

/* prototypes for private procedures */
static pascal void vscroll(ControlHandle, short);
static pascal void hscroll(ControlHandle, short);

/* initialize a premade window as a TextEdit window with options
 */
void NATEinit(na_win *winp, long flags, short docwidth, Ptr data, long len)
{
	Rect		rtemp, vtemp;
	
	winp->flags = (winp->flags & ~NATE_FLAGS) | flags;
	if (!docwidth)	docwidth = DOCWIDTH;
	teinfo->docwidth = docwidth;
	rtemp = winp->pwin->portRect;
	rtemp.top += teinfo->topoff;
	vtemp = rtemp;
	if (!(flags & NATE_NOHSCROLL)) {
		vtemp.right = vtemp.left + docwidth;
	}
	if (!(flags & NATE_READONLY)) {
		winp->keyp = NATEkeyp;
	}
	if (!(flags & NATE_NOMOUSE)) {	
		winp->mousep = NATEmousep;
		winp->idlep = NATEidlep;
	}
	winp->menup = NATEmenup;
	winp->activep = NATEactivep;
	winp->updatep = NATEupdatep;
	winp->ctrlp = NATEctrlp;
	winp->closep = NATEclosep;
	winp->cursorRgn = NewRgn();
	teinfo->vctrl = teinfo->hctrl = NULL;
	
	TEAutoView(true, teinfo->hTE = TENew(&vtemp, &rtemp));	
	if (len > 0 && data != (Ptr) NULL) {
		TESetText(data, len, teinfo->hTE);
		TESetSelect(0, 0, teinfo->hTE);
	}
	teinfo->lheight = (*teinfo->hTE)->lineHeight;
}

/* initialize a new TextEdit window
 */
short NATEinitp(na_win *winp, long *datap)
#ifndef THINK_C
#pragma unused (datap)
#endif
{
	teinfo->topoff = 0;
	NATEinit(winp, winp->flags, 0, NULL, 0);
	
	return (NA_PROCESSED);
}

/* set the controls in the TextEdit window correctly
 */
void NATEsetscroll(na_win *winp, Boolean moved, Rect *hrect, Rect *vrect)
{
	short		vmax, vvalue, hmax, hvalue;
	TEPtr		te = *teinfo->hTE;
	ControlHandle	vctrl, hctrl;
		

	vmax = te->nLines + (*(*te->hText + te->teLength - 1) == '\015' ? 1 : 0)
		- (te->viewRect.bottom - te->viewRect.top) / teinfo->lheight;
	hmax = (short) teinfo->docwidth - (te->viewRect.right - te->viewRect.left);
	if (vmax < 0) vmax = 0;
	if (hmax < 0) hmax = 0;
	vvalue = (te->viewRect.top - te->destRect.top) / teinfo->lheight;
	hvalue = te->viewRect.left - te->destRect.left;
	if (!(winp->flags & NATE_NOVSCROLL)) {
		if (teinfo->vctrl == (ControlHandle) NULL) {
			teinfo->vctrl = NewControl(winp->pwin, vrect, "\p", true, vvalue, 0, vmax,
				scrollBarProc, 0);
			if (winp->pwin != FrontWindow()) HiliteControl(teinfo->vctrl, 255);
		} else {
			if (vvalue < 0) vvalue = 0;
			if (vvalue > vmax) vvalue = vmax;
			SetCtlMax(vctrl = teinfo->vctrl, vmax);
			SetCtlValue(vctrl, vvalue);
			if (moved) {
				MoveControl(vctrl, vrect->left, vrect->top);
				SizeControl(vctrl, vrect->right - vrect->left,
					vrect->bottom - vrect->top);
				ShowControl(vctrl);
			}
		}
	}
	if (!(winp->flags & NATE_NOHSCROLL)) {
		if (teinfo->hctrl == (ControlHandle) NULL) {
			teinfo->hctrl = NewControl(winp->pwin, hrect, "\p", true, hvalue, 0, hmax,
				scrollBarProc, 0);
			if (winp->pwin != FrontWindow()) HiliteControl(teinfo->hctrl, 255);
		} else {
			if (hvalue < 0) hvalue = 0;
			if (hvalue > hmax) hvalue = hmax;
			SetCtlMax(hctrl = teinfo->hctrl, hmax);
			SetCtlValue(hctrl, hvalue);
			if (moved) {
				MoveControl(hctrl, hrect->left, hrect->top);
				SizeControl(hctrl, hrect->right - hrect->left,
					hrect->bottom - hrect->top);
				ShowControl(hctrl);
			}
		}
	}
}

/* track procedure for the vertical scroll bar
 */
static pascal void vscroll(ControlHandle ctrl, short part)
{
	short		amount, value, max, lh;
	na_win		*winp;
	TEHandle	hTE;
	
	if (part == 0) return;
	winp = * (na_win**) GetWRefCon((*ctrl)->contrlOwner);
	hTE = teinfo->hTE;
	value = ((*hTE)->viewRect.bottom - (*hTE)->viewRect.top) /
		(lh = teinfo->lheight);
	switch (part) {
		case inUpButton:
			amount = -1;
			break;
		case inDownButton:
			amount = 1;
			break;
		case inPageUp:
			amount = - value;
			break;
		case inPageDown:
			amount = value;
			break;
	}
	if ((amount += (value = GetCtlValue(ctrl))) < 0) amount = 0;
	if (amount > (max = GetCtlMax(ctrl))) amount = max;
	SetCtlValue(ctrl, amount);
	TEScroll(0, (value - amount) * lh, hTE);
}

/* track procedure for the horizontal scroll bar
 */
static pascal void hscroll(ControlHandle ctrl, short part)
{
	short		amount, value, max;
	
	if (part) {
		TEHandle hTE = (* (nate_win**) GetWRefCon((*ctrl)->contrlOwner))->hTE;
		
		value = (*hTE)->viewRect.right - (*hTE)->viewRect.left;
		switch (part) {
			case inUpButton:
				amount = -6;
				break;
			case inDownButton:
				amount = 6;
				break;
			case inPageUp:
				amount = - value;
				break;
			case inPageDown:
				amount = value;
				break;
		}
		if ((amount += (value = GetCtlValue(ctrl))) < 0) amount = 0;
		if (amount > (max = GetCtlMax(ctrl))) amount = max;
		SetCtlValue(ctrl, amount);
		TEScroll(value - amount, 0, hTE);
	}
}

/* activate procedure for TextEdit
 */
short NATEactivep(na_win *winp, Boolean on)
{
	if (on) {
		TEActivate(teinfo->hTE);
	} else {
		TEDeactivate(teinfo->hTE);
	}
	
	return (NA_NOTPROCESSED);
}

/* Update procedure for textedit window
 */
short NATEupdatep(na_win *winp, Boolean newsize)
{
	TEHandle	hTE = teinfo->hTE;
	WindowPtr	window = winp->pwin;
	Rect		prect, vrect, hrect, drect;
	
	prect = window->portRect;
	prect.top += teinfo->topoff;
	EraseRect(&prect);
	hrect = vrect = prect;
	vrect.top--;
	hrect.left--;
	vrect.left = ++vrect.right - 16;
	hrect.top = ++hrect.bottom - 16;
	vrect.bottom -= 14;
	hrect.right -= 14;
	InsetRect(&prect, 4, 4);
	prect.right -= 15;
	if (!(winp->flags & NATE_NOHSCROLL)) prect.bottom -= 15;
	prect.bottom -= (prect.bottom - prect.top) % teinfo->lheight;
	if (newsize) {
		drect = (*hTE)->viewRect = prect;
		drect.right = drect.left + (short) teinfo->docwidth;
		(*hTE)->destRect = drect;
		RectRgn(winp->cursorRgn, &prect);
		OffsetRgn(winp->cursorRgn, -window->portBits.bounds.left,
			-window->portBits.bounds.top);
		TECalText(hTE);
		TESelView(hTE);
		if (teinfo->hctrl != (ControlHandle) NULL) HideControl(teinfo->hctrl);
		if (teinfo->vctrl != (ControlHandle) NULL) HideControl(teinfo->vctrl);
	}
	TEUpdate(&prect, hTE);
	if (newsize) NATEsetscroll(winp, true, &hrect, &vrect);
	
	return (NA_NOTPROCESSED);
}

/* control processing procedure for TextEdit
 */
short NATEctrlp(na_win *winp, Point p, short part, short mods, ControlHandle ctrl)
#ifndef THINK_C
#pragma unused (mods)
#endif
{
	short			value;
	
	if (part) {
		value = GetCtlValue(ctrl);
		switch (part) {
			case inThumb:
				part = TrackControl(ctrl, p, (ProcPtr) NULL);
				if (part && (value -= GetCtlValue(ctrl))) {
					TEHandle	hTE = teinfo->hTE;
					
					if (ctrl == teinfo->vctrl) {
						TEScroll(0, value * teinfo->lheight, hTE);
					} else if (ctrl == teinfo->hctrl) {
						TEScroll(value, 0, hTE);
					}
				}
				break;
			
			default:
				(void) TrackControl(ctrl, p,
					(ProcPtr) (ctrl == teinfo->vctrl ? vscroll : hscroll));
				break;
		}
	}
	
	return (NA_PROCESSED);
}

/* idle procedure for TextEdit
 */
short NATEidlep(na_win *winp)
{
	TEIdle(teinfo->hTE);
	
	return (NA_PROCESSED);
}

/* key press procedure for TextEdit
 */
short NATEkeyp(na_win *winp, long c, short mods)
{
	short status = NA_NOTPROCESSED;
	
	if (!(mods & cmdKey)) {
		status = NA_PROCESSED;
		ObscureCursor();
		TEKey(c, teinfo->hTE);
		NATEsetscroll(winp, false, (Rect*) NULL, (Rect*) NULL);
	}
	
	return (status);
}

/* an edit menu handler for TextEdit
 */
short NATEmenup(na_win *winp, WORD menuid, WORD itemno)
{
	MenuHandle	mh = NAmenuh(mEdit);
	TEHandle	hTE = teinfo->hTE;
	TEPtr		pte;
	short		status = NA_NOTPROCESSED;
	
	switch (menuid) {
		case 0:
			pte = *hTE;
			if (pte->selStart != pte->selEnd) {
				EnableItem(mh, iCopy);
				if (!(winp->flags & NATE_READONLY)) {
					EnableItem(mh, iCut);
					EnableItem(mh, iClear);
				}
			} else {
				DisableItem(mh, iCopy);
				if (!(winp->flags & NATE_READONLY)) {
					DisableItem(mh, iCut);
					DisableItem(mh, iClear);
				}
			}
			EnableItem(mh, iSelAll);
			if (!(winp->flags & NATE_READONLY)) {
				EnableItem(mh, iPaste);
			}
			break;

		case mEdit:
			switch (itemno) {
				case iCut:
					TECut(hTE);
					goto DOSCRAP;
					
				case iCopy:
					TECopy(hTE);
				DOSCRAP:
					ZeroScrap();
					TEToScrap();
					goto EDITDONE;
					
				case iPaste:
					TEFromScrap();
					TEPaste(hTE);
					goto EDITDONE;
					
				case iClear:
					TEDelete(hTE);
					goto EDITDONE;
				
				case iSelAll:
					TESetSelect(0, 32767, hTE);
					TESelView(hTE);
				EDITDONE:
					status = NA_PROCESSED;
					NATEsetscroll(winp, false, (Rect*) NULL, (Rect*) NULL);
					break;
			}
		default:
			DisableItem(mh, iSelAll);
			break;
	}
	
	return (status);
}

/* mouse procedure for TextEdit
 */
short NATEmousep(na_win *winp, Point p, short type, short mods)
{
	TEHandle	hTE = teinfo->hTE;
	
	if (!PtInRect(p, &(*hTE)->viewRect)) return (NA_NOTPROCESSED);
	if (type == NA_DOWN1 || type == NA_DOWN2 || type == NA_DOWNN) {
		TEClick(p, mods & shiftKey ? true : false, hTE);
		NAmousetime = TickCount();
		NAlastmouse++;
	}
	
	return (NA_PROCESSED);
}

/* close procedure for TextEdit
 */
short NATEclosep(na_win *winp)
{
	TEDispose(teinfo->hTE);
	
	return (NA_CLOSED);
}

/* append text at the end of a TextEdit window
 */
void NATEappend(na_win *winp, char *data, long len)
{
	TEHandle	hTE = ((nate_win*) winp)->hTE;
	
	TESetSelect(32767, 32767, hTE);
	TEInsert(data, len, hTE);
	TESelView(hTE);
	NATEsetscroll(winp, false, (Rect*) NULL, (Rect*) NULL);
}
