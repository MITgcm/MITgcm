/* macndlog.c -- dialog utilities for nifty application library
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

#ifndef THINK_C
#include <ToolUtils.h>
#endif
#include <Balloons.h>
#include "macnapp.h"

/* enable/disable menus for a moveable modal dialog
 */
void NAmodalMenus(int begin)
{
	short i, end;
	MenuHandle mh;
	
	/* unhilite menus */
	HiliteMenu(0);
	
	/* everything but the edit/apple/help menus */
	for (i = mFile; (mh = NAmenuh(i)); ++i) {
		if (i != mEdit) {
			if (begin) DisableItem(mh, 0);
			else EnableItem(mh, 0);
		}
	}
	
	/* kill the help items */
	mh = NAmenuh(mApple);
	i = NAappleitems;
	if (NAhelpcount) {
		HMGetHelpMenuHandle(&mh);
		i = NAhelpcount + 1;
	}
	for (end = NAhelpitems + i; i <= end; ++i) {
		if (begin) DisableItem(mh, i);
		else EnableItem(mh, i);
	}
	
	DrawMenuBar();
}

/* enable/disable a control in a dialog window
 */
void NAenableDItem(DialogPtr dialog, short item, Boolean on)
{
	short	type;
	Handle			ctrl;
	short			ty;
	Rect			box;
	
	GetDItem(dialog, item, &ty, &ctrl, &box);
	type = ty;
	if (on) type &= ~itemDisable;
	else	type |= itemDisable;
	if (type == (editText | itemDisable)) {
		type = statText;
	} else if (type == statText) {
		type = editText;
	}
	SetDItem(dialog, item, type, ctrl, &box);
}

/* hilite a control in a dialog window
 */
void NAhiliteDItem(DialogPtr dialog, short item, short how)
{
	Handle		ctrl;
	short		type;
	Rect		box;
	PenState	tmpPen;
	
	GetDItem(dialog, item, &type, &ctrl, &box);
	if (type & ctrlItem) {
		HiliteControl((ControlHandle) ctrl, how);
	} else if (type & (statText | editText)) {
		GetPenState(&tmpPen);
		PenNormal();
		if (how == 255) PenPat(QD(gray));
		InsetRect(&box, -3, -3);
		FrameRect(&box);
		SetPenState(&tmpPen);
	}
}

/* make an item visible/invisible in a dialog window
 */
void NAvisibleDItem(DialogPtr dialog, short item, Boolean show)
{
	if (show)	ShowDItem(dialog, item);
	else		HideDItem(dialog, item);
}

/* set the text in a dialog item
 */
void NAsetIText(DialogPtr dialog, short item, PCstr *str)
{
	Handle		texth;
	
	NAgetDHandle(dialog, item, &texth);
	SetIText(texth, str);
}

/* get the text in a dialog item
 */
void NAgetIText(DialogPtr dialog, short item, PCstr *str)
{
	Handle		texth;
	
	NAgetDHandle(dialog, item, &texth);
	GetIText(texth, str);
	SetClen(str);
}

/* set the appropriate radio buttons
 */
short NAradioSet(DialogPtr dialog, short firstitem, short lastitem, short setting)
{
	short	item;
	ControlHandle	ctrl;
	
	for (item = firstitem; item <= lastitem; item++) {
		NAgetDHandle(dialog, item, &ctrl);
		SetCtlValue(ctrl, item == setting ? 1 : 0);
	}
	
	return (setting - firstitem);
}

/* get the itemno of the active radio button
 */
short NAradioGet(DialogPtr dialog, short firstitem, short lastitem)
{
	short	item;
	ControlHandle	ctrl;
	
	for (item = firstitem; item <= lastitem; item++) {
		NAgetDHandle(dialog, item, &ctrl);
		if (GetCtlValue(ctrl)) return (item);
	}
	
	return (firstitem);
}

/* handle the edit menu for a dialog window
 */
short NAdialogMenu(na_win *win, WORD menu, WORD item)
{
	MenuHandle	mh = NAmenuh(mEdit);
	DialogPeek dpeek = (DialogPeek) win->pwin;
	short result = NA_NOTPROCESSED;
	
	switch (menu) {
		case 0:
			if ((*dpeek->textH)->selStart != (*dpeek->textH)->selEnd) {
				EnableItem(mh, iCopy);
				EnableItem(mh, iCut);
				EnableItem(mh, iClear);
			}
			EnableItem(mh, iPaste);
			if ((*dpeek->textH)->teLength > 0) EnableItem(mh, iSelAll);
			break;
		case mEdit:
			switch (item) {
				case iCut:
					DlgCut(win->pwin);
					ZeroScrap();
					TEToScrap();
					break;
				case iCopy:
					DlgCopy(win->pwin);
					ZeroScrap();
					TEToScrap();
					break;
				case iClear:
					DlgDelete(win->pwin);
					break;
				case iPaste:
					DlgPaste(win->pwin);
					break;
				case iSelAll:
					SelIText(win->pwin, dpeek->editField + 1, 0, 32767);
					break;
			}
			result = NA_PROCESSED;
			break;
	}
	if (menu != 0) DisableItem(mh, iSelAll);
	
	return (result);
}
