/* macnfile.c -- standard file operations for nifty application library
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

#include <string.h>
#include "macnapp.h"

/* copy SFReply to StandardFileReply
 */
static void sftostd(SFReply *rep, StandardFileReply *reply)
{
	long procid = 0;
	
	if ((reply->sfGood = rep->good) == true) {
		reply->sfReplacing = false;
		reply->sfType = rep->fType;
		memcpy((void *) reply->sfFile.name, rep->fName, *rep->fName + 1);
		reply->sfFile.parID = 0;
		reply->sfFile.vRefNum = rep->vRefNum;
		GetWDInfo(rep->vRefNum, &reply->sfFile.vRefNum, &reply->sfFile.parID, &procid);
	}
}

/* get a file to save
 */
void NAputFile(Str255 prompt, Str255 initfname, StandardFileReply *reply)
{
	SFReply rep;
	Point where;
	
	if (NAgestaltBits & NA_HASSTDFILE) {
		StandardPutFile(prompt, initfname, reply);
	} else {
		where.h = where.v = 0;
		SFPutFile(where, prompt, initfname, nil, &rep);
		sftostd(&rep, reply);
	}
}

/* get a file to open
 */
void NAgetFile(FileFilterProcPtr filter, short numtypes,
	SFTypeList types, StandardFileReply *reply)
{
	Point p;
	SFReply rep;
	
	if (NAgestaltBits & NA_HASSTDFILE) {
		StandardGetFile(filter, numtypes, types, reply);
	} else {
		p.h = p.v = 0;
		SFGetFile(p, NULL, (ProcPtr) filter, numtypes, types, 0, &rep);
		sftostd(&rep, reply);
	}
}
