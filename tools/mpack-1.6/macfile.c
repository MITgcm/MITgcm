/* macfile.c -- simple applesingle/appledouble encoding/decoding routines
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
/* (C) Copyright 1994-1995 by Christopher J. Newman
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

#include <stdio.h>
#include <string.h>
#include "macmpack.h"   /* for copy_buf & watch */

/* applefile definitions used */
#define APPLESINGLE_MAGIC 0x00051600L
#define APPLEDOUBLE_MAGIC 0x00051607L
#define VERSION 0x00020000
#define ENT_DFORK   1
#define ENT_RFORK   2
#define ENT_NAME    3
#define ENT_COMMENT 4
#define ENT_DATES   8
#define ENT_FINFO   9
#define CONVERT_TIME 1265437696L

/* applefile structures */
typedef struct ap_header {
	long magic;
	long version;
	char fill[16];
	short entries;
} ap_header;
typedef struct ap_entry {
	unsigned long id;
	unsigned long offset;
	unsigned long length;
} ap_entry;
typedef struct ap_dates {
	long create, modify, backup, access;
} ap_dates;

/* default number of entries */
#define NUM_ENTRIES 6

/* Generate an applefile
 *  outfile -- output file
 *  fpb -- hierarchical file parameter block
 *  rfork, dfork -- resource & data forks
 * returns -1 on failure, 0 on success
 *
 * closes dfork & rfork, but not outputfile
 */
int encode_applefile(FILE *outfile, HFileInfo *fpb, FILE *rfork, FILE *dfork)
{
	ap_header head;
	ap_entry entries[NUM_ENTRIES];
	ap_dates dates;
	short i, count;
	long comlen, procID;
	DateTimeRec cur_time;
	unsigned long cur_secs;
	IOParam vinfo;
	GetVolParmsInfoBuffer vp;
	DTPBRec dtp;
	char comment[256];
	
	/* make sure things look OK */
	if (!rfork || !outfile) {
		if (rfork) fclose(rfork);
		if (dfork) fclose(dfork);
		if (outfile) fclose(outfile);
		return (-1);
	}
	
	/* get a file comment, if possible */
	procID = 0;
	GetWDInfo(fpb->ioVRefNum, &fpb->ioVRefNum, &fpb->ioDirID, &procID);
	memset((void *) &vinfo, '\0', sizeof (vinfo));
	vinfo.ioVRefNum = fpb->ioVRefNum;
	vinfo.ioBuffer = (Ptr) &vp;
	vinfo.ioReqCount = sizeof (vp);
	comlen = 0;
	if (PBHGetVolParmsSync((HParmBlkPtr) &vinfo) == noErr &&
		((vp.vMAttrib >> bHasDesktopMgr) & 1)) {
		memset((void *) &dtp, '\0', sizeof (dtp));
		dtp.ioVRefNum = fpb->ioVRefNum;
		if (PBDTGetPath(&dtp) == noErr) {
			dtp.ioDTBuffer = (Ptr) comment;
			dtp.ioNamePtr = fpb->ioNamePtr;
			dtp.ioDirID = fpb->ioFlParID;
			if (PBDTGetCommentSync(&dtp) == noErr) comlen = dtp.ioDTActCount;
		}
	}
	
	/* write header */
	head.magic = dfork ? APPLESINGLE_MAGIC : APPLEDOUBLE_MAGIC;
	head.version = VERSION;
	memset(head.fill, '\0', sizeof (head.fill));
	head.entries = NUM_ENTRIES - (dfork ? 0 : 1);
	fwrite((char *) &head, sizeof (head), 1, outfile);
	
	/* write entry descriptors */
	entries[0].offset = sizeof (head) + sizeof (ap_entry) * head.entries;
	entries[0].id = ENT_NAME;
	entries[0].length = *fpb->ioNamePtr;
	entries[1].id = ENT_FINFO;
	entries[1].length = sizeof (FInfo) + sizeof (FXInfo);
	entries[2].id = ENT_DATES;
	entries[2].length = sizeof (ap_dates);
	entries[3].id = ENT_COMMENT;
	entries[3].length = comlen;
	entries[4].id = ENT_RFORK;
	entries[4].length = fpb->ioFlRLgLen;
	entries[5].id = ENT_DFORK;
	entries[5].length = fpb->ioFlLgLen;
	for (i = 1; i < NUM_ENTRIES; ++i) {
		entries[i].offset = entries[i-1].offset + entries[i-1].length;
	}
	fwrite((char *) entries, sizeof (ap_entry), head.entries, outfile);
	
	/* write name */
	fwrite((char *) fpb->ioNamePtr + 1, *fpb->ioNamePtr, 1, outfile);
	/* write finder info */
	fwrite((char *) &fpb->ioFlFndrInfo, sizeof (FInfo), 1, outfile);
	fwrite((char *) &fpb->ioFlXFndrInfo, sizeof (FXInfo), 1, outfile);
	/* write dates */
	GetTime(&cur_time);
	Date2Secs(&cur_time, &cur_secs);
	dates.create = fpb->ioFlCrDat + CONVERT_TIME;
	dates.modify = fpb->ioFlMdDat + CONVERT_TIME;
	dates.backup = fpb->ioFlBkDat + CONVERT_TIME;
	dates.access = cur_secs + CONVERT_TIME;
	fwrite((char *) &dates, sizeof (ap_dates), 1, outfile);
	/* write comment */
	if (comlen) fwrite(comment, sizeof (char), comlen, outfile);
	/* write resource fork */
	while ((count = fread(copy_buf, sizeof (char), sizeof (copy_buf), rfork)) > 0) {
		fwrite(copy_buf, sizeof (char), count, outfile);
	}
	fclose(rfork);
	/* write data fork */
	if (dfork) {
		while ((count = fread(copy_buf, sizeof (char), sizeof (copy_buf), dfork)) > 0) {
			fwrite(copy_buf, sizeof (char), count, outfile);
		}
		fclose(dfork);
	}
	
	return (0);
}

/* decode an applefile
 *  infile -- input file
 *  fspec  -- file spec of saved file
 * returns -1 on failure, 0 on success
 */
int decode_applefile(FILE *infile, FSSpec *fspec)
{
	ap_header head;
	ap_entry entries[NUM_ENTRIES + 1];
	ap_dates dates;
	StandardFileReply reply;
	int i, j;
	short refnum;
	long count;
	OSErr err;
	HFileInfo *fpb;
	CInfoPBRec cipbr;
	long procID;
	IOParam vinfo;
	GetVolParmsInfoBuffer vp;
	DTPBRec dtp;
	char comment[256];
	
	/* read & verify header */
	fread((char *) &head, sizeof (head), 1, infile);
	if (head.magic != APPLESINGLE_MAGIC && head.magic != APPLEDOUBLE_MAGIC) {
		return (-1);
	}
	if (head.version != VERSION) {
		return (-1);
	}
	
	/* read entries */
	for (i = j = 0; i < head.entries; ++i) {
		fread((char *) (entries + j), sizeof (ap_entry), 1, infile);
		if (j < NUM_ENTRIES) switch (entries[j].id) {
			case ENT_NAME:
			case ENT_FINFO:
			case ENT_DATES:
			case ENT_COMMENT:
			case ENT_RFORK:
			case ENT_DFORK:
				++j;
				break;
		}
	}
	
	/* read name */
	for (i = 0; i < j && entries[i].id != ENT_NAME; ++i);
	if (i == j) return (-1);
	fseek(infile, entries[i].offset, SEEK_SET);
	if (entries[i].length > 63) entries[i].length = 63;
	*fspec->name = fread((char *) fspec->name + 1, sizeof (char), entries[i].length, infile);
	SetCursor(&arrow);
	NAputFile("\pSave decoded file as:", fspec->name, &reply);
	SetCursor(&watch);
	statrefresh();
	if (!reply.sfGood) return (didchat = -1);
	*fspec = reply.sfFile;

	/* create & get info for file */
	if (reply.sfReplacing) HDelete(fspec->vRefNum, fspec->parID, fspec->name);
	if (HCreate(fspec->vRefNum, fspec->parID, fspec->name, '????', '????') != noErr) {
		return (-1);
	}
	fpb = (HFileInfo *) &cipbr;
	fpb->ioVRefNum = fspec->vRefNum;
	fpb->ioNamePtr = fspec->name;
	fpb->ioDirID = fspec->parID;
	fpb->ioFDirIndex = 0;
	PBGetCatInfoSync(&cipbr);
	
	/* get finder info */
	for (i = 0; i < j && entries[i].id != ENT_FINFO; ++i);
	if (i < j) {
		fseek(infile, entries[i].offset, SEEK_SET);
		fread((char *) &fpb->ioFlFndrInfo, sizeof (FInfo), 1, infile);
		fread((char *) &fpb->ioFlXFndrInfo, sizeof (FXInfo), 1, infile);
		fpb->ioFlFndrInfo.fdFlags &= 0xf800; /* clear flags maintained by finder */
	}
	
	/* get file date info */
	for (i = 0; i < j && entries[i].id != ENT_DATES; ++i);
	if (i < j) {
		fseek(infile, entries[i].offset, SEEK_SET);
		fread((char *) &dates, sizeof (dates), 1, infile);
		fpb->ioFlCrDat = dates.create - CONVERT_TIME;
		fpb->ioFlMdDat = dates.modify - CONVERT_TIME;
		fpb->ioFlBkDat = dates.backup - CONVERT_TIME;
	}
	
	/* update info */
	fpb->ioDirID = fpb->ioFlParID;
	PBSetCatInfoSync(&cipbr);
	
	/* get comment & save it */
	for (i = 0; i < j && entries[i].id != ENT_COMMENT; ++i);
	if (i < j && entries[i].length != 0) {
		memset((void *) &vinfo, '\0', sizeof (vinfo));
		vinfo.ioVRefNum = fpb->ioVRefNum;
		vinfo.ioBuffer = (Ptr) &vp;
		vinfo.ioReqCount = sizeof (vp);
		if (PBHGetVolParmsSync((HParmBlkPtr) &vinfo) == noErr &&
			((vp.vMAttrib >> bHasDesktopMgr) & 1)) {
			memset((void *) &dtp, '\0', sizeof (dtp));
			dtp.ioVRefNum = fpb->ioVRefNum;
			if (PBDTGetPath(&dtp) == noErr) {
				if (entries[i].length > 255) entries[i].length = 255;
				fseek(infile, entries[i].offset, SEEK_SET);
				fread(comment, entries[i].length, 1, infile);
				dtp.ioDTBuffer = (Ptr) comment;
				dtp.ioNamePtr = fpb->ioNamePtr;
				dtp.ioDirID = fpb->ioDirID;
				dtp.ioDTReqCount = entries[i].length;
				if (PBDTSetCommentSync(&dtp) == noErr) {
					PBDTFlushSync(&dtp);
				}
			}
		}
	}
	
	/* do resource/data forks */
	for (i = 0; i < j; ++i) {
		if (entries[i].id == ENT_RFORK || entries[i].id == ENT_DFORK) {
			fseek(infile, entries[i].offset, SEEK_SET);
			if (entries[i].id == ENT_DFORK) {
				err = HOpen(fspec->vRefNum, fspec->parID, fspec->name, 2, &refnum);
			} else {
				err = HOpenRF(fspec->vRefNum, fspec->parID, fspec->name, 2, &refnum);
			}
			if (err != noErr) {
				HDelete(fspec->vRefNum, fspec->parID, fspec->name);
				return (-1);
			}
			while (entries[i].length > sizeof (copy_buf)) {
				count = fread(copy_buf, sizeof (char), sizeof (copy_buf), infile);
				entries[i].length -= count;
				FSWrite(refnum, &count, (Ptr) copy_buf);
			}
			count = fread(copy_buf, sizeof (char), entries[i].length, infile);
			FSWrite(refnum, &count, (Ptr) copy_buf);
			FSClose(refnum);
		}
	}
	
	return (0);
}
