/* macbhex.c -- simple binhex decoding routine */
/* (C) Copyright 1993,1994 by Carnegie Mellon University
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

#include <stdio.h>
#include <ctype.h>
#include <memory.h>
#include "part.h"
#include "macnapp.h"
#include "macmpack.h"

/* from macos.c: */
extern void renameDescFile(char *, short, long);

char binhex_decode[256] = {
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -1, -1,
	13, 14, 15, 16, 17, 18, 19, -1, 20, 21, -1, -1, -1, -1, -1, -1,
	22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, -1,
	37, 38, 39, 40, 41, 42, 43, -1, 44, 45, 46, 47, -1, -1, -1, -1,
	48, 49, 50, 51, 52, 53, 54, -1, 55, 56, 57, 58, 59, 60, -1, -1,
	61, 62, 63, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};
#define BHEXVAL(c) (binhex_decode[(unsigned char) c])

typedef union {
	unsigned char c[4];
	unsigned long val;
} longbuf;

typedef struct {
	OSType type, creator;
	unsigned short flags;
	long dlen, rlen;
} binhex_header;

#define STATE_START  0
#define STATE_FNAME  1
#define STATE_HEADER 2
#define STATE_HCRC   3
#define STATE_DFORK  4
#define STATE_DCRC   5
#define STATE_RFORK  6
#define STATE_RCRC   7
#define STATE_DONE   8
#define STATE_ERROR  9

typedef struct binhex_state {
	short state;			/* current state */
	short part;				/* current part number */
	unsigned short CRC;		/* cumulative CRC */
	unsigned short fileCRC;	/* CRC value from file */
	longbuf octetbuf;		/* buffer for decoded 6-bit values */
	short octetin;			/* current input position in octetbuf */
	short donepos;			/* ending position in octetbuf */
	short inCRC;			/* flag set when reading a CRC */
	long count;				/* generic counter */
	FILE *outfile;			/* output file */
	short marker;			/* flag indicating maker */
	unsigned char rlebuf;	/* buffer for last run length encoding value */
	PCstr namebuf[65];		/* buffer for binhex filename */
	binhex_header head;		/* buffer for header */
	FSSpec fspec;			/* output file */
} binhex_state;

/* global state */
static binhex_state bh;

/* process a binhex character
 */
static void binhex_process(struct part *inpart)
{
	unsigned short tmpcrc, cval;
	unsigned char ctmp, c = bh.rlebuf;
	StandardFileReply reply;
	FInfo finfo;
	char buf[256];
	
	/* do CRC */
	ctmp = bh.inCRC ? c : 0;
	cval = bh.CRC & 0xf000;
	tmpcrc = ((unsigned short) (bh.CRC << 4) | (ctmp >> 4))
			^ (cval | (cval >> 7) | (cval >> 12));
	cval = tmpcrc & 0xf000;
	bh.CRC = ((unsigned short) (tmpcrc << 4) | (ctmp & 0x0f))
			^ (cval | (cval >> 7) | (cval >> 12));

	/* handle state */
	switch (bh.state) {
		case STATE_START:
			bh.state = STATE_FNAME;
			bh.count = 1;
			*bh.namebuf = (c & 63);
			break;
		case STATE_FNAME:
			bh.namebuf[bh.count] = c;
			if (bh.count++ > *bh.namebuf) {
				bh.state = STATE_HEADER;
				bh.count = 0;
			}
			break;
		case STATE_HEADER:
			((char *)&bh.head)[bh.count] = c;
			if (++bh.count == 18) {
				bh.state = STATE_HCRC;
				bh.inCRC = 1;
				bh.count = 0;
			}
			break;
		case STATE_DFORK:
		case STATE_RFORK:
			putc(c, bh.outfile);
			if (--bh.count == 0) {
				fclose(bh.outfile);
				bh.outfile = NULL;
				++bh.state;
				bh.inCRC = 1;
			}
			break;
		case STATE_HCRC:
		case STATE_DCRC:
		case STATE_RCRC:
			if (!bh.count++) {
				bh.fileCRC = (unsigned short) c << 8;
			} else {
				if ((bh.fileCRC | c) != bh.CRC) {
					if (bh.state > STATE_HCRC) {
						HDelete(bh.fspec.vRefNum, bh.fspec.parID, bh.fspec.name);
						SetCursor(&arrow);
						yell("BinHex file corrupted in transit");
						SetCursor(&watch);
					}
					bh.state = STATE_ERROR;
					break;
				}
				bh.CRC = 0;
				if (++bh.state == STATE_DONE) {
					finfo.fdType = bh.head.type;
					finfo.fdCreator = bh.head.creator;
					finfo.fdFlags = bh.head.flags & 0xf800;
					HSetFInfo(bh.fspec.vRefNum, bh.fspec.parID, bh.fspec.name, &finfo);
					PtoCstr(bh.fspec.name);
					renameDescFile((char *)bh.fspec.name, bh.fspec.vRefNum, bh.fspec.parID);
					break;
				}
				bh.count = bh.head.rlen;
				if (bh.state == STATE_DFORK) {
					/* prompt user */
					sprintf(buf, "Saving BinHex file %s", C(bh.namebuf));
					chat(buf);
					SetCursor(&arrow);
					NAputFile("\pSave decoded BinHex file as:", P(bh.namebuf), &reply);
					SetCursor(&watch);
					statrefresh();
					if (!reply.sfGood) {
						didchat = -1;
						bh.state = STATE_ERROR;
					} else {
						bh.fspec = reply.sfFile;
						HCreate(bh.fspec.vRefNum, bh.fspec.parID, bh.fspec.name,
							bh.head.creator, bh.head.type);
						bh.count = bh.head.dlen;
					}
				}
				if (bh.count) {
					bh.inCRC = 0;
					bh.outfile = Macopen(inpart->infile, bh.fspec.name, bh.fspec.vRefNum,
						bh.fspec.parID, 1, bh.state == STATE_DFORK ? 0 : 1, fsWrPerm);
					if (!bh.outfile) {
						bh.state = STATE_ERROR;
						HDelete(bh.fspec.vRefNum, bh.fspec.parID, bh.fspec.name);
						SetCursor(&arrow);
						yell("Failed to open file for writing");
						SetCursor(&watch);
					}
				} else {
					++bh.state;
				}
			}
			break;
	}
}

/*
 * decode a binhex file
 *  returns -1 on fatal error, 0 for continue, 1 for done
 */
int os_binhex(struct part *inpart, int part, int nparts)
{
	long val;
	int c;
	char *bptr;
	short octetpos;
	static char buf[1024];
	
	/* reset state */
	if (part == 1) {
		bh.state = STATE_START;
		bh.part = 0;
		bh.CRC = 0;
		bh.octetbuf.val = 0;
		bh.octetin = 26;
		bh.donepos = 3;
		bh.inCRC = 0;
		bh.outfile = NULL;
		bh.marker = 0;
	}
	if (++bh.part != part) bh.state = STATE_ERROR;
	
	/* do nothing on error/completion */
	if (!inpart) {
		if (bh.state < STATE_DONE) bh.state = STATE_ERROR;
	} else {
		/* skip blank lines */
		do {
			if (part_gets(buf, sizeof (buf), inpart) == NULL) return (0);
		} while (*buf == '\n');
		bptr = buf;
		if (part == 1 && *bptr++ != ':') bh.state = STATE_ERROR;
		
		/* line reading loop */
		do {
			/* check line for separator */
			if (!strncmp(buf, "--- ", 4)) break;
			buf[strlen(buf) - 1] = '\0';
			
			/* loop through line of binhex charaters */
			while (bh.state < STATE_DONE) {
				/* fill in octetbuf */
				do {
					if ((val = BHEXVAL(*bptr++)) == -1) {
						if (bptr[-1]) {
							--bh.donepos;
							if (bh.octetin >= 14) --bh.donepos;
							if (bh.octetin >= 20) --bh.donepos;
						}
						break;
					}
					bh.octetbuf.val |= val << bh.octetin;
				} while ((bh.octetin -= 6) > 2);
				if (!bptr[-1]) break;
				
				/* handle decoded characters -- run length encoding (rle) detection */
				for (octetpos = 0; octetpos < bh.donepos; ++octetpos) {
					/* get character & handle rle */
					c = bh.octetbuf.c[octetpos];
					if (c == 0x90 && !bh.marker++) continue;
					if (bh.marker) {
						if (c == 0) {
							bh.rlebuf = 0x90;
							binhex_process(inpart);
						} else {
							while (--c > 0) {
								binhex_process(inpart);
							}
						}
						bh.marker = 0;
					} else {
						bh.rlebuf = (unsigned char) c;
						binhex_process(inpart);
					}
					if (bh.state >= STATE_DONE) break;
				}
				if (bh.donepos < 3 && bh.state < STATE_DONE) bh.state = STATE_ERROR;
				bh.octetin = 26;
				bh.octetbuf.val = 0;
			}
		} while (bh.state < STATE_DONE && part_gets(bptr = buf, sizeof (buf), inpart) != NULL);
	}
	
	/* error clean up */
	if (bh.state == STATE_ERROR && bh.outfile) {
		fclose(bh.outfile);
		bh.outfile = NULL;
		HDelete(bh.fspec.vRefNum, bh.fspec.parID, bh.fspec.name);
	}
	
	return (bh.state == STATE_ERROR ? 1 : 0);
}
