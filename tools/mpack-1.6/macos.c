/* macos.c -- operating system dependant mpack code for the Macintosh
 */
/* (C) Copyright 1993-1995 by Carnegie Mellon University
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
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "common.h"
#include "macnapp.h"
#include "macmpack.h"

extern char *malloc(), *realloc();

char copy_buf[COPY_BUFSIZE];

char *xmalloc(unsigned size)
{
    char *ret;

    if (ret = malloc((unsigned) size))
      return ret;

    yell("Not enough memory available.");
    maccleanup();
    exit(1);
}

char *xrealloc (char *ptr, unsigned size)
{
    char *ret;

    /* xrealloc (NULL, size) behaves like xmalloc (size), as in ANSI C */
    if (ret = !ptr ? malloc ((unsigned) size) : realloc (ptr, (unsigned) size))
      return ret;

    yell("Not enough memory available");
    maccleanup();
    exit(1);
}

char *strsave(char *str)
{
    char *p = xmalloc(strlen(str)+1);
    strcpy(p, str);
    return p;
}

/* save output filename buffer */
static char *output_fname = NULL;
static short applefile_flag = 0;
static FSSpec fspec;

/* rename or copy a file to a new location
 */
static void rename_or_copy(srcfname, dstfname, dstvol, dirid)
	char *srcfname;
	PCstr *dstfname;
	short dstvol;
	long dirid;
{
	PCstr tstr[257];
	short srefnum, drefnum;
	long count;
	WDPBRec wdpb;
	HParamBlockRec hpb;
	CMovePBRec cpb;
	
	wdpb.ioNamePtr = P(tstr);
	if (PBHGetVol(&wdpb, FALSE) != noErr) return;
	CtoPCstrcpy(tstr, srcfname);
	if (HOpen(0, 0, P(tstr), fsRdPerm, &srefnum) != noErr) return;
	if (GetEOF(srefnum, &count) == noErr && !count) {
		FSClose(srefnum);
		return;
	}
	if (wdpb.ioWDVRefNum == dstvol) {
		/* files on same drive -- do a rename/move */
		FSClose(srefnum);
		hpb.fileParam.ioNamePtr = P(tstr);
		hpb.fileParam.ioVRefNum = 0;
		hpb.ioParam.ioMisc = (Ptr) P(dstfname);
		hpb.fileParam.ioDirID = 0;
		if (PBHRenameSync(&hpb) == noErr && wdpb.ioWDDirID != dirid) {
			cpb.ioNamePtr = P(dstfname);
			cpb.ioVRefNum = 0;
			cpb.ioNewName = NULL;
			cpb.ioNewDirID = dirid;
			cpb.ioDirID = 0;
			PBCatMoveSync(&cpb);
		}
	} else {
		/* files on different drive -- do a copy */
		if (HCreate(dstvol, dirid, P(dstfname), _fcreator, _ftype) != noErr) {
			FSClose(srefnum);
			return;
		}
		if (HOpen(dstvol, dirid, P(dstfname), fsWrPerm, &drefnum) != noErr) {
			FSClose(srefnum);
			HDelete(dstvol, dirid, P(dstfname));
			return;
		}
		count = sizeof (copy_buf);
		while (FSRead(srefnum, &count, (Ptr) copy_buf) != noErr && count > 0) {
			FSWrite(drefnum, &count, (Ptr) copy_buf);
			count = sizeof (copy_buf);
		}
		FSClose(srefnum);
		FSClose(drefnum);
	}
}

/* Generate a message-id */
char *os_genid()
{
    char *result;
    long tick;
    unsigned long time;

	tick = TickCount();
    result = malloc(64);
    GetDateTime(&time);
    HLock((Handle) mpack_prefs);
    sprintf(result, "%lu.%lu@%s", tick, time, (*mpack_prefs)->internet_host);
    HUnlock((Handle) mpack_prefs);
    
    /* make sure tick count is bumped before we return... */
    while (tick == TickCount());
    
    return (result);
}

/* Create and return directory for a message-id
 */
char *os_idtodir(id)
char *id;
{
	static PCstr buf[257];
	PCstr idbuf[257];
	char *fname;
    short uqid;
    OSErr err;
    long dirID;
    Handle h;
    ResType type = 'IDna';

	/* prepare filename */
	PtoPCstrcpy(buf, (char *) P(pfolder->prefs));
	fname = C(buf) + PCstrlen(buf);
	
	/* is there a mapping for the id? */
	CtoPCstrcpy(idbuf, id);
	h = GetNamedResource(type, P(idbuf));
	
	/* no mapping -- create one */
	if (!h) {
		while ((uqid = UniqueID(type)) < 128);
		h = NewHandle(sizeof (short));
		if (h) (**(short **)h) = uqid;
		AddResource(h, type, uqid, P(idbuf));
		if ((err = ResError()) != noErr) {
			return (NULL);
		}
	} else {
		uqid = ** (short **) h;
	}
	
	/* set directory name & create it */
	sprintf(fname, "%d:", uqid);
	SetPlen(buf);
	err = DirCreate(pfolder->fspec.vRefNum, 0, P(buf), &dirID);
	if (err != noErr && err != dupFNErr) {
		RmveResource(h);
		DisposHandle(h);
		h = NULL;
	}
	
	return (h ? C(buf) : NULL);
}

/*
 * We are done with the directory returned by os_idtodir()
 * Remove it
 */
os_donewithdir(dir)
char *dir;
{
	PCstr buf[257];
	short uqid;
	char *fname;
	Handle h;
	
	CtoPCstrcpy(buf, dir);
	HDelete(0, 0, P(buf));
	fname = strrchr(C(buf), ':');
	while (fname > C(buf) && *--fname != ':');
	if (fname > C(buf)) {
		uqid = atoi(fname + 1);
		h = GetResource('IDna', uqid);
		if (h) {
			RmveResource(h);
			DisposHandle(h);
		}
	}
}

/* rename the description file
 */
void renameDescFile(char *fname, short vRefNum, long dirid)
{
	PCstr tstr[65];
	char *p;

    MapTypeCreator("text/plain", 0);

	/* save description file */
	CtoPCstrcpy(tstr, fname);
    if (p = strrchr(C(tstr), '.')) *p = '\0';
    strcat(C(tstr), ".desc");
    SetPlen(tstr);
    rename_or_copy(TEMPFILENAME, tstr, vRefNum, dirid);
    (void) remove(TEMPFILENAME);
}

FILE *os_createnewfile(fname) 
char *fname;
{
    return fopen(fname, "w");
}

/*
 * Create a new file, with suggested filename "fname".
 * "fname" may have come from an insecure source, so clean it up first.
 * It may also be null.
 * "contentType" is passed in for use by systems that have typed filesystems.
 * "flags" contains a bit pattern describing attributes of the new file.
 */
FILE *os_newtypedfile(fname, contentType, flags, contentParams)
char *fname;
char *contentType;
int flags;
params *contentParams;
{
    char *p;
    int applefile;
    FILE *outfile = 0, *tmpf;
    StandardFileReply reply;
    PCstr tstr[257];

    if (!fname) fname = "";
    
    /* Translate ':' to underscore */
    for (p=fname; *p; p++) {
    	if (*p == ':' || !isprint(*p)) *p = '_';
    }
    
    /* chop filename to length */
    if (strlen(fname) > 31) {
		fname += strlen(fname) - 31;
    }
    
    /* remove leading periods, to protect from ".SONY" attacks */
    while (*fname == '.') ++fname;

	/* get filename from user */
	applefile = !strcmp(contentType, "application/applefile");
	if (!applefile || !(flags & FILE_INAPPLEDOUBLE)) {
		sprintf(C(tstr), "Found file: %s (%s)",
			fname[0] ? fname : "Untitled", contentType);
		SetPlen(tstr);
		stattext(P(tstr), 1);
	}
	if (!applefile && (!applefile_flag || !(flags & FILE_INAPPLEDOUBLE))) {
	    SetCursor(&arrow);
	    CtoPstr(fname);
	    NAputFile("\pSave decoded file as:",
	    	fname[0] ? (unsigned char *) fname : "\pUntitled", &reply);
	    PtoCstr((unsigned char *) fname);
	    SetCursor(&watch);
	    statrefresh();
	    if (!reply.sfGood) {
	    	didchat = -1;
	    	return (NULL);
	    }
	}

	/* set the type */
	MapTypeCreator(contentType, 0);

	/* save file */
	tmpf = tmpfile();
	if (applefile) {
		outfile = tmpf;
		tmpf = NULL;
	} else if ((flags & FILE_INAPPLEDOUBLE) && applefile_flag) {
		outfile = Macopen(tmpf, fspec.name, fspec.vRefNum, fspec.parID,
			flags & FILE_BINARY, 0, fsWrPerm);
	} else {
		HCreate(reply.sfFile.vRefNum, reply.sfFile.parID, reply.sfFile.name, _fcreator, _ftype);
	    outfile = Macopen(tmpf, reply.sfFile.name, reply.sfFile.vRefNum, reply.sfFile.parID,
	    	flags & FILE_BINARY, 0, fsWrPerm);
	}
	applefile_flag = applefile;
	if (tmpf) fclose(tmpf);
	PtoCstr(reply.sfFile.name);
	fname = (char *) reply.sfFile.name;
    if (!outfile) {
    	sprintf(C(tstr), "Couldn't open file %s", fname);
    	warn(C(tstr));
    	return (0);
    }
	
    if (output_fname) free(output_fname);
    output_fname = strsave(fname);
    
    renameDescFile(fname, reply.sfFile.vRefNum, reply.sfFile.parID);

    return outfile;
}

/*
 * Close a file opened by os_newTypedFile()
 */
os_closetypedfile(outfile)
FILE *outfile;
{
	char buf[128];
	
	if (applefile_flag) {
		rewind(outfile);
		if (decode_applefile(outfile, &fspec) < 0) {
			sprintf(buf, "Failed to decode file %s", output_fname);
			if (didchat >= 0) warn(buf);
			applefile_flag = 0;
		}
	}

	/* close file */
    fclose(outfile);
}

/*
 * Warn user that the MD5 digest of the last file created by os_newtypedfile()
 * did not match that supplied in the Content-MD5: header.
 */
os_warnMD5mismatch()
{
    char *warning;

    warning = xmalloc(strlen(output_fname) + 100);
    sprintf(warning, "%s was corrupted in transit",
	    output_fname);
    warn(warning);
    free(warning);
}

/* bring up an error dialog for a file error
 */
void os_perror(char *str)
{
	extern int errno;
	char *err = strerror(errno), *scan;
	char msg[256];
	short maxflen;
	
	maxflen = 255 - (strlen(err) + 2);
	if (strlen(str) > maxflen) {
		str += strlen(str) - maxflen;
		for (scan = str; *scan && *scan++ != ':';);
		if (*scan) str = scan;
	}
	sprintf(msg, "%s: %s", str, err);
	yell(msg);
}
