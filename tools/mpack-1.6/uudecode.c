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
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include "xmalloc.h"
#include "common.h"
#include "part.h"

extern char *os_idtodir(char *id);
extern FILE *os_newtypedfile(char *fname, char *contentType, int flags, params contentParams);
extern FILE *os_createnewfile(char *fname);
extern int os_binhex(struct part *inpart, int part, int nparts);
extern void os_closetypedfile(FILE *outfile);
extern void os_donewithdir(char *dir);
extern void os_perror(char *str);
extern void chat(char *s);

extern void part_ungets(char *s, struct part *part);
extern void part_close(struct part *part);
extern int handleMessage(struct part *inpart, char *defaultContentType,
			 int inAppleDouble, int extractText);

static FILE *startDescFile(char *fname);
static void uudecodeline(char *line, FILE *outfile);

int parseSubject(char *subject, char **fnamep, int *partp, int *npartsp);
int saveUuFile(struct part *inpart, char *fname, int part, int nparts,
	       char *firstline);
int descEnd(char *line);
int uudecodefiles(char *dir, int nparts);

/* Length of a normal uuencoded line, including newline */
#define UULENGTH 62

/*
 * Table of valid boundary characters
 *
 * XXX: Old versions of Mark Crispin's c-client library
 * generate boundaries which contain the syntactically
 * illegal character '#'.  It is marked in this table with
 * a 2 in case we want to use this table in the future to
 * complain about bad syntax.
 *
 */
static char bchar[256] = {
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
     1, 0, 0, 2,  0, 0, 0, 1,  1, 1, 0, 1,  1, 1, 1, 1,
     1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 0,  0, 1, 0, 1,
     0, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,
     1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 0,  0, 0, 0, 1,
     0, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,
     1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 0,  0, 0, 0, 0,
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
     0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
};

/*
 * Read an input file, looking for data in split-uuencode format
 */
int handleUuencode(struct part *inpart, char *subject, int extractText)
{
    char *fname = 0, *tmpfname;
    int part, nparts;
    int tmppart, tmpnparts;
    char buf[1024], buf2[1024];
    char fnamebuf[80];
    char *boundary_end, *p;
    int wantdescfile = 0;
    FILE *descfile = 0;

    /* Scan "Subject:" header for filename/part information */
    if (parseSubject(subject, &fname, &part, &nparts) != 0) {
	part = -1;
    }
    if (part == 0) {
	return saveUuFile(inpart, fname, part, nparts, (char *)0);
    }
    if (part == 1) {
	wantdescfile = 1;
    }

    /* Scan body for interesting lines */
    while (part_gets(buf, sizeof(buf), inpart)) {
	/* Uuencode "begin" line */
	if (!strncmp(buf, "begin ", 6) &&
	    isdigit(buf[6]) && isdigit(buf[7]) && isdigit(buf[8]) &&
	    buf[9] == ' ') {
	    if (part == -1) {
		/*
		 * We have no part N of M information.  Perhaps it is
		 * a single-part uuencoded file.
		 */
		return saveUuFile(inpart, (char *)0, 1, 0, buf);
	    }
	    else {
		if (descfile) fclose(descfile);
		return saveUuFile(inpart, fname, part, nparts, buf);
	    }
	}
	else if (!strncmp(buf, "section ", 8) && isdigit(buf[8])) {
	    tmppart = 0;
	    for (p = buf+8; isdigit(*p); p++) tmppart = tmppart*10 + *p - '0';
	    if (tmppart == 0) continue;
	    if (strncmp(p, " of ", 4) == 0) {
		/*
		 * "section N of ... of file F ..."
		 */
		for (p += 4; *p && strncmp(p, " of file ", 9) != 0; p++);
		if (!*p) continue;
		p += 9;
		tmpfname = p;
		p = strchr(p, ' ');
		if (!p) continue;
		*p = '\0';
		if (descfile) fclose(descfile);
		return saveUuFile(inpart, tmpfname, tmppart, 0, (char *)0);
	    }
	    else if (*p == '/' && isdigit(p[1])) {
		/*
		 * "section N/M   file F ..."
		 */
		tmpnparts = 0;
		for (p++; isdigit(*p); p++) {
		    tmpnparts = tmpnparts*10 + *p - '0';
		}
		while (*p && isspace(*p)) p++;
		if (tmppart > tmpnparts || strncmp(p, "file ", 5) != 0) {
		    continue;
		}
		tmpfname = p+5;
		p = strchr(tmpfname, ' ');
		if (!p) continue;
		*p = '\0';
		if (descfile) fclose(descfile);
		return saveUuFile(inpart, tmpfname, tmppart, tmpnparts,
				  (char *)0);
	    }
	}
	else if (!strncmp(buf, "POST V", 6)) {
	    /*
	     * "POST Vd.d.d F (Part N/M)"
	     */
	    p = strchr(buf+6, ' ');
	    if (!p) continue;
	    tmpfname = p+1;
	    p = strchr(tmpfname, ' ');
	    if (!p || strncmp(p, " (Part ", 7) != 0) continue;
	    *p = '\0';
	    p += 7;
	    tmppart = 0;
	    while (isdigit(*p)) tmppart = tmppart*10 + *p++ - '0';
	    if (tmppart == 0 || *p++ != '/') continue;
	    tmpnparts = 0;
	    while (isdigit(*p)) tmpnparts = tmpnparts*10 + *p++ - '0';
	    if (tmppart > tmpnparts || *p != ')') continue;
	    if (descfile) fclose(descfile);
	    return saveUuFile(inpart, tmpfname, tmppart, tmpnparts, (char *)0);
	}
	else if (!strncmp(buf, "File: ", 6)) {
	    /*
	     * "File: F -- part N of M -- ...
	     */
	    tmpfname = buf+6;
	    p = strchr(tmpfname, ' ');
	    if (!p || strncmp(p, " -- part ", 9) != 0) continue;
	    *p = '\0';
	    p += 9;
	    tmppart = 0;
	    while (isdigit(*p)) tmppart = tmppart*10 + *p++ - '0';
	    if (tmppart == 0 || strncmp(p, " of ", 4) != 0) continue;
	    p += 4;
	    tmpnparts = 0;
	    while (isdigit(*p)) tmpnparts = tmpnparts*10 + *p++ - '0';
	    if (tmppart > tmpnparts || strncmp(p, " -- ", 4) != 0) continue;
	    if (descfile) fclose(descfile);
	    return saveUuFile(inpart, tmpfname, tmppart, tmpnparts, (char *)0);
	}
	else if (!strncmp(buf, "[Section: ", 10)) {
	    /*
	     * "[Section: N/M  File: F ..."
	     */
	    tmppart = 0;
	    for (p = buf+10; isdigit(*p); p++) tmppart = tmppart*10 + *p - '0';
	    if (tmppart == 0) continue;
	    tmpnparts = 0;
	    for (p++; isdigit(*p); p++) {
		tmpnparts = tmpnparts*10 + *p - '0';
	    }
	    while (*p && isspace(*p)) p++;
	    if (tmppart > tmpnparts || strncmp(p, "File: ", 6) != 0) {
		continue;
	    }
	    tmpfname = p+6;
	    p = strchr(tmpfname, ' ');
	    if (!p) continue;
	    *p = '\0';
	    if (descfile) fclose(descfile);
	    return saveUuFile(inpart, tmpfname, tmppart, tmpnparts, (char *)0);
	}
	else if (*buf == '[') {
	    /*
	     * "[F ... - part N of M]"
	     * (usual BinHex practice)
	     */
	    tmpfname = buf+1;
	    p = strchr(tmpfname, ' ');
	    if (!p) continue;
	    *p++ = '\0';
	    while (p && strncmp(p, "- part ", 7) != 0) {
		p = strchr(p+1, '-');
	    }
	    if (!p) continue;
	    p += 7;
	    tmppart = 0;
	    while (isdigit(*p)) tmppart = tmppart*10 + *p++ - '0';
	    if (tmppart == 0 || strncmp(p, " of ", 4) != 0) continue;
	    p += 4;
	    tmpnparts = 0;
	    while (isdigit(*p)) tmpnparts = tmpnparts*10 + *p++ - '0';
	    if (tmppart > tmpnparts || *p != ']') continue;
	    if (descfile) fclose(descfile);
	    return saveUuFile(inpart, tmpfname, tmppart, tmpnparts, (char *)0);
	}
	else if (fname && part > 0 && nparts > 0 && part <= nparts &&
		 (!strncmp(buf, "BEGIN", 5) ||
		  !strncmp(buf, "--- BEGIN ---", 12) ||
		  (buf[0] == 'M' && strlen(buf) == UULENGTH))) {
	    /*
	     * Found the start of a section of uuencoded data
	     * and have the part N of M information.
	     */
	    if (descfile) fclose(descfile);
	    return saveUuFile(inpart, fname, part, nparts, buf);
	}
	else if (!strncasecmp(buf, "x-file-name: ", 13)) {
	    for (p = buf + 13; *p && !isspace(*p); p++);
	    *p = '\0';
	    strncpy(fnamebuf, buf+13, sizeof(fnamebuf)-1);
	    fnamebuf[sizeof(fnamebuf)-1] = '\0';
	    fname = fnamebuf;
	    continue;
	}
	else if (!strncasecmp(buf, "x-part: ", 8)) {
	    tmppart = atoi(buf+8);
	    if (tmppart > 0) part = tmppart;
	    continue;
	}
	else if (!strncasecmp(buf, "x-part-total: ", 14)) {
	    tmpnparts = atoi(buf+14);
	    if (tmpnparts > 0) nparts = tmpnparts;
	    continue;
	}
	else if (part == 1 && fname && !descfile &&
		 !strncasecmp(buf, "x-file-desc: ", 13)) {
	    if ((descfile = startDescFile(fname))) {
		fputs(buf+13, descfile);
		fclose(descfile);
		descfile = 0;
	    }
	    continue;
	}
	else if (!strcmp(buf,
			 "(This file must be converted with BinHex 4.0)\n")) {
	    if (descfile) fclose(descfile);
	    return os_binhex(inpart, 1, 1);
	}
	else if (!strncasecmp(buf, "content-", 8)) {
	    /*
	     * HEURISTIC: If we see something that looks like a content-*
	     * header, push it back and call the message parser.
	     */
	    p = buf+8;
	    /* Check to see if header's field-name is syntactically valid */
	    while (*p) {
		if (*p == ':' || *p <= ' ' || *p >= '\177') break;
		p++;
	    }
	    if (*p == ':') {
		part_ungets(buf, inpart);
		if (descfile) fclose(descfile);
		return handleMessage(inpart, "text/plain", 0, extractText);
	    }
	}
	if (buf[0] == '-' && buf[1] == '-') {
	    /*
	     * Heuristic: If we see something that looks like a
	     * multipart boundary, followed by something that looks
	     * like a header, push them back and parse as a multipart.
	     */
	    p = buf+2;
	    while (*p) {
		if (!bchar[(unsigned char)*p]) break;
		p++;
	    }
	    if (*p != '\n') {
		/*
		 * We found an invalid boundary character.
		 * Move 'p' such that it will fail all subsequent checks.
		 */
		p = buf + 2;
	    }
	    /* Back up to ignore trailing whitespace */
	    while (p > buf+2 && p[-1] == ' ') p--;

	    /*
	     * Check that boundary is within legal size limits
	     * If so, peek at next line
	     */
	    if (p - buf > 2 && p - buf <= 72 &&
		part_gets(buf2, sizeof(buf2), inpart)) {
		boundary_end = p;
		p = buf2;
		/*
		 * Check to see if a syntactically valid header follows
		 * what looks to be a boundary.
		 *
		 * XXX: Unfortunately, we can't check for "Content-";
		 * it is syntactically valid to have a body-part
		 * header that doesn't start with that and ZMail
		 * takes advantage of that.  If this heuristic starts
		 * causing problems, we could keep looking ahead until
		 * we find a "Content-" header or find something that's
		 * not a header.
		 */
		while (*p) {
		    if (*p == ':' || *p <= ' ' || *p >= '\177') break;
		    p++;
		}
		
		/* Push back the lookahead line */
		part_ungets(buf2, inpart);

		if (p > buf2 && *p == ':') {
		    /* Push back the boundary */
		    part_ungets(buf, inpart);

		    /*
		     * Generate and push back a header to get us into
		     * the multipart parser.
		     */
		    *boundary_end = '\0';
		    sprintf(buf2,
			 "Content-type: multipart/mixed; boundary=\"%s\"\n\n",
			    buf+2);
		    part_ungets(buf2, inpart);
		    
		    if (descfile) fclose(descfile);
		    return handleMessage(inpart, "text/plain", 0, extractText);
		}
	    }
	}

	/*
	 * Save useful-looking text that is before a "part 1 of N"
	 * in a description file.
	 */
	if (wantdescfile && !descfile) {
	    for (p = buf; *p && isspace(*p); p++);
	    if (*p) {
		if (!strncasecmp(p, "x-", 2)) {
		    /*
		     * Check for "X-foobar:"
		     * If so, there probably will be a "X-File-Desc:" line
		     * later, so ignore this line.
		     */
		    while (*p != ':' && *p > ' ' && *p < '\177') p++;
		    if (*p == ':') continue;
		}
		if (!descEnd(buf) && (descfile = startDescFile(fname))) {
		    fputs(buf, descfile);
		}
		wantdescfile = 0;
	    }
	}
	else if (descfile) {
	    if (descEnd(buf)) {
		fclose(descfile);
		descfile = 0;
	    }
	    else {
		fputs(buf, descfile);
	    }
	}
    }	

    if (descfile) fclose(descfile);
    return 0;
}

/*
 * Handle a split-uuencode part
 * If nparts is 0, then look for an "end" line to detect the last part.
 * If fname is null, then we are attempting to decode a single-part message.
 * If firstline is non-null, it is written as the first line of the saved part
 */
int
saveUuFile(struct part *inpart, char *fname, int part, int nparts, char *firstline)
{
    char buf[1024];
    char *dir;
    FILE *partfile;

    if (fname) {
	sprintf(buf, "Saving part %d ", part);
	if (nparts) sprintf(buf+strlen(buf), "of %d ", nparts);
	strcat(buf, fname);
	chat(buf);
    }
    else fname = "unknown";

    /* Create directory to store parts and copy this part there. */
    dir = os_idtodir(fname);
    if (!dir) return 1;
    sprintf(buf, "%s%d", dir, part);
    partfile = os_createnewfile(buf);
        if (!partfile) {
	os_perror(buf);
	return 1;
    }
    if (firstline) fputs(firstline, partfile);
    while (part_gets(buf, sizeof(buf), inpart)) {
	fputs(buf, partfile);
	if (nparts == 0 && strcmp(buf, "end\n") == 0) {
	    /* This is the last part. Remember the fact */
	    nparts = part;
	    fclose(partfile);
	    sprintf(buf, "%sCT", dir);
	    partfile = os_createnewfile(buf);
	    if (!partfile) {
		os_perror(buf);
	    }
	    else {
		fprintf(partfile, "%d\n", nparts);
	    }
	    break;
	}
    }
    fclose(partfile);

    /* Retrieve any previously saved number of the last part */
    if (nparts == 0) {
	sprintf(buf, "%sCT", dir);
	if ((partfile = fopen(buf, "r"))) {
	    if (fgets(buf, sizeof(buf), partfile)) {
		nparts = atoi(buf);
		if (nparts < 0) nparts = 0;
	    }
	    fclose(partfile);
	}
    }

    if (nparts == 0) return 0;

    /* Check to see if we have all parts.  Start from the highest numbers
     * as we are more likely not to have them.
     */
    for (part = nparts; part; part--) {
	sprintf(buf, "%s%d", dir, part);
	partfile = fopen(buf, "r");
	if (partfile) {
	    fclose(partfile);
	}
	else {
	    return 0;
	}
    }

    return uudecodefiles(dir, nparts);
}

/*
 * Parse a Subject: header, looking for clues with which to decode
 * split-uuencoded data.
 */
int
parseSubject(char *subject, char **fnamep, int *partp, int *npartsp)
{
    char *scan, *bak, *start;
    int part = -1, nparts = 0, hasdot = 0;

    /* No subject header */
    if (!subject) return 1;

    /* Skip leading whitespace and other garbage */
    scan = subject;
    while (*scan == ' ' || *scan == '\t' || *scan == '-') scan++;
    if (!strncasecmp(scan, "repost", 6)) {
	for (scan += 6; *scan == ' ' || *scan == '\t'
	     || *scan == ':' || *scan == '-'; scan++);
    }

    /* Replies aren't usually data */
    if (!strncasecmp(scan, "re:", 3)) return 1;

    /* Get filename */

    /* Grab the first filename-like string.  Explicitly ignore strings with
     * prefix "v<digit>" ending in ":", since that is a popular volume/issue
     * representation syntax
     */
    do {
	while (*scan != '\n' && !isalnum(*scan) && *scan != '_') ++scan;
	*fnamep = start = scan;
	while (isalnum(*scan) || *scan == '-' || *scan == '+' || *scan == '&'
	       || *scan == '_' || *scan == '.') {
	    if (*scan++ == '.') hasdot = 1;
	}
	if (!*scan || *scan == '\n') return 1;
    } while (start == scan
	     || (start[0] == 'v' && isdigit(start[1]) && *scan == ':'));
    *scan++ = '\0';
    
    /* Try looking for a filename with a "." in it later in the subject line.
     * Exclude <digit>.<digit>, since that is usually a version number.
     */
    if (!hasdot) {
    	while (*(start = scan) != '\0' && *scan != '\n') {
	    while (isspace(*start)) ++start;
	    for (scan = start; isalnum(*scan) || *scan == '-' || *scan == '+'
		 || *scan == '&' || *scan == '_' || *scan == '.'; ++scan) {
		if (*scan == '.' && 
		    (!isdigit(scan[-1]) || !isdigit(scan[1]))) {
		    hasdot = 1;
		}
	    }
	    if (hasdot && scan > start) {
		*fnamep = start;
		*scan++ = '\0';
		break;
	    }
	    while (*scan && *scan != '\n' && !isalnum(*scan)) ++scan;
    	}
    	scan = *fnamep + strlen(*fnamep) + 1;
    }

    /* Get part number */
    while (*scan && *scan != '\n') {
	/* skip over versioning */
	if (*scan == 'v' && isdigit(scan[1])) {
	    ++scan;
	    while (isdigit(*scan)) ++scan;
	}
	/* look for "1/6" or "1 / 6" or "1 of 6" or "1-of-6" or "1o6" */
	if (isdigit(*scan) &&
	    (scan[1] == '/'
	     || (scan[1] == ' ' && scan[2] == '/')
	     || (scan[1] == ' ' && scan[2] == 'o' && scan[3] == 'f')
	     || (scan[1] == '-' && scan[2] == 'o' && scan[3] == 'f')
	     || (scan[1] == 'o' && isdigit(scan[2])))) {
	    while (isdigit(scan[-1])) scan--;
	    part = 0;
	    while (isdigit(*scan)) {
		part = part * 10 + *scan++ - '0';
	    }
	    while (*scan != '\0' && *scan != '\n' && !isdigit(*scan)) scan++;
	    if (isdigit(*scan)) {
		nparts = 0;
		while (isdigit(*scan)) {
		    nparts = nparts * 10 + *scan++ - '0';
		}
	    }
	    break;
	}

	/* look for "6 parts" or "part 1" */
	if (!strncasecmp("part", scan, 4)) {
	    if (scan[4] == 's') {
		for (bak = scan; bak >= subject && !isdigit(*bak); bak--);
		if (bak > subject) {
		    while (bak > subject && isdigit(bak[-1])) bak--;
		    nparts = 0;
		    while (isdigit(*bak)) {
			nparts = nparts * 10 + *bak++ - '0';
		    }
		}
	    } else {
		while (*scan && *scan != '\n' && !isdigit(*scan)) scan++;
		bak = scan - 1;
		if (isdigit(*scan)) {
		    part = 0;
		    do {
			part = part * 10 + *scan++ - '0';
		    } while (isdigit(*scan));
		}
		scan = bak;
	    }
	}
	scan++;
    }

    if (nparts == 0 || part == -1 || part > nparts) return 1;
    *partp = part;
    *npartsp = nparts;
    return 0;
}

/*
 * Return nonzero if 'line' should mark the end of a part-1 description
 */
int
descEnd(char *line)
{
    return !strncmp(line, "---", 3) ||
	!strncmp(line, "#!", 2) ||
	!strncasecmp(line, "part=", 5) ||
	!strncasecmp(line, "begin", 5);
}

/*
 * Open and return a file pointer for a description file for 'fname'.
 * If a description file for 'fname' already exists, or if there is an
 * error, return a null pointer.
 */
static FILE *startDescFile(char *fname)
{
    char buf[1024];
    char *dir;
    FILE *descfile;

    /* Create directory to store parts and copy this part there. */
    dir = os_idtodir(fname);
    if (!dir) return 0;
    sprintf(buf, "%s0", dir);

    /* See if part 0 already exists, return failure if so */
    descfile = fopen(buf, "r");
    if (descfile) {
	fclose(descfile);
	return 0;
    }

    descfile = os_createnewfile(buf);
    if (!descfile) {
	os_perror(buf);
	return 0;
    }
    return descfile;
}

/*
 * Decode the uuencoded file that is in 'nparts' pieces in 'dir'.
 */
int
uudecodefiles(char *dir, int nparts)
{
    int part;
    enum {st_start, st_inactive, st_decode, st_nextlast, st_last,
	    st_binhex} state;
    FILE *infile;
    FILE *outfile = NULL;
    struct part *inpart;
    char buf[1024];
    char lastline[UULENGTH+1];
    char *fname, *p;
    char *contentType = "application/octet-stream";
    int line_length = 0;

    /* If a part 0, copy to description filename */
    sprintf(buf, "%s0", dir);
    infile = fopen(buf, "r");
    if (infile) {
	outfile = os_createnewfile(TEMPFILENAME);
	if (outfile) {
	    while (fgets(buf, sizeof(buf), infile)) {
		fputs(buf, outfile);
	    }
	    fclose(outfile);
	    outfile = NULL;
	}
	fclose(infile);
	sprintf(buf, "%s0", dir);
	remove(buf);
    }

    state = st_start;

    /* Handle each part in order */
    for (part = 1; part <= nparts; part++) {
	sprintf(buf, "%s%d", dir, part);
	infile = fopen(buf, "r");
	if (!infile) {
	    os_perror(buf);
	    if (outfile) fclose(outfile);
	    remove(TEMPFILENAME);
	    return 1;
	}

	while (fgets(buf, sizeof(buf), infile)) {
	    switch (state) {
	    case st_start:	/* Looking for start of uuencoded
				 *  or binhex'ed file */
		if (!strcmp(buf,
			"(This file must be converted with BinHex 4.0)\n")) {
		    state = st_binhex;
		    inpart = part_init(infile);
		    os_binhex(inpart, part, nparts);
		    part_close(inpart);
		    goto endbinhex;
		}
		if (strncmp(buf, "begin ", 6)) break;
		/* skip mode */
		p = buf + 6;
		while (*p && !isspace(*p)) p++;
		while (*p && isspace(*p)) p++;
		fname = p;
		while (*p && !isspace(*p)) p++;
		*p = '\0';
		if (!*fname) return 1;

		/* Guess the content-type of common filename extensions */
		if ((p = strrchr(fname, '.'))) {
		    if (!strcasecmp(p, ".gif")) contentType = "image/gif";
		    if (!strcasecmp(p, ".jpg")) contentType = "image/jpeg";
		    if (!strcasecmp(p, ".jpeg")) contentType = "image/jpeg";
		    if (!strcasecmp(p, ".mpg")) contentType = "video/mpeg";
		    if (!strcasecmp(p, ".mpeg")) contentType = "video/mpeg";
		}

		/* Create output file and start decoding */
		outfile = os_newtypedfile(fname, contentType, FILE_BINARY,
					  (params) 0);
		if (!outfile) {
		    fclose(infile);
		    return 1;
		}
		state = st_decode;
		break;

	    case st_inactive:	/* Looking for uuencoded data to resume */
		if (*buf != 'M' || strlen(buf) != line_length) {
		    if (*buf == 'B' && !strncmp(buf, "BEGIN", 5)) {
			state = st_decode;
		    }
		    break;
		}
		state = st_decode;
		/* FALL THROUGH */
	    case st_decode:	/* Decoding data */
		if (line_length == 0) line_length = strlen(buf);
		if (*buf == 'M' && strlen(buf) == line_length) {
		    uudecodeline(buf, outfile);
		    break;
		}
		if (strlen(buf) > line_length) {
		    state = st_inactive;
		    break;
		}
		/*
		 * May be on nearing end of file.
		 * Save this line in case we are.
		 */
		strcpy(lastline, buf);
		if (*buf == ' ' || *buf == '`') {
		    state = st_last;
		}
		else {
		    state = st_nextlast;
		}
		break;

	    case st_nextlast:	/* May be nearing end of file */
		if (*buf == ' ' || *buf == '`') {
		    state = st_last;
		}
		else {
		    state = st_inactive;
		}
		break;

	    case st_last:	/* Should be at end of file */
		if (!strncmp(buf, "end", 3) && isspace(buf[3])) {
		    /* Handle that last line we saved */
		    uudecodeline(lastline, outfile);
		    fclose(infile);
		    os_closetypedfile(outfile);
		    for (;part <= nparts; part++) {
			sprintf(buf, "%s%d", dir, part);
			remove(buf);
		    }
		    sprintf(buf, "%sCT", dir);
		    remove(buf);
		    os_donewithdir(dir);
		    return 0;
		}
		state = st_inactive;
		break;

	    case st_binhex:
		if (strncmp(buf, "---", 3)) break;
		inpart = part_init(infile);
		os_binhex(inpart, part, nparts);
		part_close(inpart);
		goto endbinhex;
	    }
	}
	if (state != st_binhex) state = st_inactive;
	fclose(infile);
    endbinhex:
	sprintf(buf, "%s%d", dir, part);
	remove(buf);
    }
    if (outfile) os_closetypedfile(outfile);
    if (state == st_binhex) os_binhex(0, 0, 0);
    sprintf(buf, "%sCT", dir);
    remove(buf);
    os_donewithdir(dir);
    return 0;
}

#define DEC(c)	(((c) - ' ') & 077)

/*
 * Decode a uuencoded line to 'outfile'
 */
static void uudecodeline(char *line, FILE *outfile)
{
    int c, len;

    len = DEC(*line++);
    while (len) {
	c = DEC(*line) << 2 | DEC(line[1]) >> 4;
	putc(c, outfile);
	if (--len) {
	    c = DEC(line[1]) << 4 | DEC(line[2]) >> 2;
	    putc(c, outfile);
	    if (--len) {
		c = DEC(line[2]) << 6 | DEC(line[3]);
		putc(c, outfile);
		len--;
	    }
	}
	line += 4;
    }
}

    
