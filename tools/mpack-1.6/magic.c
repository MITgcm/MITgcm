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
#include <string.h>

/* Description of the various file formats and their magic numbers */
struct magic {
    char *name;			/* Name of the file format */
    char *num;		/* The magic number */
    int len;		/* Length of same (0 means strlen(magicnum)) */
};

/* The magic numbers of the file formats we know about */
static struct magic magic[] = {
    { "image/gif", "GIF", 0 },
    { "image/jpeg", "\377\330\377", 0 },
    { "video/mpeg", "\0\0\001\263", 4 },
    { "application/postscript", "%!", 0 },
};
static int num_magic = (sizeof(magic)/sizeof(magic[0]));
static char *default_type = "application/octet-stream";

/* The longest magic number */
static int max_magiclen = 0;

/*
 * Determins the format of the file "inputf".  The name
 * of the file format (or NULL on error) is returned.
 */
char *magic_look(FILE *infile)
{
    int i, j;
    char buf[80];
    int numread = 0;

    if (max_magiclen == 0) {
	for (i=0; i<num_magic; i++) {
	    if (magic[i].len == 0) magic[i].len = strlen(magic[i].num);
	    if (magic[i].len > max_magiclen) max_magiclen = magic[i].len;
	}
    }

    numread = fread(buf, 1, max_magiclen, infile);
    rewind(infile);

    for (i=0; i<num_magic; i++) {
	if (numread >= magic[i].len) {
	    for (j=0; j<magic[i].len; j++) {
		if (buf[j] != magic[i].num[j]) break;
	    }
	    if (j == magic[i].len) return magic[i].name;
	}
    }

    return default_type;
}
