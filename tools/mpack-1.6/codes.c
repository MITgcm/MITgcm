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
/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.  */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "xmalloc.h"
#include "md5.h"

void output64chunk(int c1, int c2, int c3, int pads, FILE *outfile);
static char basis_64[] =
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

int to64(FILE *infile, FILE *outfile, long int limit)
{
    int c1, c2, c3, ct=0, written=0;

    if (limit && limit < 73) return 1;

    while ((c1 = getc(infile)) != EOF) {
        c2 = getc(infile);
        if (c2 == EOF) {
            output64chunk(c1, 0, 0, 2, outfile);
        } else {
            c3 = getc(infile);
            if (c3 == EOF) {
                output64chunk(c1, c2, 0, 1, outfile);
            } else {
                output64chunk(c1, c2, c3, 0, outfile);
            }
        }
        ct += 4;
        if (ct > 71) {
            putc('\n', outfile);
	    if (limit) {
		limit -= ct + 1;
		if (limit < 73) return 1;
	    }
	    written += 73;
            ct = 0;
        }
    }
    if (ct) {
	putc('\n', outfile);
	ct++;
    }
    return written + ct;
}

void output64chunk(int c1, int c2, int c3, int pads, FILE *outfile)
{
    putc(basis_64[c1>>2], outfile);
    putc(basis_64[((c1 & 0x3)<< 4) | ((c2 & 0xF0) >> 4)], outfile);
    if (pads == 2) {
        putc('=', outfile);
        putc('=', outfile);
    } else if (pads) {
        putc(basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
        putc('=', outfile);
    } else {
        putc(basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
        putc(basis_64[c3 & 0x3F], outfile);
    }
}

char *md5contextTo64(MD5_CTX *context)
{
    unsigned char digest[18];
    char encodedDigest[25];
    int i;
    char *p;

    MD5Final(digest, context);
    digest[sizeof(digest)-1] = digest[sizeof(digest)-2] = 0;

    p = encodedDigest;
    for (i=0; i < sizeof(digest); i+=3) {
	*p++ = basis_64[digest[i]>>2];
	*p++ = basis_64[((digest[i] & 0x3)<<4) | ((digest[i+1] & 0xF0)>>4)];
	*p++ = basis_64[((digest[i+1] & 0xF)<<2) | ((digest[i+2] & 0xC0)>>6)];
	*p++ = basis_64[digest[i+2] & 0x3F];
    }
    *p-- = '\0';
    *p-- = '=';
    *p-- = '=';
    return strsave(encodedDigest);
}    

char *md5digest(FILE *infile, long int *len)
{
    MD5_CTX context;
    char buf[1000];
    long length = 0;
    int nbytes;
    
    MD5Init(&context);
    while ((nbytes = fread(buf, 1, sizeof(buf), infile))) {
	length += nbytes;
	MD5Update(&context, buf, nbytes);
    }
    rewind(infile);
    if (len) *len = length;
    return md5contextTo64(&context);
}
