/*
 * Read MIME body-part, stopping on boundaries.
 */
/* (C) Copyright 1994 by Carnegie Mellon University
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

/* Max length of a MIME "boundary", per RFC 1521 */
#define PART_MAX_BOUNDARY_LEN 70

/* Structure describing an input file from which we read MIME */
struct part {
    /* Input file */
    FILE *infile;

    /* Input buffer */
    unsigned char *buf;
    int buf_alloc;
    unsigned char *ptr;
    int cnt;

    /* Boundary information */
    char (*boundary)[PART_MAX_BOUNDARY_LEN+1];
    int *boundary_length;
    int boundary_alloc;
    int boundary_num;
    int boundary_seen;		/* Index of boundary last seen, or
				 * boundary_num if no pending boundary
				 */
};

#define part_getc(s) (((s)->cnt-- > 0 && (s)->ptr[0] != '\n') ? (int)*(s)->ptr++ : part_fill(s))

#define part_ungetc(c, s) ((s)->cnt++, ((s)->boundary_seen = (s)->boundary_num), (*--(s)->ptr = (c)))

extern struct part *part_init(FILE *infile);
extern char *part_gets(char *s, int n, struct part *part);

