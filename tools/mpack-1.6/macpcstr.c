/* macpcstr.c -- niftyapp library pascal/C combination strings
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
/* (C) Copyright 1990, 1991 by Christopher J. Newman
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
 *
 *	Created 9/1/88, Assembly Code 6/27/90
 */

#ifdef THINK_C
typedef unsigned char PCstr;

/* assembler function prototypes */
void PtoPCstrcpy(void);
void CtoPCstrcpy(void);
void PCtoPCstrcpy(void);
void PtoPCstrncpy(void);
void CtoPCstrncpy(void);
void PtoPCstrcat(void);
void CtoPCstrcat(void);
PCstr *PtoPCstr(void);
PCstr *CtoPCstr(void);
void SetPlen(void);
PCstr *longtoPCstr(long);	/* not in assembler */

void PtoPCstrcpy( /* PCstr *dest, *src */ )
{
	asm 68000 {
		movea.l	8(sp),a0		; a0 = src
		movea.l	4(sp),a1		; a1 = dest
		clr.w	d0
		move.b	(a0),d0
		clr.b	1(a1,d0)
	@loop:
		move.b	(a0)+,(a1)+
		dbf.w	d0,@loop
	}
}

void CtoPCstrcpy( /* PCstr *dest, char *src */)
{
	asm 68000 {
		movea.l	8(sp),a0	; a0 = src
		movea.l	4(sp),a1	; a1 = dest
		addq.l	#1,a1
		moveq.l	#-1,d0
	@loop:
		addq.w	#1,d0
		move.b	(a0)+,(a1)+
		bne.s	@loop
		movea.l	4(sp),a1	; a1 = dest
		move.b	d0,(a1)
	}
}

void PCtoPCstrcpy( /* PCstr *dest, PCstr *src */)
{
	asm 68000 {
		movea.l	8(sp),a0	; a0 = src
		movea.l	4(sp),a1	; a1 = dest
		move.b	(a0)+,(a1)+
	@loop:
		move.b	(a0)+,(a1)+
		bne.s	@loop
	}
}

void PtoPCstrncpy( /* PCstr *dest, char *src, short n */)
{
	asm 68000 {
		movea.l	8(sp),a0	; a0 = src
		movea.l	4(sp),a1	; a1 = dest
		move.w	12(sp),d0	; d0 = n
		clr.w	d1
		move.b	(a0)+,d1
		cmp.w	d0,d1
		bcc.s	@skip
		move.w	d1,d0
	@skip:
		move.b	d0,(a1)+
		subq.w	#1,d0
		bcs.s	@exit
	@loop:
		move.b	(a0)+,(a1)+
		dbf		d0,@loop
	@exit:
	}
}

void CtoPCstrncpy( /* PCstr *dest, char *src, short n */ )
{
	asm 68000 {
		movea.l	8(sp),a0	; a0 = src
		movea.l	4(sp),a1	; a1 = dest
		addq.l	#1,a1
		clr.w	d1
		move.w	12(sp),d0	; d0 = n
		bra.s	@skip
	@loop:
		addq.w	#1,d1
		move.b	(a0)+,(a1)+
	@skip:
		dbeq.w	d0,@loop
		clr.b	(a1)
		movea.l	4(sp),a1	; a1 = dest
		subq.w	#1,d1
		move.b	d1,(a1)
	}
}

void PtoPCstrcat( /* PCstr *dest, char *src */ )
{
	asm 68000 {
		movea.l	8(sp),a0	; a0 = src
		movea.l	4(sp),a1	; a1 = dest
		clr.w	d0
		clr.w	d1
		move.b	(a0)+,d0
		move.b	(a1),d1
		add.b	d0,(a1)
		lea.l	1(a1,d1),a1
		bra.s	@skip
	@loop:
		move.b	(a0)+,(a1)+
	@skip:
		dbf.w	d0,@loop
		clr.b	(a1)
	}
}

void CtoPCstrcat( /* PCstr *dest, char *src */ )
{
	asm 68000 {
		movea.l	8(sp),a0	; a0 = src
		movea.l	4(sp),a1	; a1 = dest
		clr.w	d0
		move.b	(a1),d0
		lea.l	1(a1,d0),a1
		subq.w	#1,d0
	@loop:
		addq.w	#1,d0
		move.b	(a0)+,(a1)+
		bne.s	@loop
		movea.l	4(sp),a1	; a1 = dest
		move.b	d0,(a1)
	}
}

PCstr *PtoPCstr( /* char *str */ )
{
	asm 68000 {
		movea.l	4(sp),a0	; a0 = str
		clr.w	d0
		move.b	(a0),d0
		clr.b	1(a0,d0)
		move.l	a0,d0
	}
}

PCstr *CtoPCstr( /* char *str */)
{
	asm 68000 {
		movea.l	4(sp),a0	; a0 = str
		move.b	(a0)+,d0
		lea.l	(a0),a1
	@loop:
		move.b	(a1),d1
		move.b	d0,(a1)+
		move.b	d1,d0
		bne.s	@loop
		move.b	d0,(a1)
		suba.l	a0,a1
		move.l	a1,d0
		move.b	d0,-(a0)
		move.l	a0,d0
	}
}

void SetPlen( /* PCstr *pcstr */ )
{
	asm 68000 {
		movea.l	4(sp),a0	; a0 = str
		lea.l	1(a0),a1
		moveq.l	#-1,d0
	@loop:
		addq.w	#1,d0
	@skip:
		tst.b	(a1)+
		bne.s	@loop
		move.b	d0,(a0)
	}
}
#else
/* C function prototypes in mac_napp.h */
#include "macnapp.h"

void PtoPCstrcpy(dest, src)
	register PCstr	*dest;
	register char	*src;
{
	register short	i;
	
	i = Pstrlen(src);
	C(dest)[i] = '\0';
	do {
		*dest++ = *src++;
	} while (i--);
}

void CtoPCstrcpy(dest, src)
	register PCstr	*dest;
	register char	*src;
{
	register short	i;
	register char	*cpy;

	cpy = C(dest);
	for (i = 0; *cpy++ = *src++; i++);
	*dest = i;
}

void PCtoPCstrcpy(dest, src)
	register PCstr	*dest;
	register PCstr	*src;
{
	*dest++ = *src++;
	while (*dest++ = *src++);
}

void PtoPCstrncpy(PCstr *dest, char *src, short n)
{
	if (Pstrlen(src) < n) n = Pstrlen(src);
	*dest++ = n;
	src++;
	while (n--) *dest++ = *src++;
	*dest++ = '\0';
}

void CtoPCstrncpy(PCstr *dest, char *src, short n)
{
	register char	*tmp;
	register short	i;
	
	tmp = C(dest);
	for (i = 0; n-- && (*tmp++ = *src++); i++);
	*tmp = '\0';
	*dest = i;
}

void PtoPCstrcat(dest, src)
	register PCstr	*dest;
	register char	*src;
{
	register short	i;
	register short	j;
	
	i = *dest;
	*dest += (j = (unsigned char) *src++);
	dest += i + 1;
	while (j--) *dest++ = *src++;
	*dest = '\0';
}

void CtoPCstrcat(dest, src)
	register PCstr	*dest;
	register char	*src;
{
	register short	i;
	register char	*tmp;
	
	tmp = (char *) dest + (i = *dest) + 1;
	while (*tmp++ = *src++) i++;
	*dest = i;
}

PCstr *PtoPCstr(str)
	register char	*str;
{
	SetClen((PCstr*) str);

	return ((PCstr*) str);
}

PCstr *CtoPCstr(str)
	register char	*str;
{
	register PCstr	i;
	register char	c, d;
	register char	*tmp;
	
	i = 0;
	tmp = str;
	tmp++;
	c = *tmp++;
	do {
		d = *tmp;
		*tmp++ = c;
		i++;
	} while (c = d);
	(*(PCstr*)str) = i;
		
	return ((PCstr*) str);
}

void SetPlen(pcstr)
	register PCstr	*pcstr;
{
	register short	i = -1;
	register char	*len = C(pcstr);
	
	do {
		i++;
	} while (*len++);
	
	*pcstr = i;
}
#endif

/* simple procedure to convert decimal number of 
 * less than 20 digits to a PC string.
 * Compiling with 68020 option makes this quite a bit more efficient.
 */
PCstr *longtoPCstr(i)
    register long i;
{
    static PCstr   			sbuf[21];
    register Boolean	   	negflag;
    register unsigned long 	val, ten = 10;
    register PCstr 			*pos = sbuf + sizeof (sbuf) - 1;
    register PCstr 			*posst;

    *pos = '\0';
    posst = --pos;
    negflag = false;
    val = i;
    if (i < 0) {
    	negflag = true;
    	val = -i;
    }
    do {
		*pos = (unsigned short) (val % ten) + '0';
		pos--;
    } while (val /= ten);
    if (negflag) {
    	*pos = '-';
    	pos--;
    }
	*pos = posst - pos;

    return (pos);
}

