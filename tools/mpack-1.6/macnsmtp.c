/* macnsmtp.c -- simple async SMTP client
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
 *
 * Author:	Christopher J. Newman
 * Message:	This is a nifty program.
 */
 
#include "macnapp.h"

#define SMTP_PORT 25

typedef struct {
 	na_win w;
 	void *context;		/* user context */
 	short num, rcpt;	/* recipient count (num), and sent (rcpt) */
 	na_smtpstat statf;	/* callback */
 	na_tcp tcpid;		/* TCP id */
 	short state;		/* SMTP state (see below) */
 	short count;		/* bytes used in linebuf */
 	short refnum;		/* input file */
 	short crfound:1;	/* found a CR in SMTP server data */
 	short crlf:1;		/* found a CRLF in SMTP server data */
 	short crtrans:1;	/* translate CR to CRLF in file */
 	long headsize;		/* size of extra headers starting at data */
 	long fsize, fdone;	/* file size & amount written */
 	long bufsize;		/* usable buffer size */
 	Ptr buf;			/* output buffer */
 	char linebuf[1024];	/* input line buffer */
 	char data[1];		/* header & envelope */
} na_smtpwin;
 
#define sw ((na_smtpwin *) w)

/* states: */
#define S_conn  0  /* connecting to server */
#define S_greet 1  /* waiting for greeting */
#define S_hello 2  /* waiting for local host lookup and HELO reply */
#define S_mailf 3  /* waiting for MAIL FROM reply */
#define S_rcpt  4  /* waiting for RCPT TO reply */
#define S_data  5  /* waiting for DATA continue reply */
#define S_send  6  /* transmitting data */
#define S_done  7  /* waiting for DATA success reply */
#define S_quit  8  /* waiting for QUIT reply */
#define S_wait  9  /* waiting for connection close */
#define S_close 10 /* closed */

/* generate and submit SMTP command line (put command & data together with CRLF ending)
 * returns NATCPwrite result code
 */
static int SMTPsend(na_win *w, char *com, char *data)
{
	char buf[512];
	char *dest = buf;
	int result = 0;
	
	while ((*dest = *com) != '\0') ++dest, ++com;
	if (data) {
		while ((*dest = *data++) != '\0') ++dest;
		if (com[-1] == '<') *dest++ = '>';
	}
	*dest++ = '\r';
	*dest++ = '\n';
	result = NATCPwrite(sw->tcpid, buf, dest - buf, -1);
	
	return (result);
}

/* do final callback
 */
static void smtpclose(na_win *w, short code, short err, long size, char *data)
{
	if (sw->state < S_wait) {
		NATCPclose(sw->tcpid);
		FSClose(sw->refnum);
		sw->state = S_wait;
		(*sw->statf)(sw->context, code, err, size, data);
	}
}

/* TCP read/write callback
 */
static void readp(void *wh, na_tcp s, short status, long size, char *data)
{
	na_win *w, **taskh;
	char *dest;
	short major, count, smtpcode;
	
	/* make sure our SMTP task still exists */
	for (taskh = NAtask; taskh && taskh != wh; taskh = (*taskh)->task);
	if (!taskh) return;
	
	/* handle TCP result */
	w = NAlockWindow((na_win **) wh);
	if (status == NATCP_connect) {
		/* deal with new connection */
		sw->state = S_greet;
	} else if (status < 0) {
		/* deal with TCP errors */
		smtpclose(w, NASMTP_tcpfail, status, size, NULL);
		if (status == NATCP_closed) sw->state = S_close;
	} else if (status & NATCP_closing) {
		/* deal with a closed connection */
		if (sw->state < S_wait) {
			smtpclose(w, NASMTP_conclosed, 0, 0, NULL);
		}
	} else if (status & NATCP_data) {
		do {
			/* buffer SMTP line */
			dest = sw->linebuf + sw->count;
			while (size && sw->count < sizeof (sw->linebuf)) {
				--size, ++sw->count;
				if (sw->crfound && *data == '\n') {
					*--dest = '\0';
					--sw->count;
					++data;
					sw->crfound = 0;
					sw->crlf = 1;
					break;
				}
				sw->crfound = (*dest++ = *data++) == '\r';
			}
			if (!sw->crlf) {
				if (sw->count == sizeof (sw->linebuf)) {
					sw->linebuf[sw->count - 1] = '\0';
					smtpclose(w, NASMTP_badprot, 0, 0, sw->linebuf);
				}
				break;
			}
			sw->crlf = 0;
			/* parse SMTP result code */
			dest = sw->linebuf;
			if (sw->count < 3 || !isdigit(dest[0])
				|| !isdigit(dest[1]) || !isdigit(dest[2])) {
				smtpclose(w, NASMTP_badprot, 0, 0, dest);
				break;
			}
			sw->count = 0;
			major = dest[0] - '0';
			smtpcode = major * 100 + (dest[1] - '0') * 10 + (dest[2] - '0');
			/* handle reply continuation */
			if (dest[3] == '-') continue;
			/* handle major errors */
			if (major > 3) {
				if (sw->state != S_rcpt) {
					smtpclose(w, major == 4 ? NASMTP_temperr : NASMTP_permerr,
						smtpcode, sw->state, dest + 3);
					break;
				}
				(*sw->statf)(sw->context, NASMTP_badaddr, smtpcode, 0, sw->linebuf + 3);
			}
			dest = sw->data + sw->headsize;
			/* state changes */
			switch (sw->state) {
				case S_greet:
					if (sw->buf) {
						SMTPsend(w, "HELO ", sw->buf);
						if (sw->buf) DisposPtr(sw->buf);
					}
					sw->state = S_hello;
					break;
				case S_hello:
					SMTPsend(w, "MAIL FROM:<", dest);
					(*sw->statf)(sw->context, NASMTP_progress, 5, 0, 0);
					sw->state = S_mailf;
					break;
				case S_mailf:
				case S_rcpt:
					count = ++sw->rcpt;
					if (count < sw->num + 1) {
						while (count--) {
							while (*dest++);
						}
						SMTPsend(w, "RCPT TO:<", dest);
						(*sw->statf)(sw->context, NASMTP_progress, 5 + 10 * sw->rcpt / sw->num, 0, 0);
					} else {
						sw->state = S_data;
						SMTPsend(w, "DATA", 0);
						(*sw->statf)(sw->context, NASMTP_progress, 20, 0, 0);
					}
					break;
				case S_data:
					if (major != 3) {
						smtpclose(w, NASMTP_badprot, 0, 0, dest);
						break;
					}
					sw->state = S_send;
					if (sw->headsize) {
						sw->buf = NewPtr(sw->bufsize = sw->headsize);
						if (!sw->buf) {
							smtpclose(w, NASMTP_nomem, 0, 0, NULL);
							break;
						}
						memcpy(sw->buf, sw->data, sw->headsize);
					}
				case S_send:
					/* NOTE: this case should never happen */
					break;
				case S_done:
					sw->state = S_quit;
					SMTPsend(w, "QUIT", NULL);
					(*sw->statf)(sw->context, NASMTP_progress, 95, 0, 0);
					break;
				case S_quit:
					smtpclose(w, NASMTP_completed, 0, 0, 0);
					break;
			}
		} while (size);
	}
	NAunlockWindowh((na_win **) wh, w)
}

/* TCP gethost callback
 */
static void hostp(void *wh, na_tcp s, short status, long size, char *data)
{
	na_win *w, **taskh;
	
	/* make sure our task still exists */
	for (taskh = NAtask; taskh && taskh != wh; taskh = (*taskh)->task);
	if (!taskh) return;
	
	/* store host/error */
	w = NAlockWindow((na_win **) wh);
	if (status == NATCP_connect) {
		if (sw->state == S_hello) {
			SMTPsend(w, "HELO ", data);
		} else {
			sw->buf = NewPtr(size + 1);
			if (sw->buf == NULL) {
				smtpclose(w, NASMTP_nomem, 0, 0, NULL);
			} else {
				memcpy(sw->buf, data, size + 1);
			}
		}
	} else {
		smtpclose(w, NASMTP_tcpfail, status, size, NULL);
	}
	NAunlockWindowh((na_win **) wh, w);
}

/* translate CR to CRLF
 */
static void crtocrlf(char *buf, long *size)
{
	long crcount = 0;
	char *src, *dst, *end = buf + *size;
	
	for (src = buf; src < end; ++src) {
		if (src[0] == '\r') ++crcount;
	}
	src = end - 1;
	for (dst = src + crcount; dst > src; *dst-- = *src--) {
		if (*src == '\r') *dst-- = '\n';
	}
	*size += crcount;
}

/* SMTP task
 */
static short taskp(na_win *w)
{
	OSErr oserr;
	
	if (sw->state == S_send || sw->state == S_done) {
		/*XXX: could be generous with NewPtr() failure if a NATCPwritePending() */
		if (!sw->bufsize) {
			if ((sw->buf = NewPtr(8192)) == NULL) {
				smtpclose(w, NASMTP_nomem, 0, 0, NULL);
				return (NA_NOTPROCESSED);
			}
			sw->bufsize = sw->crtrans ? 4096 : 8192;
			oserr = FSRead(sw->refnum, &sw->bufsize, sw->buf);
			if (oserr != noErr && oserr != eofErr) sw->bufsize = 0;
			if (!sw->bufsize) {
				if (oserr == eofErr && sw->state == S_send) {
					memcpy(sw->buf, "\r\n.\r\n", 5);
					sw->bufsize = 5;
					sw->state = S_done;
				} else {
					DisposPtr(sw->buf);
				}
			} else {
				if (sw->crtrans) {
					crtocrlf(sw->buf, &sw->bufsize);
				}
				(*sw->statf)(sw->context, NASMTP_progress, 20
					+ 70 * (sw->fdone += sw->bufsize) / sw->fsize, 0, 0);
			}
		}
		if (sw->bufsize && NATCPwrite(sw->tcpid, sw->buf, sw->bufsize, 1) == NATCP_data) {
			sw->bufsize = 0;
		}
	}
	
	return (sw->state == S_close ? NA_REQCLOSE : NA_NOTPROCESSED);
}

/* SMTP close procedure
 *  IMPORTANT: if the user quits during mail transmission, we want to
 *	warn the user that mail will be lost.
 */
static short closep(na_win *w)
{
	if (sw->state < S_wait) {
		/*XXX: put modal dialog here, allow abort of close */
		if (sw->tcpid >= 0) NATCPclose(sw->tcpid);
		FSClose(sw->refnum);
	}
	
	return (NA_CLOSED);
}
 
/* submit file to SMTP:
 *  creates SMTP task, initializes data, starts TCP connection
 *  copies from & dest addresses, so they don't need to persist
 *  task is not completed until statf() is called
 */
void NASMTPsubmit(na_smtpstat statf, char *server, FSSpec *fspec, Handle headers,
				Handle envelope, short flags, void *context)
{
 	long size;
 	na_win **wh, *w;
 	char *src, *dst;
 	OSErr oserr;

	/* create task */
	size = sizeof (na_smtpwin);
	if (headers) size += GetHandleSize(headers);
	size += GetHandleSize(envelope);
 	wh = NAaddtask(taskp, size);
 	if (!wh) {
 		(*statf)(context, NASMTP_nomem, 0, 0, 0);
 		return;
 	}
 	
 	/* init task */
 	w = NAlockWindow(wh);
 	w->type = NA_SMTPTYPE;
 	w->closep = closep;
 	sw->context = context;
 	sw->statf = statf;
	if (headers && (sw->headsize = GetHandleSize(headers))) {
		memcpy(sw->data, (char *) *headers, sw->headsize);
	}
	size = GetHandleSize(envelope);
	sw->num = -1;
	dst = sw->data + sw->headsize;
	for (src = (char *) *envelope; size; --size) {
		if ((*dst++ = *src++) == '\0') ++sw->num;
	}
	if (flags & NASMTP_crtrans) sw->crtrans = 1;
		
	/* open file */
	if ((oserr = HOpen(fspec->vRefNum, fspec->parID, fspec->name,
		fsRdPerm, &sw->refnum)) != noErr) {
		(*statf)(context, NASMTP_oserr, 0, oserr, 0);
		NAcloseWindow(w, NA_CLOSED);
		return;
	}
	GetEOF(sw->refnum, &sw->fsize);
	
	/* open MacTCP */
 	sw->tcpid = NATCPopen(readp, (void *) wh, server, SMTP_PORT, 0);
 	if (sw->tcpid != -1) NATCPgethost(hostp, (void *) wh);
 	NAunlockWindowh(wh, w);
}
