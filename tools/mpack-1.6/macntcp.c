/* macntcp.c -- macintosh nifty application library async TCP routines
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

#include <string.h>
#include <MacTCPCommonTypes.h>
#include <TCPPB.h>
#include <AddressXlation.h>
#include <GetMyIPAddr.h>
#include "macnapp.h"

#define DEF_STREAM_SIZE 8192
#define MAX_TCPCON  16
#define RDS			8

/* write buffer for TCP writes
 */
typedef struct tcpwb {
	short rused;			/* number of RDS used (-1 == in TCPsend) */
	short wused;			/* amount of small buffer used */
	rdsEntry rds[RDS+1];	/* array of RDS pointers */
	char fflag[RDS+1];		/* free flags for RDS pointers (1 = call DisposPtr) */
	char *buf;				/* write buffer */
} tcpwb;

/* structure describing a TCP connection
 */
typedef struct tcpinfo {
	short state;			/* current state */
	short rclose;			/* remote host wants to close */
	short lclose;			/* local host wants to close */
	int havedata:1;			/* data is available to read */
	int urgent:1;			/* TCP in urgent mode */
	int push:1;				/* next write should be pushed */
	int pushed:1;			/* last write was pushed */
	int reading:1;			/* reading data */
	int server:1;			/* is a server, rather than client */
	int gethost:1;			/* getting hostname -- not a real connection */
	int dnrdone:1;			/* DNR query is done */
	short wbnum;			/* write buffer for next write */
	unsigned short wbsize;	/* size of write buffer */
	unsigned short reason;	/* reason for TCP termination */
	StreamPtr stream;		/* TCP stream */
	ip_port port;			/* TCP port number to connect to */
	void *context;			/* user context */
	na_tcpreadp *callback;	/* callback function */
	rdsEntry rrds[RDS+1];	/* read RDS structure */
	tcpwb wb[2];			/* write buffers */
	struct hostInfo host;	/* hostname & ip_addr of host */
	TCPiopb pb;				/* parameter block for TCP connect/write/close */
	TCPiopb rpb;			/* parameter block for TCP read */
	char buf[1];			/* stream buffer follows */
} tcpinfo;

/* TCP task state information
 */
struct tcpstate {
	na_win win;
	short tcp_driver;
	na_tcpinitp *tcp_initp;
	tcpinfo *tcpbufs[MAX_TCPCON];
	IOParam *open_pb;
	long waiticks, waitstart;
	short waitpercent;
	long streamsize;
	byte TOS, precedence;
	unsigned short wbsize;
	char localhost[256];
} **tcpstate = NULL;
#define tstate ((struct tcpstate *) win)

/* tcp state bitmasks */
#define TCP_PBINUSE   0x04		/* tcp->pb in use */
#define TCP_DNSINUSE  0x08		/* DNS in use */
#define TCP_NOTREADY  0x10      /* not ready for reading */
/* tcp states */
#define TCP_READY     1									/* inactive */
#define TCP_RESOLVE   (TCP_NOTREADY + TCP_DNSINUSE + 0)	/* resolving hostname */
#define TCP_GETHOST   (TCP_NOTREADY + TCP_DNSINUSE + 1)	/* looking up hostname */
#define TCP_WRITING   (TCP_PBINUSE + 0)					/* writing data */
#define TCP_CONNECT   (TCP_NOTREADY + TCP_PBINUSE + 0)	/* waiting for a connection */
#define TCP_CLOSING   (TCP_NOTREADY + TCP_PBINUSE + 1)	/* waiting for TCPclose */
#define TCP_CLOSED    2									/* closed */

/* free write buffer storage
 */
static void freewb(tcpwb *wb)
{
	short i;
	
	for (i = 0; wb->rds[i].length; ++i) {
		if (wb->fflag[i]) {
			DisposPtr(wb->rds[i].ptr);
			wb->fflag[i] = 0;
		}
	}
	wb->rused = 0;
	wb->wused = 0;
}

/* make sure tcp_driver is open
 */
static short tcp_checkdriver()
{
	short msg = NATCP_nodriver;
	struct tcpstate *ts = *tcpstate;
	IOParam *opb = ts->open_pb;
	
	if (!opb) return (1);
	if (opb->ioResult == 1) return (0);
	if (opb->ioResult == noErr && OpenResolver(nil) == noErr) {
		ts->tcp_driver = opb->ioRefNum;
		msg = NATCP_connect;
	}
	DisposPtr((Ptr) opb);
	ts = *tcpstate;
	ts->open_pb = NULL;
	(*ts->tcp_initp)(msg);
	
	return (1);
}

/* wait for MacTCP to finish whatever it's doing, with user cancel
 */
static short tcp_wait(struct tcpstate *ts, tcpinfo *tcp)
{
	KeyMap mapkeys;
#define keys ((unsigned char *)mapkeys)
	short percent;

	while (!tcp_checkdriver()
		|| (tcp && (tcp->state & TCP_DNSINUSE) && ! (volatile) tcp->dnrdone)
		|| (tcp && (tcp->state & TCP_PBINUSE) && (volatile short) tcp->pb.ioResult == 1)) {
		if (ts) {
			if (!ts->waiticks) return (0);
			percent = ((TickCount() - ts->waitstart) * 100) / ts->waiticks;
			if (percent > 100) percent = 100;
			if (percent != ts->waitpercent) {
				(*ts->tcp_initp)(ts->waitpercent = percent);
			}
			if (percent == 100) return (0);
		}
		SystemTask();
		GetKeys(mapkeys);
		if ((keys[0x37 >> 3] >> (0x37 & 7))
			& (keys[0x2f >> 3] >> (0x2f & 7)) & 1) {
			return (0);
		}
	}
	
	return (1);
}

/* clean up the TCP task
 */
static short tcp_closep(na_win *win)
{
	short i, j;
	tcpinfo *tcp;
	tcpwb *wb;
	TCPiopb pb;
	
	tstate->waitstart = TickCount();
	tstate->waitpercent = 0;
	tcp_wait(tstate, NULL);
	memset((void *) &pb, 0, sizeof (pb));
	for (i = 0; i < MAX_TCPCON; ++i) {
		if ((tcp = tstate->tcpbufs[i]) != NULL) {
			/* wait for MacTCP to finish what it's doing, but permit user cancel */
			if (!tcp->server || tcp->state != TCP_CONNECT) tcp_wait(tstate, tcp);
			if (!tcp->gethost) {
				pb.ioCRefNum = tstate->tcp_driver;
				pb.tcpStream = tcp->stream;
				pb.csCode = TCPRelease;
				PBControl((ParmBlkPtr) &pb, false);
			}
			freewb(tcp->wb);
			freewb(tcp->wb + 1);
			DisposPtr((Ptr) tcp);
			tstate->tcpbufs[i] = NULL;
		}
	}
	tcpstate = NULL;
	if (tstate->tcp_driver) CloseResolver();
	
	return (NA_CLOSED);
}

/* begin writing data
 */
static short beginwrite(tcpinfo *tcp)
{
	tcpwb *wb = tcp->wb + tcp->wbnum;
	
	/* if connection terminated, or we've sent a TCPclose, we can't write */
	if (tcp->rclose == 3 || tcp->lclose > 1) return (0);
	memset((void *) &tcp->pb.csParam, 0, sizeof (tcp->pb.csParam));
	tcp->pb.csCode = TCPSend;
	tcp->pb.csParam.send.ulpTimeoutValue = 60; /* 1 minute to send data */
	tcp->pb.csParam.send.ulpTimeoutAction = 0;
	tcp->pb.csParam.send.validityFlags = 0xC0;
	tcp->pb.csParam.send.wdsPtr = (Ptr) wb->rds;
	tcp->pb.csParam.send.pushFlag = tcp->pushed = tcp->push;
	PBControl((ParmBlkPtr) &tcp->pb, true);
	tcp->push = 0;
	tcp->wbnum = 1 - tcp->wbnum;
	wb->rused = -1;
	
	return (tcp->state = TCP_WRITING);
}

/* do I/O polling
 */
short NATCPtask(na_win *win)
{
	tcpinfo *tcp;
	rdsEntry *rds, *trds;
	short result, newstate;
	short processed = NA_PROCESSED;
	na_tcp i;
	tcpwb *wb;
	int j;
	
	/* finish off driver initialization: */
	if (!tstate->tcp_driver) {
		if (!tcp_checkdriver()) return (NA_NOTPROCESSED);
		if (!tstate->tcp_driver) return (NA_REQCLOSE);
	}
	/* loop through connections */
	for (i = 0; i < MAX_TCPCON; ++i) {
		if ((tcp = tstate->tcpbufs[i]) != NULL) do {
			/* read data if we have it */
			if (!tcp->reading && !tcp->rclose && tcp->havedata && tcp->state != TCP_CONNECT) {
				tcp->rpb.ioCRefNum = tstate->tcp_driver;
				tcp->rpb.tcpStream = tcp->stream;
				tcp->rpb.csCode = TCPNoCopyRcv;
				tcp->rpb.csParam.receive.rdsPtr = (Ptr) tcp->rrds;
				tcp->rpb.csParam.receive.commandTimeoutValue = 5;
				tcp->rpb.csParam.receive.rdsLength = RDS;
				if (tcp->pushed) {
					tcp->rpb.csParam.receive.commandTimeoutValue = 1;
					tcp->rpb.csParam.receive.rdsLength = 1;
				}
				tcp->havedata = 0;
				PBControl((ParmBlkPtr) &tcp->rpb, tcp->pushed ? false : true);
				tcp->reading = 1;
				tcp->pushed = 0;
			}
			if (tcp->reading) {
				if ((result = tcp->rpb.ioResult) == 1) {
					processed = NA_NOTPROCESSED;
				} else {
					tcp->reading = 0;
					if (result != noErr) {
						if (result != commandTimeout) {
							(*tcp->callback)(tcp->context, i, NATCP_noread, result, NULL);
						}
					} else {
						result = NATCP_data | NATCP_more;
						if (tcp->rpb.csParam.receive.urgentFlag) tcp->urgent = 1;
						if (tcp->urgent) result |= NATCP_urgent;
						if (tcp->rpb.csParam.receive.markFlag) tcp->urgent = 0;
						for (rds = tcp->rrds; rds->length; ++rds) {
							if (!rds[1].length) result &= ~NATCP_more;
							(*tcp->callback)(tcp->context, i, result,
								rds->length, rds->ptr);
						}
						tcp->rpb.csCode = TCPRcvBfrReturn;
						PBControl((ParmBlkPtr) &tcp->rpb, false);
					}
				}
			}
			result = tcp->pb.ioResult;
			newstate = 0;
			switch (tcp->state) {
				case TCP_GETHOST:
					if (tcp->dnrdone) {
						tcp->rclose = 3;
						newstate = TCP_CLOSED;
						if (tcp->host.rtnCode != noErr) {
							(*tcp->callback)(tcp->context, i, NATCP_nohost,
								tcp->host.rtnCode, NULL);
						} else {
							(*tcp->callback)(tcp->context, i, NATCP_connect,
								strlen(tcp->host.cname), tcp->host.cname);
							strcpy(tstate->localhost, tcp->host.cname);
						}
					}
					break;
				case TCP_RESOLVE:
					if (tcp->dnrdone) {
						if (tcp->host.rtnCode != noErr) {
							tcp->rclose = 3;
							newstate = TCP_CLOSED;
							(*tcp->callback)(tcp->context, i, NATCP_nohost,
								tcp->host.rtnCode, NULL);
						} else if (!tcp->lclose) {
							memset((void *) &tcp->pb, 0, sizeof (tcp->pb));
							tcp->pb.ioCRefNum = tstate->tcp_driver;
							tcp->pb.tcpStream = tcp->stream;
							tcp->pb.csParam.open.ulpTimeoutValue = 30;
							tcp->pb.csParam.open.ulpTimeoutAction = 1; /* Abort on timeout */
							tcp->pb.csParam.open.tosFlags = tstate->TOS;
							tcp->pb.csParam.open.precedence = tstate->precedence;
							tcp->pb.csParam.open.validityFlags = 
								timeoutValue|timeoutAction|typeOfService|precedence;
							tcp->pb.csParam.open.remoteHost = tcp->host.addr[0];
							if (tcp->server) {
								tcp->pb.csCode = TCPPassiveOpen;
								tcp->pb.csParam.open.commandTimeoutValue = 0;
								tcp->pb.csParam.open.remotePort = 0;
								tcp->pb.csParam.open.localPort = tcp->port;
							} else {
								tcp->pb.csCode = TCPActiveOpen;
								tcp->pb.csParam.open.remotePort = tcp->port;
								tcp->pb.csParam.open.localPort = 0;
							}
							PBControl((ParmBlkPtr) &tcp->pb, true);
							newstate = TCP_CONNECT;
						}
					}
					break;
				case TCP_CONNECT:
					if (result == 1) {
						processed = NA_NOTPROCESSED;
						break;
					}
					if (result != noErr) {
						tcp->rclose = 3;
						newstate = TCP_CLOSED;
						(*tcp->callback)(tcp->context, i, NATCP_nocon, result, NULL);
					} else {
						newstate = TCP_READY;
						if (tcp->server) {
							tcp->port = tcp->pb.csParam.open.remotePort;
							if (!*tcp->host.cname) {
								AddrToStr(tcp->pb.csParam.open.remoteHost, tcp->host.cname);
							}
						}
						(*tcp->callback)(tcp->context, i, NATCP_connect,
							tcp->port, tcp->host.cname);
					}
					break;
				case TCP_READY:
					/* Write data if we have it */
					wb = tcp->wb + tcp->wbnum;
					if (wb->rused && (newstate = beginwrite(tcp))) {
						break;
					}
					/* check if other side wants to close */
					if (tcp->rclose == 1) {
						tcp->rclose = 2;
						(*tcp->callback)(tcp->context, i, NATCP_closing, 0, NULL);
					}
					/* check if connection needs closing at this end */
	 				if (tcp->lclose == 1) {
						tcp->lclose = 2;
						tcp->pb.csCode = TCPClose;
						tcp->pb.csParam.close.validityFlags = 0xC0;
						tcp->pb.csParam.close.ulpTimeoutValue = 30; /* give 30 secs to close */
						tcp->pb.csParam.close.ulpTimeoutAction = 0;
						PBControl((ParmBlkPtr) &tcp->pb, true);
						newstate = TCP_CLOSING;
						break;
					}
					/* check if connection closed at both ends */
					if (tcp->rclose == 3) {
						(*tcp->callback)(tcp->context, i, NATCP_closed, tcp->reason, NULL);
						newstate = TCP_CLOSED;
					}
					break;
				case TCP_WRITING:
					if (result == 1) {
						processed = NA_NOTPROCESSED;
						break;
					}
					wb = tcp->wb;
					if (wb->rused != -1) ++wb;
					freewb(wb);
					if (result != noErr) {
						tcp->pushed = 0;
						(*tcp->callback)(tcp->context, i, NATCP_nowrite, result, NULL);
					}
					newstate = TCP_READY;
					break;
				case TCP_CLOSING:
					if (result == 1) {
						processed = NA_NOTPROCESSED;
						break;
					}
					newstate = TCP_READY;
					break;
				case TCP_CLOSED:
					if (!tcp->rclose) break;
					if (!tcp->gethost) {
						tcp->pb.csCode = TCPRelease;
						PBControl((ParmBlkPtr)&tcp->pb, false);
					}
					freewb(tcp->wb);
					freewb(tcp->wb + 1);
					DisposPtr((Ptr) tcp);
					tstate->tcpbufs[i] = NULL;
					break;
			}
			if (newstate) tcp->state = newstate;
		} while (newstate);
	}
	
	return (processed);
}

/* Async notification routine
 */
static pascal void myTCPNotifyProc(StreamPtr stream, unsigned short eventCode,
	Ptr userDataPtr, unsigned short terminReason, struct ICMPReport *icmpMsg)
{
	tcpinfo *tcp = (tcpinfo *) userDataPtr;
	
	switch (eventCode) {
		case TCPTerminate:
			tcp->rclose = 3;
			tcp->reason = terminReason;
			break;
		case TCPClosing:
			tcp->rclose = 1;
			break;
		case TCPDataArrival:
			tcp->havedata = 1;
			break;
		case TCPUrgent:
			tcp->urgent = 1;
			break;
	}
}

/* DNR resultproc */
static pascal void addrproc(struct hostInfo *hinfop, char *udata)
{
	((tcpinfo *) udata)->dnrdone = 1;
}

/* callback to pass TCP info to window
 */
static void winreadp(void *context, na_tcp i, short status, long len, char *buf)
{
	natcp_win *w;
	
	w = (natcp_win *) NAlockWindow((na_win **) context);
	w->s = i;
	(*w->readp)(&w->winp, status, len, buf);
	NAunlockWindowh((na_win **) context, &w->winp);
}

/* adjust TCP settings
 */
void NATCPsettings(long streamsize, short type_of_service, short precedence, unsigned short wbsize)
{
	if (!streamsize) streamsize = DEF_STREAM_SIZE;
	(*tcpstate)->streamsize = streamsize ? streamsize : DEF_STREAM_SIZE;
	(*tcpstate)->TOS = type_of_service;
	(*tcpstate)->precedence = precedence;
	if (!wbsize) wbsize = 1024;
	(*tcpstate)->wbsize = wbsize;
}

/* initialize TCP system
 */
void NATCPinit(na_tcpinitp *initp)
{
	IOParam *pb;
	int i;
	struct tcpstate *ts;

	pb = (IOParam *) NewPtrClear(sizeof (IOParam));
	tcpstate = (struct tcpstate **) NAaddtask(NATCPtask, sizeof (struct tcpstate));
	if (!tcpstate || !pb) {
		(*initp)(NATCP_nomem);
	} else {
		pb->ioNamePtr = "\p.IPP";
		PBOpen((ParmBlkPtr) pb, true);
		ts = *tcpstate;
		for (i = 0; i < MAX_TCPCON; ++i) ts->tcpbufs[i] = NULL;
		ts->waiticks = 60; /* wait 1 sec for TCP close by default */
		ts->win.type = NA_TCPTYPE;
		ts->win.closep = tcp_closep;
		ts->win.priority = -1;
		ts->tcp_initp = initp;
		ts->open_pb = pb;
		NATCPsettings(0, 0, 0, 0);
	}
}

/* get a TCP buffer block
 */
static tcpinfo *getTCPbuf(na_tcpreadp *callback, void *context, int *id)
{
	int i;
	tcpinfo *tcp;
	
	/* make sure driver is open */
	if (!(*tcpstate)->tcp_driver
		&& (!tcp_wait(NULL, NULL) || !(*tcpstate)->tcp_driver)) {
		(*callback)(context, -1, NATCP_nodriver, 0, NULL);
		return (NULL);
	}
	
	/* find pointer slot and create buffer */
	for (i = 0; i < MAX_TCPCON && (*tcpstate)->tcpbufs[i]; ++i);
	if (i == MAX_TCPCON) {
		(*callback)(context, -1, NATCP_notcpbuf, 0, NULL);
		return (NULL);
	}
	tcp = (tcpinfo *) NewPtr(sizeof (tcpinfo) - 1
		+ (*tcpstate)->streamsize + (unsigned long) (*tcpstate)->wbsize * 2);
	if (!tcp) {
		(*callback)(context, -1, NATCP_nomem, 0, NULL);
		return (NULL);
	};
	*id = i;
	(*tcpstate)->tcpbufs[i] = tcp;
	memset((char *) tcp, 0, sizeof (tcpinfo));
	
	/* initialize fields from global state */
	tcp->wbsize = (*tcpstate)->wbsize;
	tcp->wb[0].buf = tcp->buf + (*tcpstate)->streamsize;
	tcp->wb[1].buf = tcp->wb[0].buf + tcp->wbsize;
	tcp->pb.ioCRefNum = (*tcpstate)->tcp_driver;
	tcp->context = context;
	tcp->callback = callback;
	
	return (tcp);
}

/* get host name
 */
void NATCPgethost(na_tcpreadp *callback, void *context)
{
	int id;
	tcpinfo *tcp;
	struct IPParamBlock *ippb;
	na_win *win;
	
	if ((*tcpstate)->localhost[0]) {
		win = NAlockWindow((na_win **) tcpstate);
		(*callback)(context, -1, NATCP_connect, strlen(tstate->localhost),
			tstate->localhost);
		NAunlockWindowh((na_win **) tcpstate, win);
	} else if ((tcp = getTCPbuf(callback, context, &id)) != NULL) {
		/* here we make the assumption that an IP param block is smaller than
		 * a TCP param block.  This seems like a safe assumption to me.
		 */
		ippb = (struct IPParamBlock *) &tcp->pb;
		/* get IP address */
		ippb->ioCRefNum = (*tcpstate)->tcp_driver;
		ippb->csCode = ipctlGetAddr;
		PBControl((ParmBlkPtr)ippb, false);
		if (ippb->ioResult != 0) {
			(*callback)(context, -1, NATCP_notcpbuf, ippb->ioResult, NULL);
			DisposPtr((Ptr) tcp);
			(*tcpstate)->tcpbufs[id] = NULL;
		} else {
			/* begin IP address lookup */
			tcp->dnrdone = 0;
			AddrToName(ippb->ourAddress, &tcp->host, addrproc, (char *) tcp);
			tcp->state = TCP_GETHOST;
			tcp->gethost = 1;
		}
	}
}

/* open a TCP connection
 */
na_tcp NATCPopen(na_tcpreadp *callback, void *context, char *host, long port, short flags)
{
	int i, err = NATCP_notcpbuf;
	OSErr resolve = noErr;
	tcpinfo *tcp;
	
	if ((tcp = getTCPbuf(callback, context, &i)) == NULL) return (-1);
	if (flags & NATCP_server) tcp->server = 1;
	tcp->port = port;
	tcp->pb.csCode = TCPCreate;
	tcp->pb.csParam.create.rcvBuff = (Ptr) tcp->buf;
	tcp->pb.csParam.create.rcvBuffLen = (*tcpstate)->streamsize;
	tcp->pb.csParam.create.notifyProc = myTCPNotifyProc;
	tcp->pb.csParam.create.userDataPtr = (Ptr) tcp;
	PBControl((ParmBlkPtr)&tcp->pb, false);
	if (tcp->pb.ioResult == 0) {
		tcp->state = TCP_RESOLVE;
		tcp->stream = tcp->pb.tcpStream;
		/* a server isn't required to have a hostname */
		if (!host && tcp->server) return (i);
		tcp->dnrdone = 0;
		resolve = StrToAddr(host, &tcp->host, addrproc, (char *) tcp);
		if (resolve == noErr) tcp->dnrdone = 1;
		if (resolve == cacheFault || resolve == noErr) {
			return (i);
		}
		err = NATCP_nohost;
	}
	DisposPtr((Ptr) tcp);
	(*tcpstate)->tcpbufs[i] = NULL;
	(*callback)(context, -1, err, resolve, NULL);
	
	return (-1);
}

/* open a connection to a tcp window
 */
na_tcp NATCPwinopen(natcp_win *w, char *host, long port, short flags)
{
	return (NATCPopen(winreadp, (void *) RecoverHandle((Ptr) w), host, port, flags));
}

/* pass a buffer to tcp connection for writing
 *  dispose of -1 = copy data
 */
short NATCPwrite(na_tcp i, Ptr data, long len, short dispose)
{
	tcpinfo *tcp = (*tcpstate)->tcpbufs[i];
	rdsEntry *rds = NULL;
	tcpwb *wb;
	long totallen = 0;
	int j;
	
	if (tcp == NULL || tcp->lclose > 0 || tcp->rclose == 3) {
		return (NATCP_nocon);
	}
	wb = tcp->wb + tcp->wbnum;
	if (wb->rused == RDS) wb = tcp->wb + (1 - tcp->wbnum);
	if (wb->rused == -1 || wb->rused == RDS) return (NATCP_notcpbuf);
	for (j = 0; j < wb->rused; ++j) {
		totallen += wb->rds[j].length;
	}
	if (totallen + len >= 65535) return (NATCP_notcpbuf);
	rds = wb->rds + wb->rused;
	rds->length = len;
	rds->ptr = data;
	rds[1].length = 0;
	if (dispose < 0) {
		if (len < tcp->wbsize - wb->wused) {
			/* if data short enough, use small internal buffer */
			rds->ptr = wb->buf + wb->wused;
			wb->wused += len;
			dispose = 0;
			/* If adjacent to last rds, attach to it */
			if (wb->rused && rds[-1].ptr + rds[-1].length == rds->ptr) {
				--wb->rused;
				rds[-1].length += len;
				rds->length = 0;
			}
		} else {
			rds->ptr = NewPtr(len);
			if (!rds->ptr) return (NATCP_nomem);
			dispose = 1;
		}
		memcpy(rds->ptr, data, len);
	}
	wb->fflag[wb->rused++] = dispose;
	if (tcp->push && tcp->state == TCP_READY) {
		(void) beginwrite(tcp);
	}
	
	return (NATCP_data);
}

/* put a character on the TCP connection -- optimized for fast turnaround
 */
short NATCPputchar(na_tcp i, char c)
{
	(*tcpstate)->tcpbufs[i]->push = 1;
	
	return (NATCPwrite(i, (Ptr) &c, 1, -1));
}

/* close a TCP connection
 */
void NATCPclose(na_tcp i)
{
	tcpinfo *tcp = (*tcpstate)->tcpbufs[i];
	
	if (tcp && tcp->lclose < 1) tcp->lclose = 1;
}

/* dispose of all TCP system resources
 */
void NATCPdone(long waiticks)
{
	struct tcpstate *ts;
	
	if (tcpstate) {
		ts = (struct tcpstate *) NAlockWindow((na_win **) tcpstate);
		ts->waiticks = waiticks;
		NAcloseWindow((na_win *) ts, NA_REQCLOSE);
	}
}
