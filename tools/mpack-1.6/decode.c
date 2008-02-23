/*
 * Decode MIME parts.
 */
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
 * SOFTWARE.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include "xmalloc.h"
#include "common.h"
#include "part.h"
#include "md5.h"

extern char *os_idtodir(char *id);
extern FILE *os_newtypedfile(char *fname, char *contentType, int flags, params contentParams);
extern FILE *os_createnewfile(char *fname);
extern char *md5contextTo64(MD5_CTX *context);
extern void warn(char *s);
extern void os_perror(char *str);
extern void chat(char *s);
extern void os_donewithdir(char *dir);
extern void os_warnMD5mismatch(void);
extern void os_closetypedfile(FILE *outfile);

extern int part_depth(struct part *part);
extern void part_ungets(char *s, struct part *part);
extern void part_close(struct part *part);
extern int part_fill(struct part *part);
extern void part_addboundary(struct part *part, char *boundary);
extern int part_readboundary(struct part *part);

/* The possible content transfer encodings */
enum encoding { enc_none, enc_qp, enc_base64 };

char *ParseHeaders(struct part *inpart, char **subjectp, char **contentTypep, enum encoding *contentEncodingp, char **contentDispositionp, char **contentMD5p);
enum encoding parseEncoding(char *s);
params ParseContent(char **headerp);
char *getParam(params cParams, char *key);
char *getDispositionFilename(char *disposition);
void from64(struct part *inpart, FILE *outfile, char **digestp, int suppressCR);
void fromqp(struct part *inpart, FILE *outfile, char **digestp);
void fromnone(struct part *inpart, FILE *outfile, char **digestp);
int handlePartial(struct part *inpart, char *headers, params contentParams,
		  int extractText);
int ignoreMessage(struct part *inpart);
int handleMultipart(struct part *inpart, char *contentType,
		    params contentParams, int extractText);
int handleUuencode(struct part *inpart, char *subject, int extractText);
int handleText(struct part *inpart, enum encoding contentEncoding);
int saveToFile(struct part *inpart, int inAppleDouble, char *contentType,
	       params contentParams, enum encoding contentEncoding,
	       char *contentDisposition, char *contentMD5);

/*
 * Read and handle an RFC 822 message from the body-part 'inpart'.
 */
int handleMessage(struct part *inpart, char *defaultContentType, int inAppleDouble, int extractText)
{
    char *headers, *subject, *contentType, *contentDisposition, *contentMD5;
    enum encoding contentEncoding;
    params contentParams;

    /* Parse the headers, getting the ones we're interested in */
    headers = ParseHeaders(inpart, &subject, &contentType, &contentEncoding,
			   &contentDisposition, &contentMD5);
    if (!headers) return 1;

    /* If no content type, or a non-MIME content type, use the default */
    if (!contentType || !strchr(contentType, '/')) {
	contentType = defaultContentType;
    }
    contentParams = ParseContent(&contentType);

    if (!strcasecmp(contentType, "message/rfc822")) {
	if (contentEncoding != enc_none) {
	    warn("ignoring invalid content encoding on message/rfc822");
	}

	/* Simple recursion */
	return handleMessage(inpart, "text/plain", 0, extractText);
    }
    else if (!strcasecmp(contentType, "message/partial")) {
	if (contentEncoding != enc_none) {
	    warn("ignoring invalid content encoding on message/partial");
	}
	return handlePartial(inpart, headers, contentParams, extractText);
    }
    else if (!strncasecmp(contentType, "message/", 8)) {
	/* Probably message/external.  We don't care--toss it */
	return ignoreMessage(inpart);
    }
    else if (!strncasecmp(contentType, "multipart/", 10)) {
	if (contentEncoding != enc_none) {
	    warn("ignoring invalid content encoding on multipart");
	}
	return handleMultipart(inpart, contentType, contentParams,
			       extractText);
    }
    else if (part_depth(inpart) == 0 &&
	     !strncasecmp(contentType, "text/", 5) &&
	     contentEncoding == enc_none &&
	     !getDispositionFilename(contentDisposition) &&
	     !getParam(contentParams, "name")) {
	/* top-level text message, handle as possible uuencoded file */
	return handleUuencode(inpart, subject, extractText);
    }
    else if (!extractText && !inAppleDouble &&
	     !strncasecmp(contentType, "text/", 5) &&
	     !getDispositionFilename(contentDisposition) &&
	     !getParam(contentParams, "name")) {
	return handleText(inpart, contentEncoding);
    }
    else {
	/* Some sort of attachment, extract it */
	return saveToFile(inpart, inAppleDouble, contentType, contentParams,
			  contentEncoding, contentDisposition, contentMD5);
    }
}

/*
 * Skip whitespace and RFC-822 comments.
 */
void SkipWhitespace(char **s)
{
    char *p = *s;
    int commentlevel = 0;

    while (*p && (isspace(*p) || *p == '(')) {
	if (*p == '\n') {
	    p++;
	    if (*p != ' ' && *p != '\t') {
		*s = 0;
		return;
	    }
	}
	else if (*p == '(') {
	    p++;
	    commentlevel++;
	    while (commentlevel) {
		switch (*p) {
		case '\n':
		    p++;
		    if (*p == ' ' || *p == '\t') break;
		    /* FALL THROUGH */
		case '\0':
		    *s = 0;
		    return;
		    
		case '\\':
		    p++;
		    break;

		case '(':
		    commentlevel++;
		    break;

		case ')':
		    commentlevel--;
		    break;
		}
		p++;
	    }
	}
	else p++;
    }
    if (*p == 0) {
	*s = 0;
    }
    else {
	*s = p;
    }
}

/*
 * Read and parse the headers of an RFC 822 message, returning them in
 * a pointer to a static buffer.  The headers are read from 'inpart'.
 * A pointer to the value of any Subject:, Content-Type:,
 * Content-Disposition:, or Content-MD5: header is stored in the space
 * pointed to by 'subjectp', 'contentTypep', contentDispositionp, and
 * contentMD5p, respectively.  The Content-Transfer-Encoding is stored
 * in the enum pointed to by 'contentEncodingp'.
 */
#define HEADGROWSIZE 1000
char *ParseHeaders(struct part *inpart, char **subjectp, char **contentTypep, enum encoding *contentEncodingp, char **contentDispositionp, char **contentMD5p)
{
    static int alloced = 0;
    static char *headers;
    int left, len, i;
    char *next, *val;

    /* Read headers into buffer pointed to by "headers" */
    if (!alloced) {
	headers = xmalloc(alloced = HEADGROWSIZE);
    }
    next = headers;
    *next++ = '\n';		/* Leading newline to make matching header names easier */
    left = alloced - 2;		/* Allow room for terminating null */

    while (part_gets(next, left, inpart) && (*next != '\n' || next[-1] != '\n')) {
	len = strlen(next);

	if (next[-1] == '\n') {
	    /* Check for valid header-ness of "next" */
	    for (i = 0; i < len; i++) {
		if (next[i] == ':' ||
		    next[i] <= ' ' || next[i] >= '\177') break;
	    }
	    if (i == 0 || next[i] != ':') {
		/* Check for header continuation line */
		if (next == headers+1 || (next[0] != ' ' && next[0] != '\t')) {
		    /*
		     * Not a valid header, push back on input stream
		     * and stop reading input.
		     */
		    part_ungets(next, inpart);
		    break;
		}
	    }
	}

	left -= len;
	next += len;

	if (left < 100) {
	    len = next - headers;
	    alloced += HEADGROWSIZE;
	    left += HEADGROWSIZE;
	    headers = xrealloc(headers, alloced);
	    next = headers + len;
	}
    }

    *next = '\0';

    /* Look for the headers we find particularly interesting */
    *subjectp = *contentTypep = *contentDispositionp = *contentMD5p = 0;
    *contentEncodingp = enc_none;
    for (next = headers; *next; next++) {
	if (*next == '\n') {
	    switch(next[1]) {
	    case 's':
	    case 'S':
		if (!strncasecmp(next+2, "ubject:", 7)) {
		    val = next+9;
		    SkipWhitespace(&val);
		    if (val) *subjectp = val;
		}
		break;

	    case 'c':
	    case 'C':
		if (!strncasecmp(next+2, "ontent-type:", 12)) {
		    val = next+14;
		    SkipWhitespace(&val);
		    if (val) *contentTypep = val;
		}
		else if (!strncasecmp(next+2, "ontent-transfer-encoding:", 25)) {
		    *contentEncodingp = parseEncoding(next+27);
		}
		else if (!strncasecmp(next+2, "ontent-disposition:", 19)) {
		    val = next+21;
		    SkipWhitespace(&val);
		    if (val) *contentDispositionp = val;
		}
		else if (!strncasecmp(next+2, "ontent-md5:", 11)) {
		    val = next+13;
		    SkipWhitespace(&val);
		    if (val) *contentMD5p = val;
		}
	    }
	}
    }
    return headers;
}

/*
 * Parse the Content-Transfer-Encoding: value pointed to by 's'.
 * Returns the appropriate encoding enum.
 */
enum encoding parseEncoding(char *s)
{
    SkipWhitespace(&s);
    if (s) {
	switch (*s) {
	case 'q':
	case 'Q':
	    if (!strncasecmp(s+1, "uoted-printable", 15) &&
		(isspace(s[16]) || s[16] == '(')) {
		return enc_qp;
	    }
	    break;

	case '7':
	case '8':
	    if (!strncasecmp(s+1, "bit", 3) &&
		(isspace(s[4]) || s[4] == '(')) {
		return enc_none;
	    }
	    break;

	case 'b':
	case 'B':
	    if (!strncasecmp(s+1, "ase64", 5) &&
		(isspace(s[6]) || s[6] == '(')) {
		return enc_base64;
	    }
	    if (!strncasecmp(s+1, "inary", 5) &&
		(isspace(s[6]) || s[6] == '(')) {
		return enc_none;
	    }
	}
	warn("ignoring unknown content transfer encoding\n");	
    }
    return enc_none;
}

/*
 * Parse the value of a Content-Type: header.
 * 'headerp' points to a pointer to the input string.
 * The pointer pointed to by 'headerp' is changed to point to
 * a static buffer containing the content type stripped of whitespace
 * and parameters.  The parameters are converted to a type suitable for
 * getParm() and returned.
 */
#define PARAMGROWSIZE 10
params ParseContent(char **headerp)
{
    char *header;
    static int palloced = 0;
    static char **param;
    static int calloced = 0;
    static char *cbuf;
    char *p;
    int nparam;

    p = header = *headerp;

    /* Find end of header, including continuation lines */
    do {
	p = strchr(p+1, '\n');
    } while (p && isspace(p[1]));
    if (!p) {
	p = header + strlen(header);
    }

    /* If necessary, allocate/grow cbuf to hold header. */
    if (p - header >= calloced) {
	calloced = p - header + 1;
	if (calloced < 200) calloced = 200;
	cbuf = xrealloc(cbuf, calloced);
    }

    /* Copy header to cbuf */
    strncpy(cbuf, header, p - header);
    cbuf[p - header] = 0;
    header = *headerp = cbuf;
    
    nparam = 0;

    /* Strip whitespace from content type */
    /* ParseHeaders() stripped leading whitespace */
    p = header;
    while (header && *header && *header != ';') {
	while (*header && !isspace(*header) && *header != '(' &&
	       *header != ';') {
	    *p++ = *header++;
	}
	SkipWhitespace(&header);
    }
    if (!header || !*header) return 0;
    header++;
    *p = '\0';
    
    /* Parse the parameters */
    while (*header) {
	SkipWhitespace(&header);
	if (!header) break;

	if (nparam+1 >= palloced) {
	    palloced += PARAMGROWSIZE;
	    param = (char **) xrealloc((char *)param, palloced * sizeof(char *));
	}
	param[nparam++] = header;

	/* Find any separating semicolon.  Pay attention to quoted-strings */
	while (*header && *header != ';') {
	    if (*header == '\"') {
		++header;
		while (*header && *header != '\"') {
		    if (*header == '\\') {
			++header;
			if (!*header) break;
		    }
		    ++header;
		}
		if (!*header) break;
	    }
	    else if (*header == '(') {
		/* Convert comments to spaces */
		p = header;
		SkipWhitespace(&p);
		if (!p) {
		    break;
		}
		while (header < p) *header++ = ' ';
		header--;
	    }
	    header++;
	}
	if (*header) *header++ = '\0';
    }
    
    if (nparam == 0)
       return 0;

    param[nparam] = 0;
    return param;
}

/*
 * Get the value of the parameter named 'key' from the content-type
 * parameters 'cParams'.  Returns a pointer to a static bufer which
 * contains the value, or null if no such parameter was found.
 */
#define VALUEGROWSIZE 100
char *getParam(params cParams, char *key)
{
    static char *value;
    static int alloced = 0;
    int left;
    int keylen = strlen(key);
    char *from, *to;

    if (!cParams) return 0;

    if (!alloced) {
	value = xmalloc(alloced = VALUEGROWSIZE);
    }

    /* Find the named parameter */
    while (*cParams) {
	if (!strncasecmp(key, *cParams, keylen) &&
	    ((*cParams)[keylen] == '=' || isspace((*cParams)[keylen]))) break;
	cParams++;
    }
    if (!*cParams) return 0;

    /* Skip over the "=" and any surrounding whitespace */
    from = *cParams + keylen;
    while (*from && isspace(*from)) from++;
    if (*from++ != '=') return 0;
    while (*from && isspace(*from)) from++;
    if (!*from) return 0;

    /* Copy value into buffer */
    to = value;
    left = alloced - 1;
    if (*from == '\"') {
	/* Quoted-string */
	from++;
	while (*from && *from != '\"') {
	    if (!--left) {
		alloced += VALUEGROWSIZE;
		left += VALUEGROWSIZE;
		value = xrealloc(value, alloced);
		to = value + alloced - left - 2;
	    }
	    if (*from == '\\') {
		from++;
		if (!*from) return 0;
	    }
	    *to++ = *from++;
	}
	if (!*from) return 0;
    }
    else {
	/* Just a token */
	while (*from && !isspace(*from)) {
	    if (!--left) {
		alloced += VALUEGROWSIZE;
		left += VALUEGROWSIZE;
		value = xrealloc(value, alloced);
		to = value + alloced - left - 2;
	    }
	    *to++ = *from++;
	}
    }
    *to = '\0';
    return value;
}

/*
 * Get the value of the "filename" parameter in a Content-Disposition:
 * header.  Returns a pointer to a static buffer containing the value, or
 * a null pointer if there was no such parameter.
 */
char *
getDispositionFilename(char *disposition)
{
    static char *value;
    static int alloced = 0;
    int left;
    char *to;

    if (!disposition) return 0;

    /* Skip until we find ";" "filename" "=" tokens. */
    for (;;) {
	/* Skip until we find ";" */
	while (*disposition != ';') {
	    if (!*disposition) return 0;
	    else if (*disposition == '\"') {
		++disposition;
		while (*disposition && *disposition != '\"') {
		    if (*disposition == '\\') {
			++disposition;
			if (!*disposition) return 0;
		    }
		    ++disposition;
		}
		if (!*disposition) return 0;
	    }
	    else if (*disposition == '(') {
		SkipWhitespace(&disposition);
		if (!disposition) return 0;
		disposition--;
	    }
	    disposition++;
	}

	/* Skip over ";" and trailing whitespace */
	disposition++;
	SkipWhitespace(&disposition);
	if (!disposition) return 0;

	/*
	 * If we're not looking at a "filename" token, go back
	 * and look for another ";".  Otherwise skip it and
	 * trailing whitespace.
	 */
	if (strncasecmp(disposition, "filename", 8) != 0) continue;
	disposition += 8;
	if (!isspace(*disposition) && *disposition != '=' &&
	    *disposition != '(') {
	    continue;
	}
	SkipWhitespace(&disposition);
	if (!disposition) return 0;

	/* If we're looking at a "=", we found what we're looking for */
	if (*disposition++ == '=') break;
    }

    SkipWhitespace(&disposition);
    if (!disposition) return 0;
      
    if (!alloced) {
	value = xmalloc(alloced = VALUEGROWSIZE);
    }

    /* Copy value into buffer */
    to = value;
    left = alloced - 1;
    if (*disposition == '\"') {
	/* Quoted-string */
	disposition++;
	while (*disposition && *disposition != '\"') {
	    if (!--left) {
		alloced += VALUEGROWSIZE;
		left += VALUEGROWSIZE;
		value = xrealloc(value, alloced);
		to = value + alloced - left - 2;
	    }
	    if (*disposition == '\\') {
		disposition++;
		if (!*disposition) return 0;
	    }
	    *to++ = *disposition++;
	}
	if (!*disposition) return 0;
    }
    else {
	/* Just a token */
	while (*disposition && !isspace(*disposition) &&
	       *disposition != '(') {
	    if (!--left) {
		alloced += VALUEGROWSIZE;
		left += VALUEGROWSIZE;
		value = xrealloc(value, alloced);
		to = value + alloced - left - 2;
	    }
	    *to++ = *disposition++;
	}
    }
    *to = '\0';
    return value;
}    

/*
 * Read and handle a message/partial object from the file 'inpart'.
 */
int handlePartial(struct part *inpart, char *headers, params contentParams, int extractText)
{
    char *id, *dir, *p;
    int thispart;
    int nparts = 0;
    char buf[1024];
    FILE *partfile, *outfile;
    struct part *outpart;
    int i, docopy;

    id = getParam(contentParams, "id");
    if (!id) {
	warn("partial message has no id parameter");
	goto ignore;
    }

    /* Get directory to store the parts being reassembled */
    dir = os_idtodir(id);
    if (!dir) goto ignore;

    p = getParam(contentParams, "number");
    if (!p) {
	warn("partial message doesn't have number parameter");
	goto ignore;
    }
    thispart = atoi(p);

    if ((p = getParam(contentParams, "total"))) {
	nparts = atoi(p);
	if (nparts <= 0) {
	    warn("partial message has invalid number of parts");
	    goto ignore;
	}
	/* Store number of parts in reassembly directory */
	sprintf(buf, "%sCT", dir);
	partfile = os_createnewfile(buf);
	if (!partfile) {
	    os_perror(buf);
	    goto ignore;
	}
	fprintf(partfile, "%d\n", nparts);
	fclose(partfile);
    }
    else {
	/* Try to retrieve number of parts from reassembly directory */
	sprintf(buf, "%sCT", dir);
	if ((partfile = fopen(buf, "r"))) {
	    if (fgets(buf, sizeof(buf), partfile)) {
		nparts = atoi(buf);
		if (nparts < 0) nparts = 0;
	    }
	    fclose(partfile);
	}
    }

    /* Sanity check */
    if (thispart <= 0 || (nparts && thispart > nparts)) {
	warn("partial message has invalid number");
	goto ignore;
    }

    sprintf(buf, "Saving part %d ", thispart);
    if (nparts) sprintf(buf+strlen(buf), "of %d ", nparts);
    strcat(buf, getParam(contentParams, "id"));
    chat(buf);

    /* Create file to store this part */
    sprintf(buf, "%s%d", dir, thispart);
    partfile = os_createnewfile(buf);
    if (!partfile) {
	os_perror(buf);
	goto ignore;
    }

    /* Do special-case header handling for first part */
    if (thispart == 1) {
	int skippedfirstbyte = 0;

	while (*headers) {
	    if (*headers == '\n' &&
		(!strncasecmp(headers, "\ncontent-", 9) ||
		 !strncasecmp(headers, "\nmessage-id:", 12))) {
		/* Special case, skip header */
		headers++;
		while (*headers && (*headers != '\n' || isspace(headers[1]))) {
		    headers++;
		}
	    }
	    else {
		/* First byte of headers is extra newline, don't write it to file */
		if (skippedfirstbyte++)	putc(*headers, partfile);
		headers++;
	    }
	}
	docopy = 0;
	/* Handle headers in the multipart/partial body */
	while (part_gets(buf, sizeof(buf), inpart)) {
	    if (*buf == '\n') {
		putc('\n', partfile);
		break;
	    }
	    if (!strncasecmp(buf, "content-", 8) || !strncasecmp(buf, "message-id:", 11)) {
		docopy = 1;
	    }
	    else if (!isspace(*buf)) {
		docopy = 0;
	    }

	    if (docopy) fputs(buf, partfile);
	    while(buf[strlen(buf)-1] != '\n' && part_gets(buf, sizeof(buf), inpart)) {
		if (docopy) fputs(buf, partfile);
	    }
	}
    }

    /* Copy the contents to the file */
    while (part_gets(buf, sizeof(buf), inpart)) {
	fputs(buf, partfile);
    }
    fclose(partfile);

    /* Check to see if we have all parts.  Start from the highest numbers
     * as we are more likely not to have them.
     */
    for (i = nparts; i; i--) {
	sprintf(buf, "%s%d", dir, i);
	partfile = fopen(buf, "r");
	if (partfile) {
	    fclose(partfile);
	}
	else {
	    break;
	}
    }

    if (i || !nparts) {
	/* We don't have all the parts yet */
	return 0;
    }

    /* We have everything, concatenate all the parts into a single file */
    sprintf(buf, "%sFULL", dir);
    outfile = os_createnewfile(buf);
    if (!outfile) {
	os_perror(buf);
	return 1;
    }
    for (i=1; i<=nparts; i++) {
	sprintf(buf, "%s%d", dir, i);
	partfile = fopen(buf, "r");
	if (!partfile) {
	    os_perror(buf);
	    return 1;
	}
	while (fgets(buf, sizeof(buf), partfile)) {
	    fputs(buf, outfile);
	}
	fclose(partfile);

	/* Done with message part file, delete it */
	sprintf(buf, "%s%d", dir, i);
	remove(buf);
    }

    /* Open the concatenated file for reading and handle it */
    fclose(outfile);
    sprintf(buf, "%sFULL", dir);
    outfile = fopen(buf, "r");
    if (!outfile) {
	os_perror(buf);
	return 1;
    }
    outpart = part_init(outfile);
    handleMessage(outpart, "text/plain", 0, extractText);
    part_close(outpart);

    /* Clean up the rest of the reassembly directory */
    sprintf(buf, "%sFULL", dir);
    remove(buf);
    sprintf(buf, "%sCT", dir);
    remove(buf);
    os_donewithdir(dir);

    return 0;

 ignore:
    ignoreMessage(inpart);
    return 1;
}

/*
 * Skip over a message object from the file 'inpart'.
 */
int ignoreMessage(struct part *inpart)
{
    while (part_getc(inpart) != EOF);
    return 0;
}

/*
 * Read and handle a multipart object from 'inpart'.
 */
int handleMultipart(struct part *inpart, char *contentType, params contentParams, int extractText)
{
    char *id;
    char *defaultContentType = "text/plain";
    int isAppleDouble = 0;

    /* Components of multipart/digest have a different default content-type */
    if (!strcasecmp(contentType, "multipart/digest")) {
	defaultContentType = "message/rfc822";
    }
    if (!strcasecmp(contentType, "multipart/appledouble")) {
	isAppleDouble++;
    }

    if (!(id = getParam(contentParams, "boundary"))) {
	warn("multipart message has no boundary parameter");
	id="";
    }

    /* Add the new boundary id */
    part_addboundary(inpart, id);

#ifdef __riscos
    /*
     * "Marcel" encodes RISCOS directory structure in the multipart
     * structure.  That is the Wrong Way to do it, but we hold our
     * nose and pass the information to the OS layer.
     */
    os_boundaryhookopen(part_depth(inpart));
#endif

    /*
     * Skip over preamble.
     * HACK: The initial boundary doesn't have to start with a newline,
     * so we deal with this by stuffing an initial newline into the input
     * stream
     */
    part_ungetc('\n', inpart);
    ignoreMessage(inpart);

    /* Handle the component messages */
    while (!part_readboundary(inpart)) {
	handleMessage(inpart, defaultContentType, isAppleDouble, extractText);
    }

#ifdef __riscos
    os_boundaryhookclose(part_depth(inpart));
#endif

    /* Skip over postamble */
    ignoreMessage(inpart);

    /* Remove any lingering unused description file */
    (void) remove(TEMPFILENAME);

    return 0;
}

/*
 * Handle a text message object from 'inpart' by saving it to
 * the temporary description file.
 */
int handleText(struct part *inpart, enum encoding contentEncoding)
{
    FILE *descfile;

    descfile = os_createnewfile(TEMPFILENAME);
    if (!descfile) {
	os_perror(TEMPFILENAME);
	ignoreMessage(inpart);
	return 1;
    }

    /* Write the file, handling the appropriate encoding */
    switch (contentEncoding) {
    case enc_none:
	fromnone(inpart, descfile, (char **)0);
	break;

    case enc_qp:
	fromqp(inpart, descfile, (char **)0);
	break;

    case enc_base64:
	from64(inpart, descfile, (char **)0, 1);
	break;
    }

    fclose(descfile);
    return 0;
}

/*
 * Read a message object from 'inpart' and save it to a file.
 */
int saveToFile(struct part *inpart, int inAppleDouble, char *contentType, params contentParams, enum encoding contentEncoding, char *contentDisposition, char *contentMD5)
{
    FILE *outfile = 0;
    int flags = 0;
    int suppressCR = 0;
    char *outputmd5;
    char *fname;

    if (!strncasecmp(contentType, "text/", 5)) {
	suppressCR = 1;
    }
    else if (contentEncoding == enc_base64) {
	/*
	 * HEURISTIC: It is not in general possible to determine whether
	 * any non-text content type is line-oriented.  We guess
	 * the "binary" status of a part from the composer's choice
	 * of content transfer encoding.
	 *
	 * If the content transfer encoding is "binary" and the input is
	 * not line-oriented, we're screwed anyway--the input file has
	 * been opened in text mode.  So the "binary output file" heuristic
	 * is not applied in this case.
	 */
	flags |= FILE_BINARY;
    }

    if (inAppleDouble) flags |= FILE_INAPPLEDOUBLE;
    
    /* Find an appropriate filename and create the output file */
    fname = getDispositionFilename(contentDisposition);
    if (!fname) fname = getParam(contentParams, "name");
    if (fname) fname = strsave(fname);
    outfile = os_newtypedfile(fname, contentType, flags, contentParams);
    if (fname) free(fname);
    if (!outfile) {
	ignoreMessage(inpart);
	return 1;
    }

    /* Write the file, handling the appropriate encoding */
    switch (contentEncoding) {
    case enc_none:
	fromnone(inpart, outfile, &outputmd5);
	break;

    case enc_qp:
	fromqp(inpart, outfile, &outputmd5);
	break;

    case enc_base64:
	from64(inpart, outfile, &outputmd5, suppressCR);
	break;
    }
    rewind(outfile);

    /* Check the MD5 digest if it was supplied */
    if (contentMD5) {
	if (strncmp(outputmd5, contentMD5, strlen(outputmd5)) != 0) {
	    os_warnMD5mismatch();
	}
    }
    free(outputmd5);

    os_closetypedfile(outfile);
    return 0;
}

#define XX 127
/*
 * Table for decoding hexadecimal in quoted-printable
 */
static char index_hex[256] = {
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
     0, 1, 2, 3,  4, 5, 6, 7,  8, 9,XX,XX, XX,XX,XX,XX,
    XX,10,11,12, 13,14,15,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,10,11,12, 13,14,15,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
};
#define HEXCHAR(c)  (index_hex[(unsigned char)(c)])

/*
 * Table for decoding base64
 */
static char index_64[256] = {
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,62, XX,XX,XX,63,
    52,53,54,55, 56,57,58,59, 60,61,XX,XX, XX,XX,XX,XX,
    XX, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14,
    15,16,17,18, 19,20,21,22, 23,24,25,XX, XX,XX,XX,XX,
    XX,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40,
    41,42,43,44, 45,46,47,48, 49,50,51,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
};
#define CHAR64(c)  (index_64[(unsigned char)(c)])

void from64(struct part *inpart, FILE *outfile, char **digestp, int suppressCR)
{
    int c1, c2, c3, c4;
    int DataDone = 0;
    char buf[3];
    MD5_CTX context;

    if (digestp) MD5Init(&context);
    while ((c1 = part_getc(inpart)) != EOF) {
        if (c1 != '=' && CHAR64(c1) == XX) {
            continue;
        }
        if (DataDone) continue;
        do {
            c2 = part_getc(inpart);
        } while (c2 != EOF && c2 != '=' && CHAR64(c2) == XX);
        do {
            c3 = part_getc(inpart);
        } while (c3 != EOF && c3 != '=' && CHAR64(c3) == XX);
        do {
            c4 = part_getc(inpart);
        } while (c4 != EOF && c4 != '=' && CHAR64(c4) == XX);
        if (c2 == EOF || c3 == EOF || c4 == EOF) {
            warn("Premature EOF");
            break;
        }
        if (c1 == '=' || c2 == '=') {
            DataDone=1;
            continue;
        }
        c1 = CHAR64(c1);
        c2 = CHAR64(c2);
	buf[0] = ((c1<<2) | ((c2&0x30)>>4));
        if (!suppressCR || buf[0] != '\r') putc(buf[0], outfile);
        if (c3 == '=') {
	    if (digestp) MD5Update(&context, buf, 1);
            DataDone = 1;
        } else {
            c3 = CHAR64(c3);
	    buf[1] = (((c2&0x0F) << 4) | ((c3&0x3C) >> 2));
            if (!suppressCR || buf[1] != '\r') putc(buf[1], outfile);
            if (c4 == '=') {
		if (digestp) MD5Update(&context, buf, 2);
                DataDone = 1;
            } else {
                c4 = CHAR64(c4);
		buf[2] = (((c3&0x03) << 6) | c4);
                if (!suppressCR || buf[2] != '\r') putc(buf[2], outfile);
		if (digestp) MD5Update(&context, buf, 3);		
            }
        }
    }
    if (digestp) *digestp = md5contextTo64(&context);
}

void fromqp(struct part *inpart, FILE *outfile, char **digestp)
{
    int c1, c2;
    MD5_CTX context;
    char c;

    if (digestp) MD5Init(&context);

    while ((c1 = part_getc(inpart)) != EOF) {
	if (c1 == '=') {
	    c1 = part_getc(inpart);
	    if (c1 != '\n') {
		c1 = HEXCHAR(c1);
		c2 = part_getc(inpart);
		c2 = HEXCHAR(c2);
		c = c1<<4 | c2;
		if (c != '\r') putc(c, outfile);
		if (digestp) MD5Update(&context, &c, 1);
	    }
	} else {
	    putc(c1, outfile);
	    if (c1 == '\n') {
		if (digestp) MD5Update(&context, "\r", 1);
	    }
	    c = c1;
	    if (digestp) MD5Update(&context, &c, 1);
	}
    }
    if (digestp) *digestp=md5contextTo64(&context);
}

void fromnone(struct part *inpart, FILE *outfile, char **digestp)
{
    int c;
    char ch;
    MD5_CTX context;

    if (digestp) MD5Init(&context);

    while ((c = part_getc(inpart)) != EOF) {
	putc(c, outfile);
	if (c == '\n') {
	    if (digestp) MD5Update(&context, "\r", 1);
	}
	ch = c;
	if (digestp) MD5Update(&context, &ch, 1);
    }
    if (digestp) *digestp=md5contextTo64(&context);
}

