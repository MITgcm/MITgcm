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

#ifdef __OS2__ 
# include <io.h>
#endif

#if defined(__MSDOS__) || defined(__OS2__)
#define ERR(s, c)					\
    if (opterr) {					\
	char buff[3];					\
	buff[0] = c; buff[1] = '\r'; buff[2] = '\n';	\
	(void)write(2, av[0], strlen(av[0]));		\
	(void)write(2, s, strlen(s));			\
	(void)write(2, buff, 3);			\
    }
#else /* __MSDOS__ */
#define ERR(s, c)					\
    if (opterr) {					\
	char buff[2];					\
	buff[0] = c; buff[1] = '\n';			\
	(void)write(2, av[0], strlen(av[0]));		\
	(void)write(2, s, strlen(s));			\
	(void)write(2, buff, 2);			\
    }
#endif

int	opterr = 1;
int	optind = 1;
int	optopt;
char	*optarg;


/*
**  Return options and their values from the command line.
**  This comes from the AT&T public-domain getopt published in mod.sources
**  (i.e., comp.sources.unix before the great Usenet renaming).
*/
int
getopt(int ac, char **av, char *opts)
{
    extern char	*strchr(const char *, int);
    static int	i = 1;
    char	*p;

    /* Move to next value from argv? */
    if (i == 1) {
	if (optind >= ac ||
#if defined(__MSDOS__) || defined(__OS2__)
	    (av[optind][0] != '-' && av[optind][0] != '/')
#else
	    av[optind][0] != '-'
#endif
	    || av[optind][1] == '\0')
	    return EOF;
	if (strcmp(av[optind], "--") == 0) {
	    optind++;
	    return EOF;
	}
    }

    /* Get next option character. */
    if ((optopt = av[optind][i]) == ':'
     || (p = strchr(opts,  optopt)) == NULL) {
	ERR(": illegal option -- ", optopt);
	if (av[optind][++i] == '\0') {
	    optind++;
	    i = 1;
	}
	return '?';
    }

    /* Snarf argument? */
    if (*++p == ':') {
	if (av[optind][i + 1] != '\0')
	    optarg = &av[optind++][i + 1];
	else {
	    if (++optind >= ac) {
		ERR(": option requires an argument -- ", optopt);
		i = 1;
		return '?';
	    }
	    optarg = av[optind++];
	}
	i = 1;
    }
    else {
	if (av[optind][++i] == '\0') {
	    i = 1;
	    optind++;
	}
	optarg = NULL;
    }

    return optopt;
}
