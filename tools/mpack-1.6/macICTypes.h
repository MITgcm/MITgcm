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
	The canonical Internet Config interface is defined in Pascal.  These headers have
	not been thoroughly tested.  If there is a conflict between these headers and the
	Pascal interfaces, the Pascal should take precedence.
*/

/* ///////////////////////////////////////////////////////////////////////////////// */

#ifndef __ICTYPES__
#define __ICTYPES__

#ifndef __TYPES__
#include <Types.h>
#endif

/* ///////////////////////////////////////////////////////////////////////////////// */

#if defined(powerc) || defined (__powerc)
#pragma options align=mac68k
#endif

enum {
	icPrefNotFoundErr = -666,		/* preference not found (duh!) */
	icPermErr = -667,						/* cannot set preference */
	icPrefDataErr = -668,				/* problem with preference data */
	icInternalErr = -669,				/* hmm, this is not good */
	icTruncatedErr = -670,			/* more data was present than was returned */
	icNoMoreWritersErr = -671		/* you cannot begin a write session because someone else is already doing it */
};

enum {
	ICattr_no_change = 0xFFFFFFFFL,				/* supply this to ICSetPref to tell it not to change the attributes */
	ICattr_locked_bit = 0,								/* bits in the preference attributes */
	ICattr_locked_mask = 0x00000001L			/* masks for the above */
};

#define ICfiletype 'ICAp'
#define ICcreator 'ICAp'

#define ICdefault_file_name "\pInternet Preferences"	/* default file name, for internal use, overridden by a component resource */

enum {
	ICdefault_file_name_ID = 1024					/* ID of resource in component file */
};

struct ICDirSpec {											/* a record that specifies a folder */
	short vRefNum;
	long dirID;
};
typedef struct ICDirSpec ICDirSpec;

typedef ICDirSpec ICDirSpecArray[4];		/* an array of the above */
typedef ICDirSpecArray *ICDirSpecArrayPtr;	/* a pointer to that array */

typedef long ICAttr;										/* type for preference attributes */
typedef long ICError;										/* type for error codes */
typedef Ptr ICInstance;									/* opaque type for preference reference */
enum {
	icNoPerm = 0,
	icReadOnlyPerm = 1,
	icReadWritePerm = 2
};
typedef unsigned char ICPerm;

#if defined(powerc) || defined(__powerc)
#pragma options align=reset
#endif

#endif
