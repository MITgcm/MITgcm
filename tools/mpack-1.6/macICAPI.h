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
/*
	The canonical Internet Config interface is defined in Pascal.  These headers have
	not been thoroughly tested.  If there is a conflict between these headers and the
	Pascal interfaces, the Pascal should take precedence.
*/

/* ///////////////////////////////////////////////////////////////////////////////// */

#ifndef __ICAPI__
#define __ICAPI__

#ifndef __TYPES__
#include <Types.h>
#endif

#ifndef __FILES__
#include <Files.h>
#endif

#ifndef __ICTYPES__
#include <ICTypes.h>
#endif

/* ///////////////////////////////////////////////////////////////////////////////// */

#ifdef __cplusplus
extern "C" {
#endif

	pascal ICError ICStart(ICInstance *inst, OSType creator);
	/* call at application initialisation */

	pascal ICError ICStop(ICInstance inst);
	/* call at application termination */

	pascal ICError ICFindConfigFile(ICInstance inst, short count, ICDirSpecArrayPtr folders);
	/* count is the number of ICDirSpecs that are valid in folders */
	/* searches the specified folders first, then backs out to preferences folder */
	/* don't you worry about how it finds the file (; */
	/* you can pass nil to folders if count is 0 */

	pascal ICError ICSpecifyConfigFile(ICInstance inst, FSSpec *config);
	/* for use *only* by Internet Configuration application */

	pascal ICError ICGetSeed(ICInstance inst, long *seed);
	/* returns current seed for prefs file */
	/* this seed changes every time a preference is modified */
	/* poll this to detect preference changes by other applications */

	pascal ICError ICGetPerm(ICInstance inst, ICPerm *perm);
	/* returns the permissions currently associated with this file */
	/* mainly used by overriding components, applications normally */
	/* know what permissions they have */

	pascal ICError ICBegin(ICInstance inst, ICPerm perm);
	/* start reading/writing the preferences */
	/* must be balanaced by a ICEnd */
	/* do not call WaitNextEvent between this pair */
	/* specify either icReadOnlyPerm or icReadWritePerm */
	/* note that this may open resource files and leave them open until ICEnd */

	pascal ICError ICGetPref(ICInstance inst, ConstStr255Param key, ICAttr *attr, Ptr buf, long *size);
	/* this routine may be called without a ICBegin/ICEnd pair, in which case */
	/* it implicitly calls ICBegin(inst, icReadOnlyPerm */
	/* given a key string, returns the attributes and the (optionally) the data for a preference */
	/* key must not be the empty string */
	/* if buf is nil then no data fetched and incoming size is ignored*/
	/* size must be non-negative, is size of allocated space for data at buf */
	/* attr and size and always set on return */
	/* size is actual size of data (if key present); */
	/* attr is pref attributes */
 	/* if icTruncatedErr then everything is valid, except you lost some data, size is size of real data*/
	/* on other errors, attr is ICattr_no_change and size is 0 */

	pascal ICError ICSetPref(ICInstance inst, ConstStr255Param key, ICAttr attr, Ptr buf, long size);
	/* this routine may be called without a ICBegin/ICEnd pair, in which case */
	/* it implicitly calls ICBegin(inst, icReadWritePerm */
	/* given a key string, sets the attributes and the data for a preference (either is optional); */
	/* key must not be the empty string */
	/* if buf is nil then no data stored and size is ignored, used for setting attr */
	/* size must be non-negative, is size of the data at buf to store */
	/* icPermErr if ICBegin was given icReadOnlyPerm */
	/* icPermErr if current attr is locked, new attr is locked and buf <> nil */

	pascal ICError ICCountPref(ICInstance inst, long *count);
	/* count total number of preferences */
	/* if error then count is 0 */

	pascal ICError ICGetIndPref(ICInstance inst, long n, Str255 key);
	/* return the key of the Nth preference */
	/* n must be positive */
	/* icPrefNotFoundErr if n is beyond the last preference */

	pascal ICError ICDeletePref(ICInstance inst, ConstStr255Param key);
	/* delete the preference specified by key */
	/* key must not be the empty string */
	/* preference specified by key must be present */
	/* icPrefNotFoundErr if it isn't */

	pascal ICError ICEnd(ICInstance inst);
	/* stop reading/writing the preferences */

	pascal ICError ICDefaultFileName(ICInstance inst, Str63 name);
	/* return the default file name */
	/* the component calls this routine to set up the default internet configuration file name*/
	/* this allows this operation to be intercepted by a component that has captured us */
	/* it currently gets it from the component resource file */
	/* the glue version is hardwired */

	pascal ICError ICGetComponentInstance(ICInstance inst, Ptr *component_inst);
	/* returns noErr and the component instance that we're talking to, if we're using the component */
	/* returns an error and nil if we're doing it with glue */

#ifdef __cplusplus
}
#endif __cplusplus

#endif
