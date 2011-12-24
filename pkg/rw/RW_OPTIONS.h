C $Header: /u/gcmpack/MITgcm/pkg/rw/RW_OPTIONS.h,v 1.4 2011/12/24 01:09:40 jmc Exp $
C $Name:  $

C CPP options file for Read-Write (RW) package
C Use this file for selecting options within the RW package

#ifndef RW_OPTIONS_H
#define RW_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_RW
C     Package-specific Options & Macros go here

C use READ_MFLDS S/R in "safe" mode (set/check/unset for each file to read)
C involves more thread synchronization (could slow down multi-threaded run)
#define RW_SAFE_MFLDS

C disable writing of small-overlap size array (to reduce memory size
C   since those S/R do a local copy to 3-D full-size overlap array)
#undef RW_DISABLE_SMALL_OVERLAP

#endif /* ALLOW_RW */
#endif /* RW_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
