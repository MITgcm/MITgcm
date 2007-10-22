C $Header: /u/gcmpack/MITgcm/pkg/rw/RW_OPTIONS.h,v 1.2 2007/10/22 13:20:07 jmc Exp $
C $Name:  $

C CPP options file for RW package
C
C Use this file for selecting options within the RW package

#ifndef RW_OPTIONS_H
#define RW_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_RW

#include "CPP_OPTIONS.h"

C use READ_MFLDS S/R in "safe" mode (set/check/unset for each file to read)
C involves more thread synchronization (could slow down multi-threaded run)
#define RW_SAFE_MFLDS

#endif /* ALLOW_RW */
#endif /* RW_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
