C $Header: /u/gcmpack/MITgcm/pkg/mnc/MNC_OPTIONS.h,v 1.8 2011/12/24 01:09:39 jmc Exp $
C $Name:  $

#ifndef MNC_OPTIONS_H
#define MNC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_MNC

#include "MNC_ID_HEADER.h"

CEH3 package-specific options go here

C     #define MNC_DEBUG

C     #define MNC_DEBUG_GTYPE  /*  Adds MNC gtype as an attribute  */

C     The following are for back-wards compatibility with "old-style" mnc
C     file names:
C
C     #define MNC_WRITE_OLDNAMES
C     #define MNC_READ_OLDNAMES

C     These are the default minimum number of characters used for the
C     per-file and per-tile file names
#define MNC_DEF_FMNC  3
#define MNC_DEF_TMNC  3

#endif /* ALLOW_MNC */
#endif /* MNC_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
