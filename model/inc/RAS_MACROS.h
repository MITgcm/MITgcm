C $Header: /u/gcmpack/MITgcm/model/inc/RAS_MACROS.h,v 1.1 1998/11/30 23:45:24 adcroft Exp $
C
C     /==========================================================\
C     | RAS_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RA_CONST
#define  _rAs(i,j,bi,bj) rAs(1,1,1,1)
#endif

#ifdef RA_FX
#define  _rAs(i,j,bi,bj) rAs(i,1,bi,1)
#endif

#ifdef RA_FY
#define  _rAs(i,j,bi,bj) rAs(1,j,1,bj)
#endif

#ifndef _rAs
#define  _rAs(i,j,bi,bj) rAs(i,j,bi,bj)
#endif
