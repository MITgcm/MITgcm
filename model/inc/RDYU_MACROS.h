C $Header: /u/gcmpack/MITgcm/model/inc/Attic/RDYU_MACROS.h,v 1.1 1998/05/27 21:01:47 cnh Exp $
C
C     /==========================================================\
C     | RDYU_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RDYU_CONST
#define  _rdyU(i,j,bi,bj) rdyU(1,1,1,1)
#endif

#ifdef RDYU_FX
#define  _rdyU(i,j,bi,bj) rdyU(i,1,bi,1)
#endif

#ifdef RDYU_FY
#define  _rdyU(i,j,bi,bj) rdyU(1,j,1,bj)
#endif

#ifndef _rdyU
#define  _rdyU(i,j,bi,bj) rdyU(i,j,bi,bj)
#endif
