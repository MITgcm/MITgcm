C $Header: /u/gcmpack/MITgcm/model/inc/Attic/RDYC_MACROS.h,v 1.1 1998/05/27 21:01:47 cnh Exp $
C
C     /==========================================================\
C     | RDYC_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RDYC_CONST
#define  _rdyC(i,j,bi,bj) rdyC(1,1,1,1)
#endif

#ifdef RDYC_FX
#define  _rdyC(i,j,bi,bj) rdyC(i,1,bi,1)
#endif

#ifdef RDYC_FY
#define  _rdyC(i,j,bi,bj) rdyC(1,j,1,bj)
#endif

#ifndef _rdyC
#define  _rdyC(i,j,bi,bj) rdyC(i,j,bi,bj)
#endif
