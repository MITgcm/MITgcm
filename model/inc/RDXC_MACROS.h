C $Header: /u/gcmpack/MITgcm/model/inc/Attic/RDXC_MACROS.h,v 1.1 1998/05/27 21:01:46 cnh Exp $
C
C     /==========================================================\
C     | RDXC_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RDXC_CONST
#define  _rdxC(i,j,bi,bj) rdxC(1,1,1,1)
#endif

#ifdef RDXC_FX
#define  _rdxC(i,j,bi,bj) rdxC(i,1,bi,1)
#endif

#ifdef RDXC_FY
#define  _rdxC(i,j,bi,bj) rdxC(1,j,1,bj)
#endif

#ifndef _rdxC
#define  _rdxC(i,j,bi,bj) rdxC(i,j,bi,bj)
#endif
