C $Header: /u/gcmpack/MITgcm/model/inc/Attic/RDXF_MACROS.h,v 1.1 1998/05/27 21:01:46 cnh Exp $
C
C     /==========================================================\
C     | RDXF_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RDXF_CONST
#define  _rdxF(i,j,bi,bj) rdxF(1,1,1,1)
#endif

#ifdef RDXF_FX
#define  _rdxF(i,j,bi,bj) rdxF(i,1,bi,1)
#endif

#ifdef RDXF_FY
#define  _rdxF(i,j,bi,bj) rdxF(1,j,1,bj)
#endif

#ifndef _rdxF
#define  _rdxF(i,j,bi,bj) rdxF(i,j,bi,bj)
#endif
