C $Header: /u/gcmpack/MITgcm/model/inc/Attic/ZA_MACROS.h,v 1.1 1998/05/30 02:10:16 cnh Exp $
C
C     /==========================================================\
C     | ZA_MACROS.h                                              |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef ZA_CONST
#define  _zA(i,j,bi,bj) zA(1,1,1,1)
#endif

#ifdef ZA_FX
#define  _zA(i,j,bi,bj) zA(i,1,bi,1)
#endif

#ifdef ZA_FY
#define  _zA(i,j,bi,bj) zA(1,j,1,bj)
#endif

#ifndef _zA
#define  _zA(i,j,bi,bj) zA(i,j,bi,bj)
#endif
