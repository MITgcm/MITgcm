C $Header: /u/gcmpack/MITgcm/model/inc/YC_MACROS.h,v 1.2 2001/02/04 14:38:45 cnh Exp $
C $Name:  $
C
C     /==========================================================\
C     | YC_MACROS.h                                              |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef YC_CONST
#define  _yC(i,j,bi,bj) yC(1,1,1,1)
#endif

#ifdef YC_FX
#define  _yC(i,j,bi,bj) yC(i,1,bi,1)
#endif

#ifdef YC_FY
#define  _yC(i,j,bi,bj) yC(1,j,1,bj)
#endif

#ifndef _yC
#define  _yC(i,j,bi,bj) yC(i,j,bi,bj)
#endif
