C $Header: /u/gcmpack/MITgcm/model/inc/DYC_MACROS.h,v 1.2 2001/02/04 14:38:44 cnh Exp $
C $Name:  $
C
C     /==========================================================\
C     | DYC_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef DYC_CONST
#define  _dyC(i,j,bi,bj) dyC(1,1,1,1)
#endif

#ifdef DYC_FX
#define  _dyC(i,j,bi,bj) dyC(i,1,bi,1)
#endif

#ifdef DYC_FY
#define  _dyC(i,j,bi,bj) dyC(1,j,1,bj)
#endif

#ifndef _dyC
#define  _dyC(i,j,bi,bj) dyC(i,j,bi,bj)
#endif
