C $Header: /u/gcmpack/MITgcm/model/inc/DYU_MACROS.h,v 1.2 2001/02/04 14:38:44 cnh Exp $
C $Name:  $
C
C     /==========================================================\
C     | DYU_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef DYU_CONST
#define  _dyU(i,j,bi,bj) dyU(1,1,1,1)
#endif

#ifdef DYU_FX
#define  _dyU(i,j,bi,bj) dyU(i,1,bi,1)
#endif

#ifdef DYU_FY
#define  _dyU(i,j,bi,bj) dyU(1,j,1,bj)
#endif

#ifndef _dyU
#define  _dyU(i,j,bi,bj) dyU(i,j,bi,bj)
#endif
