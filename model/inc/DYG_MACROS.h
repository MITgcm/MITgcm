C $Header: /u/gcmpack/MITgcm/model/inc/DYG_MACROS.h,v 1.3 2001/02/04 14:38:44 cnh Exp $
C $Name:  $
C
C     /==========================================================\
C     | DYG_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef DYG_CONST
#define  _dyG(i,j,bi,bj) dyG(1,1,1,1)
#endif

#ifdef DYG_FX
#define  _dyG(i,j,bi,bj) dyG(i,1,bi,1)
#endif

#ifdef DYG_FY
#define  _dyG(i,j,bi,bj) dyG(1,j,1,bj)
#endif

#ifndef _dyG
#define  _dyG(i,j,bi,bj) dyG(i,j,bi,bj)
#endif
