C $Header: /u/gcmpack/MITgcm/model/inc/FCORI_MACROS.h,v 1.1 1998/05/30 02:10:15 cnh Exp $
C
C     /==========================================================\
C     | FCORI_MACROS.h                                           |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef FCORI_CONST
#define  _fCori(i,j,bi,bj) fCori(1,1,1,1)
#endif

#ifdef FCORI_FX
#define  _fCori(i,j,bi,bj) fCori(i,1,bi,1)
#endif

#ifdef FCORI_FY
#define  _fCori(i,j,bi,bj) fCori(1,j,1,bj)
#endif

#ifndef _fCori
#define  _fCori(i,j,bi,bj) fCori(i,j,bi,bj)
#endif
