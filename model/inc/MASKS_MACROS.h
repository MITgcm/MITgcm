C $Header: /u/gcmpack/MITgcm/model/inc/MASKS_MACROS.h,v 1.1 1998/05/30 02:10:15 cnh Exp $
C
C     /==========================================================\
C     | MASKS_MACROS.h                                           |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef MASKS_CONST
#define  _maskS(i,j,k,bi,bj) maskS(1,1,1,1,1)
#endif

#ifdef MASKS_FX
#define  _maskS(i,j,k,bi,bj) maskS(i,1,1,bi,1)
#endif

#ifdef MASKS_FY
#define  _maskS(i,j,k,bi,bj) maskS(1,j,1,1,bj)
#endif

#ifdef MASKS_FXY
#define  _maskS(i,j,k,bi,bj) maskS(i,j,1,bi,bj)
#endif

#ifndef _maskS
#define  _maskS(i,j,k,bi,bj) maskS(i,j,k,bi,bj)
#endif
