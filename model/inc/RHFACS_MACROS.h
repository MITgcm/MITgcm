C $Header: /u/gcmpack/MITgcm/model/inc/Attic/RHFACS_MACROS.h,v 1.1 1998/05/28 15:03:13 cnh Exp $
C
C     /==========================================================\
C     | RHFACS_MACROS.h                                           |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RHFACS_CONST
#define  _rhFacS(i,j,k,bi,bj) rhFacS(1,1,1,1,1)
#endif

#ifdef RHFACS_FX
#define  _rhFacS(i,j,k,bi,bj) rhFacS(i,1,1,bi,1)
#endif

#ifdef RHFACS_FY
#define  _rhFacS(i,j,k,bi,bj) rhFacS(1,j,1,1,bj)
#endif

#ifdef RHFACS_FXY
#define  _rhFacS(i,j,k,bi,bj) rhFacS(i,j,1,bi,bj)
#endif

#ifndef _rhFacS
#define  _rhFacS(i,j,k,bi,bj) rhFacS(i,j,k,bi,bj)
#endif
