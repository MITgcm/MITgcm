C $Header: /u/gcmpack/MITgcm/model/inc/Attic/RHFACC_MACROS.h,v 1.1 1998/05/28 15:03:13 cnh Exp $
C
C     /==========================================================\
C     | RHFACC_MACROS.h                                          |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RHFACC_CONST
#define  _rhFacC(i,j,k,bi,bj) rhFacC(1,1,1,1,1)
#endif

#ifdef RHFACC_FX
#define  _rhFacC(i,j,k,bi,bj) rhFacC(i,1,1,bi,1)
#endif

#ifdef RHFACC_FY
#define  _rhFacC(i,j,k,bi,bj) rhFacC(1,j,1,1,bj)
#endif

#ifdef RHFACC_FXY
#define  _rhFacC(i,j,k,bi,bj) rhFacC(i,j,1,bi,bj)
#endif

#ifndef _hFacC
#define  _hFacC(i,j,k,bi,bj) hFacC(i,j,k,bi,bj)
#endif
