C $Header: /u/gcmpack/MITgcm/model/inc/HFACC_MACROS.h,v 1.1 1998/05/27 21:01:46 cnh Exp $
C
C     /==========================================================\
C     | HFACC_MACROS.h                                           |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef HFACC_CONST
#define  _hFacC(i,j,k,bi,bj) hFacC(1,1,1,1,1)
#endif

#ifdef HFACC_FX
#define  _hFacC(i,j,k,bi,bj) hFacC(i,1,1,bi,1)
#endif

#ifdef HFACC_FY
#define  _hFacC(i,j,k,bi,bj) hFacC(1,j,1,1,bj)
#endif

#ifdef HFACC_FXY
#define  _hFacC(i,j,k,bi,bj) hFacC(i,j,1,bi,bj)
#endif

#ifndef _hFacC
#define  _hFacC(i,j,k,bi,bj) hFacC(i,j,k,bi,bj)
#endif
