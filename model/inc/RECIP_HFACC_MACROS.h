C $Header: /u/gcmpack/MITgcm/model/inc/RECIP_HFACC_MACROS.h,v 1.1 1998/08/15 16:55:48 cnh Exp $
C
C     /==========================================================\
C     | RECIP_HFACC_MACROS.h                                     |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RECIP_HFACC_CONST
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(1,1,1,1,1)
#endif

#ifdef RECIP_HFACC_FX
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(i,1,1,bi,1)
#endif

#ifdef RECIP_HFACC_FY
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(1,j,1,1,bj)
#endif

#ifdef RECIP_HFACC_FXY
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(i,j,1,bi,bj)
#endif

#ifndef _recip_hFacC
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(i,j,k,bi,bj)
#endif
