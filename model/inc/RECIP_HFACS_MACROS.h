C $Header: /u/gcmpack/MITgcm/model/inc/RECIP_HFACS_MACROS.h,v 1.1 1998/08/15 16:55:49 cnh Exp $
C
C     /==========================================================\
C     | RECIP_HFACS_MACROS.h                                     |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RECIP_HFACS_CONST
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(1,1,1,1,1)
#endif

#ifdef RECIP_HFACS_FX
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(i,1,1,bi,1)
#endif

#ifdef RECIP_HFACS_FY
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(1,j,1,1,bj)
#endif

#ifdef RECIP_HFACS_FXY
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(i,j,1,bi,bj)
#endif

#ifndef _recip_hFacS
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(i,j,k,bi,bj)
#endif
