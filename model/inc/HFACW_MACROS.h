C $Header: /u/gcmpack/MITgcm/model/inc/HFACW_MACROS.h,v 1.1 1998/05/27 21:01:46 cnh Exp $
C
C     /==========================================================\
C     | HFACW_MACROS.h                                           |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef HFACW_CONST
#define  _hFacW(i,j,k,bi,bj) hFacW(1,1,1,1,1)
#endif

#ifdef HFACW_FX
#define  _hFacW(i,j,k,bi,bj) hFacW(i,1,1,bi,1)
#endif

#ifdef HFACW_FY
#define  _hFacW(i,j,k,bi,bj) hFacW(1,j,1,1,bj)
#endif

#ifdef HFACW_FXY
#define  _hFacW(i,j,k,bi,bj) hFacW(i,j,1,bi,bj)
#endif

#ifndef _hFacW
#define  _hFacW(i,j,k,bi,bj) hFacW(i,j,k,bi,bj)
#endif
