C $Header: /u/gcmpack/MITgcm/model/inc/Attic/RHFACW_MACROS.h,v 1.2 1998/05/28 15:09:29 cnh Exp $
C
C     /==========================================================\
C     | RHFACW_MACROS.h                                          |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RHFACW_CONST
#define  _rhFacW(i,j,k,bi,bj) rhFacW(1,1,1,1,1)
#endif

#ifdef RHFACW_FX
#define  _rhFacW(i,j,k,bi,bj) rhFacW(i,1,1,bi,1)
#endif

#ifdef RHFACW_FY
#define  _rhFacW(i,j,k,bi,bj) rhFacW(1,j,1,1,bj)
#endif

#ifdef RHFACW_FXY
#define  _rhFacW(i,j,k,bi,bj) rhFacW(i,j,1,bi,bj)
#endif

#ifndef _rhFacW
#define  _rhFacW(i,j,k,bi,bj) rhFacW(i,j,k,bi,bj)
#endif
