C $Header: /u/gcmpack/MITgcm/model/inc/HFACW_MACROS.h,v 1.4 2006/06/07 01:55:12 heimbach Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: HFACW_MACROS.h
C    !INTERFACE:
C    include HFACW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | HFACW_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

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

#ifdef ALLOW_DEPTH_CONTROL
# define _hFacW(i,j,k,bi,bj) hFacW(i,j,k,bi,bj)*maskW(i,j,k,bi,bj)
#endif

#ifndef _hFacW
#define  _hFacW(i,j,k,bi,bj) hFacW(i,j,k,bi,bj)
#endif
