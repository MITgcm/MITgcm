C $Header: /u/gcmpack/MITgcm/model/inc/HFACC_MACROS.h,v 1.4 2006/06/07 01:55:12 heimbach Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: HFACC_MACROS.h
C    !INTERFACE:
C    include HFACC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | HFACC_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

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

#ifdef ALLOW_DEPTH_CONTROL
# define _hFacC(i,j,k,bi,bj) hFacC(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
#endif

#ifndef _hFacC
#define  _hFacC(i,j,k,bi,bj) hFacC(i,j,k,bi,bj)
#endif
