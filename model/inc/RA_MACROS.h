C $Header: /u/gcmpack/MITgcm/model/inc/RA_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: RA_MACROS.h
C    !INTERFACE:
C    include RA_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RA_MACROS.h                                               
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef RA_CONST
#define  _rA(i,j,bi,bj) rA(1,1,1,1)
#endif

#ifdef RA_FX
#define  _rA(i,j,bi,bj) rA(i,1,bi,1)
#endif

#ifdef RA_FY
#define  _rA(i,j,bi,bj) rA(1,j,1,bj)
#endif

#ifndef _rA
#define  _rA(i,j,bi,bj) rA(i,j,bi,bj)
#endif
