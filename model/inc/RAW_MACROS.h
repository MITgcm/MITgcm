C $Header: /u/gcmpack/MITgcm/model/inc/RAW_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: RAW_MACROS.h
C    !INTERFACE:
C    include RAW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RAW_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef RA_CONST
#define  _rAw(i,j,bi,bj) rAw(1,1,1,1)
#endif

#ifdef RA_FX
#define  _rAw(i,j,bi,bj) rAw(i,1,bi,1)
#endif

#ifdef RA_FY
#define  _rAw(i,j,bi,bj) rAw(1,j,1,bj)
#endif

#ifndef _rAw
#define  _rAw(i,j,bi,bj) rAw(i,j,bi,bj)
#endif
