C $Header: /u/gcmpack/MITgcm/model/inc/XC_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: XC_MACROS.h
C    !INTERFACE:
C    include XC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | XC_MACROS.h                                               
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef XC_CONST
#define  _xC(i,j,bi,bj) xC(1,1,1,1)
#endif

#ifdef XC_FX
#define  _xC(i,j,bi,bj) xC(i,1,bi,1)
#endif

#ifdef XC_FY
#define  _xC(i,j,bi,bj) xC(1,j,1,bj)
#endif

#ifndef _xC
#define  _xC(i,j,bi,bj) xC(i,j,bi,bj)
#endif
