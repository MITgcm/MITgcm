C $Header: /u/gcmpack/MITgcm/model/inc/DYC_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: DYC_MACROS.h
C    !INTERFACE:
C    include DYC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYC_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef DYC_CONST
#define  _dyC(i,j,bi,bj) dyC(1,1,1,1)
#endif

#ifdef DYC_FX
#define  _dyC(i,j,bi,bj) dyC(i,1,bi,1)
#endif

#ifdef DYC_FY
#define  _dyC(i,j,bi,bj) dyC(1,j,1,bj)
#endif

#ifndef _dyC
#define  _dyC(i,j,bi,bj) dyC(i,j,bi,bj)
#endif
