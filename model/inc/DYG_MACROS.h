C $Header: /u/gcmpack/MITgcm/model/inc/DYG_MACROS.h,v 1.4 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: DYG_MACROS.h
C    !INTERFACE:
C    include DYG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYG_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef DYG_CONST
#define  _dyG(i,j,bi,bj) dyG(1,1,1,1)
#endif

#ifdef DYG_FX
#define  _dyG(i,j,bi,bj) dyG(i,1,bi,1)
#endif

#ifdef DYG_FY
#define  _dyG(i,j,bi,bj) dyG(1,j,1,bj)
#endif

#ifndef _dyG
#define  _dyG(i,j,bi,bj) dyG(i,j,bi,bj)
#endif
