C $Header: /u/gcmpack/MITgcm/model/inc/DYF_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: DYF_MACROS.h
C    !INTERFACE:
C    include DYF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYF_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef DYF_CONST
#define  _dyF(i,j,bi,bj) dyF(1,1,1,1)
#endif

#ifdef DYF_FX
#define  _dyF(i,j,bi,bj) dyF(i,1,bi,1)
#endif

#ifdef DYF_FY
#define  _dyF(i,j,bi,bj) dyF(1,j,1,bj)
#endif

#ifndef _dyF
#define  _dyF(i,j,bi,bj) dyF(i,j,bi,bj)
#endif
