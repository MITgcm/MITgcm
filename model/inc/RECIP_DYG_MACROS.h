C $Header: /u/gcmpack/MITgcm/model/inc/RECIP_DYG_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: RECIP_DYG_MACROS.h
C    !INTERFACE:
C    include RECIP_DYG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYG_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef RECIP_DYG_CONST
#define  _recip_dyG(i,j,bi,bj) recip_dyG(1,1,1,1)
#endif

#ifdef RECIP_DYG_FX
#define  _recip_dyG(i,j,bi,bj) recip_dyG(i,1,bi,1)
#endif

#ifdef RECIP_DYG_FY
#define  _recip_dyG(i,j,bi,bj) recip_dyG(1,j,1,bj)
#endif

#ifndef _recip_dyG
#define  _recip_dyG(i,j,bi,bj) recip_dyG(i,j,bi,bj)
#endif
