C $Header: /u/gcmpack/MITgcm/model/inc/RECIP_DYC_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: RECIP_DYC_MACROS.h
C    !INTERFACE:
C    include RECIP_DYC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYC_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef RECIP_DYC_CONST
#define  _recip_dyC(i,j,bi,bj) recip_dyC(1,1,1,1)
#endif

#ifdef RECIP_DYC_FX
#define  _recip_dyC(i,j,bi,bj) recip_dyC(i,1,bi,1)
#endif

#ifdef RECIP_DYC_FY
#define  _recip_dyC(i,j,bi,bj) recip_dyC(1,j,1,bj)
#endif

#ifndef _recip_dyC
#define  _recip_dyC(i,j,bi,bj) recip_dyC(i,j,bi,bj)
#endif
