C $Header: /u/gcmpack/MITgcm/model/inc/RECIP_DYF_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: RECIP_DYF_MACROS.h
C    !INTERFACE:
C    include RECIP_DYF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYF_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef RECIP_DYF_CONST
#define  _recip_dyF(i,j,bi,bj) recip_dyF(1,1,1,1)
#endif

#ifdef RECIP_DYF_FX
#define  _recip_dyF(i,j,bi,bj) recip_dyF(i,1,bi,1)
#endif

#ifdef RECIP_DYF_FY
#define  _recip_dyF(i,j,bi,bj) recip_dyF(1,j,1,bj)
#endif

#ifndef _recip_dyF
#define  _recip_dyF(i,j,bi,bj) recip_dyF(i,j,bi,bj)
#endif
