C $Header: /u/gcmpack/MITgcm/model/inc/TANPHIATV_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: TANPHIATV_MACROS.h
C    !INTERFACE:
C    include TANPHIATV_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | TANPHIATV_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef TANPHIATV_CONST
#define  _tanPhiAtV(i,j,bi,bj) tanPhiAtV(1,1,1,1)
#endif

#ifdef TANPHIATV_FX
#define  _tanPhiAtV(i,j,bi,bj) tanPhiAtV(i,1,bi,1)
#endif

#ifdef TANPHIATV_FY
#define  _tanPhiAtV(i,j,bi,bj) tanPhiAtV(1,j,1,bj)
#endif

#ifndef _tanPhiAtV
#define  _tanPhiAtV(i,j,bi,bj) tanPhiAtV(i,j,bi,bj)
#endif
