C $Header: /u/gcmpack/MITgcm/model/inc/RECIP_DXF_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: RECIP_DXF_MACROS.h
C    !INTERFACE:
C    include RECIP_DXF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DXF_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef RECIP_DXF_CONST
#define  _recip_dxF(i,j,bi,bj) recip_dxF(1,1,1,1)
#endif

#ifdef RECIP_DXF_FX
#define  _recip_dxF(i,j,bi,bj) recip_dxF(i,1,bi,1)
#endif

#ifdef RECIP_DXF_FY
#define  _recip_dxF(i,j,bi,bj) recip_dxF(1,j,1,bj)
#endif

#ifndef _recip_dxF
#define  _recip_dxF(i,j,bi,bj) recip_dxF(i,j,bi,bj)
#endif
