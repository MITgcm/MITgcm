C $Header: /u/gcmpack/MITgcm/model/inc/DXF_MACROS.h,v 1.4 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: DXF_MACROS.h
C    !INTERFACE:
C    include DXF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXF_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef DXF_CONST
#define  _dxF(i,j,bi,bj) dxF(1,1,1,1)
#endif

#ifdef DXF_FX
#define  _dxF(i,j,bi,bj) dxF(i,1,bi,1)
#endif

#ifdef DXF_FY
#define  _dxF(i,j,bi,bj) dxF(1,j,1,bj)
#endif

#ifndef _dxF
#define  _dxF(i,j,bi,bj) dxF(i,j,bi,bj)
#endif
