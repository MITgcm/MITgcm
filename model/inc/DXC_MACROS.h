C $Header: /u/gcmpack/MITgcm/model/inc/DXC_MACROS.h,v 1.4 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: DXC_MACROS.h
C    !INTERFACE:
C    include DXC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXC_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef DXC_CONST
#define  _dxC(i,j,bi,bj) dxC(1,1,1,1)
#endif

#ifdef DXC_FX
#define  _dxC(i,j,bi,bj) dxC(i,1,bi,1)
#endif

#ifdef DXC_FY
#define  _dxC(i,j,bi,bj) dxC(1,j,1,bj)
#endif

#ifndef _dxC
#define  _dxC(i,j,bi,bj) dxC(i,j,bi,bj)
#endif
