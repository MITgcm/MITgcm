C $Header: /u/gcmpack/MITgcm/model/inc/DXG_MACROS.h,v 1.4 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: DXG_MACROS.h
C    !INTERFACE:
C    include DXG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXG_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef DXG_CONST
#define  _dxG(i,j,bi,bj) dxG(1,1,1,1)
#endif

#ifdef DXG_FX
#define  _dxG(i,j,bi,bj) dxG(i,1,bi,1)
#endif

#ifdef DXG_FY
#define  _dxG(i,j,bi,bj) dxG(1,j,1,bj)
#endif

#ifndef _dxG
#define  _dxG(i,j,bi,bj) dxG(i,j,bi,bj)
#endif
