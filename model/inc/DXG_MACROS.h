C $Header: /u/gcmpack/MITgcm/model/inc/DXG_MACROS.h,v 1.1 1998/05/26 21:29:44 cnh Exp $
C
C     /==========================================================\
C     | DXG_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef DXG_CONST
#define  _dxG(i,j,bi,bj) dxG(1,1,1,1)
#endif

#ifdef DXG_FX_ONLY
#define  _dxG(i,j,bi,bj) dxG(i,1,bi,1)
#endif

#ifdef DXG_FY_ONLY
#define  _dxG(i,j,bi,bj) dxG(1,j,1,bj)
#endif

#ifndef _dxG
#define  _dxG(i,j,bi,bj) dxG(i,j,bi,bj)
#endif
