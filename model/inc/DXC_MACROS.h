C $Header: /u/gcmpack/MITgcm/model/inc/DXC_MACROS.h,v 1.1 1998/05/26 21:29:44 cnh Exp $
C
C     /==========================================================\
C     | DXC_MACROS.h                                             |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef DXC_CONST
#define  _dxC(i,j,bi,bj) dxC(1,1,1,1)
#endif

#ifdef DXC_FX_ONLY
#define  _dxC(i,j,bi,bj) dxC(i,1,bi,1)
#endif

#ifdef DXC_FY_ONLY
#define  _dxC(i,j,bi,bj) dxC(1,j,1,bj)
#endif

#ifndef _dxC
#define  _dxC(i,j,bi,bj) dxC(i,j,bi,bj)
#endif
