C $Header: /u/gcmpack/MITgcm/model/inc/RECIP_DXG_MACROS.h,v 1.1 1998/08/15 16:55:48 cnh Exp $
C
C     /==========================================================\
C     | RECIP_DXG_MACROS.h                                       |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef RECIP_DXG_CONST
#define  _recip_dxG(i,j,bi,bj) recip_dxG(1,1,1,1)
#endif

#ifdef RECIP_DXG_FX
#define  _recip_dxG(i,j,bi,bj) recip_dxG(i,1,bi,1)
#endif

#ifdef RECIP_DXG_FY
#define  _recip_dxG(i,j,bi,bj) recip_dxG(1,j,1,bj)
#endif

#ifndef _recip_dxG
#define  _recip_dxG(i,j,bi,bj) recip_dxG(i,j,bi,bj)
#endif
