C $Header: /u/gcmpack/MITgcm/model/inc/TANPHIATU_MACROS.h,v 1.1 1998/06/08 21:43:00 cnh Exp $
C
C     /==========================================================\
C     | TANPHIATU_MACROS.h                                       |
C     |==========================================================|
C     | These macros are used to reduce memory requirement and/or|
C     | memory references when variables are fixed along a given |
C     | axis or axes.                                            |
C     \==========================================================/

#ifdef TANPHIATU_CONST
#define  _tanPhiAtU(i,j,bi,bj) tanPhiAtU(1,1,1,1)
#endif

#ifdef TANPHIATU_FX
#define  _tanPhiAtU(i,j,bi,bj) tanPhiAtU(i,1,bi,1)
#endif

#ifdef TANPHIATU_FY
#define  _tanPhiAtU(i,j,bi,bj) tanPhiAtU(1,j,1,bj)
#endif

#ifndef _tanPhiAtU
#define  _tanPhiAtU(i,j,bi,bj) tanPhiAtU(i,j,bi,bj)
#endif
