C
CBOP
C    !ROUTINE: HFACS_MACROS.h
C    !INTERFACE:
C    include HFACS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | HFACS_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP

#ifdef HFACS_CONST
#define  _hFacS(i,j,k,bi,bj) hFacS(1,1,1,1,1)
#endif

#ifdef HFACS_FX
#define  _hFacS(i,j,k,bi,bj) hFacS(i,1,1,bi,1)
#endif

#ifdef HFACS_FY
#define  _hFacS(i,j,k,bi,bj) hFacS(1,j,1,1,bj)
#endif

#ifdef HFACS_FXY
#define  _hFacS(i,j,k,bi,bj) hFacS(i,j,1,bi,bj)
#endif

#ifdef ALLOW_DEPTH_CONTROL
# define _hFacS(i,j,k,bi,bj) hFacS(i,j,k,bi,bj)*maskS(i,j,k,bi,bj)
#endif

#ifndef _hFacS
#define  _hFacS(i,j,k,bi,bj) hFacS(i,j,k,bi,bj)
#endif
