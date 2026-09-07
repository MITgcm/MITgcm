!
!BOP
!    !ROUTINE: HFACC_MACROS.h
!    !INTERFACE:
!    include HFACC_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | HFACC_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef HFACC_CONST
#define  _hFacC(i,j,k,bi,bj) hFacC(1,1,1,1,1)
#endif

#ifdef HFACC_FX
#define  _hFacC(i,j,k,bi,bj) hFacC(i,1,1,bi,1)
#endif

#ifdef HFACC_FY
#define  _hFacC(i,j,k,bi,bj) hFacC(1,j,1,1,bj)
#endif

#ifdef HFACC_FXY
#define  _hFacC(i,j,k,bi,bj) hFacC(i,j,1,bi,bj)
#endif

#ifdef ALLOW_DEPTH_CONTROL
# define _hFacC(i,j,k,bi,bj) hFacC(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
#endif

#ifndef _hFacC
#define  _hFacC(i,j,k,bi,bj) hFacC(i,j,k,bi,bj)
#endif
