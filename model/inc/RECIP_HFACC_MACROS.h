!
!BOP
!    !ROUTINE: RECIP_HFACC_MACROS.h
!    !INTERFACE:
!    include RECIP_HFACC_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_HFACC_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RECIP_HFACC_CONST
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(1,1,1,1,1)
#endif

#ifdef RECIP_HFACC_FX
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(i,1,1,bi,1)
#endif

#ifdef RECIP_HFACC_FY
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(1,j,1,1,bj)
#endif

#ifdef RECIP_HFACC_FXY
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(i,j,1,bi,bj)
#endif

#ifdef ALLOW_DEPTH_CONTROL
# define _recip_hFacC(i,j,k,bi,bj) recip_hFacC(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
#endif

#ifndef _recip_hFacC
#define  _recip_hFacC(i,j,k,bi,bj) recip_hFacC(i,j,k,bi,bj)
#endif
