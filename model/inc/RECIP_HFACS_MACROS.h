!
!BOP
!    !ROUTINE: RECIP_HFACS_MACROS.h
!    !INTERFACE:
!    include RECIP_HFACS_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_HFACS_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RECIP_HFACS_CONST
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(1,1,1,1,1)
#endif

#ifdef RECIP_HFACS_FX
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(i,1,1,bi,1)
#endif

#ifdef RECIP_HFACS_FY
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(1,j,1,1,bj)
#endif

#ifdef RECIP_HFACS_FXY
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(i,j,1,bi,bj)
#endif

#ifdef ALLOW_DEPTH_CONTROL
# define _recip_hFacS(i,j,k,bi,bj) recip_hFacS(i,j,k,bi,bj)*maskS(i,j,k,bi,bj)
#endif

#ifndef _recip_hFacS
#define  _recip_hFacS(i,j,k,bi,bj) recip_hFacS(i,j,k,bi,bj)
#endif
