!
!BOP
!    !ROUTINE: RECIP_HFACW_MACROS.h
!    !INTERFACE:
!    include RECIP_HFACW_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_HFACW_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RECIP_HFACW_CONST
#define  _recip_hFacW(i,j,k,bi,bj) recip_hFacW(1,1,1,1,1)
#endif

#ifdef RECIP_HFACW_FX
#define  _recip_hFacW(i,j,k,bi,bj) recip_hFacW(i,1,1,bi,1)
#endif

#ifdef RECIP_HFACW_FY
#define  _recip_hFacW(i,j,k,bi,bj) recip_hFacW(1,j,1,1,bj)
#endif

#ifdef RECIP_HFACW_FXY
#define  _recip_hFacW(i,j,k,bi,bj) recip_hFacW(i,j,1,bi,bj)
#endif

#ifdef ALLOW_DEPTH_CONTROL
# define _recip_hFacW(i,j,k,bi,bj) recip_hFacW(i,j,k,bi,bj)*maskW(i,j,k,bi,bj)
#endif

#ifndef _recip_hFacW
#define  _recip_hFacW(i,j,k,bi,bj) recip_hFacW(i,j,k,bi,bj)
#endif
