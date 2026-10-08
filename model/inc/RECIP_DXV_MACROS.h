!
!BOP
!    !ROUTINE: RECIP_DXV_MACROS.h
!    !INTERFACE:
!    include RECIP_DXV_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_DXV_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RECIP_DXV_CONST
#define  _recip_dxV(i,j,bi,bj) recip_dxV(1,1,1,1)
#endif

#ifdef RECIP_DXV_FX
#define  _recip_dxV(i,j,bi,bj) recip_dxV(i,1,bi,1)
#endif

#ifdef RECIP_DXV_FY
#define  _recip_dxV(i,j,bi,bj) recip_dxV(1,j,1,bj)
#endif

#ifndef _recip_dxV
#define  _recip_dxV(i,j,bi,bj) recip_dxV(i,j,bi,bj)
#endif
