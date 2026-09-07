!
!BOP
!    !ROUTINE: RECIP_DXG_MACROS.h
!    !INTERFACE:
!    include RECIP_DXG_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_DXG_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

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
