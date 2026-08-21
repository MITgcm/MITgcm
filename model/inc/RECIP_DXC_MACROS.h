!
!BOP
!    !ROUTINE: RECIP_DXC_MACROS.h
!    !INTERFACE:
!    include RECIP_DXC_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_DXC_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RECIP_DXC_CONST
#define  _recip_dxC(i,j,bi,bj) recip_dxC(1,1,1,1)
#endif

#ifdef RECIP_DXC_FX
#define  _recip_dxC(i,j,bi,bj) recip_dxC(i,1,bi,1)
#endif

#ifdef RECIP_DXC_FY
#define  _recip_dxC(i,j,bi,bj) recip_dxC(1,j,1,bj)
#endif

#ifndef _recip_dxC
#define  _recip_dxC(i,j,bi,bj) recip_dxC(i,j,bi,bj)
#endif
