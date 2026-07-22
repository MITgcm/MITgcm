!
!BOP
!    !ROUTINE: RECIP_DXF_MACROS.h
!    !INTERFACE:
!    include RECIP_DXF_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_DXF_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RECIP_DXF_CONST
#define  _recip_dxF(i,j,bi,bj) recip_dxF(1,1,1,1)
#endif

#ifdef RECIP_DXF_FX
#define  _recip_dxF(i,j,bi,bj) recip_dxF(i,1,bi,1)
#endif

#ifdef RECIP_DXF_FY
#define  _recip_dxF(i,j,bi,bj) recip_dxF(1,j,1,bj)
#endif

#ifndef _recip_dxF
#define  _recip_dxF(i,j,bi,bj) recip_dxF(i,j,bi,bj)
#endif
