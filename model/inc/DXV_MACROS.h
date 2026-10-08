!
!BOP
!    !ROUTINE: DXV_MACROS.h
!    !INTERFACE:
!    include DXV_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | DXV_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef DXV_CONST
#define  _dxV(i,j,bi,bj) dxV(1,1,1,1)
#endif

#ifdef DXV_FX
#define  _dxV(i,j,bi,bj) dxV(i,1,bi,1)
#endif

#ifdef DXV_FY
#define  _dxV(i,j,bi,bj) dxV(1,j,1,bj)
#endif

#ifndef _dxV
#define  _dxV(i,j,bi,bj) dxV(i,j,bi,bj)
#endif
