!
!BOP
!    !ROUTINE: DXC_MACROS.h
!    !INTERFACE:
!    include DXC_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | DXC_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef DXC_CONST
#define  _dxC(i,j,bi,bj) dxC(1,1,1,1)
#endif

#ifdef DXC_FX
#define  _dxC(i,j,bi,bj) dxC(i,1,bi,1)
#endif

#ifdef DXC_FY
#define  _dxC(i,j,bi,bj) dxC(1,j,1,bj)
#endif

#ifndef _dxC
#define  _dxC(i,j,bi,bj) dxC(i,j,bi,bj)
#endif
