!
!BOP
!    !ROUTINE: DXG_MACROS.h
!    !INTERFACE:
!    include DXG_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | DXG_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef DXG_CONST
#define  _dxG(i,j,bi,bj) dxG(1,1,1,1)
#endif

#ifdef DXG_FX
#define  _dxG(i,j,bi,bj) dxG(i,1,bi,1)
#endif

#ifdef DXG_FY
#define  _dxG(i,j,bi,bj) dxG(1,j,1,bj)
#endif

#ifndef _dxG
#define  _dxG(i,j,bi,bj) dxG(i,j,bi,bj)
#endif
