!
!BOP
!    !ROUTINE: DXF_MACROS.h
!    !INTERFACE:
!    include DXF_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | DXF_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef DXF_CONST
#define  _dxF(i,j,bi,bj) dxF(1,1,1,1)
#endif

#ifdef DXF_FX
#define  _dxF(i,j,bi,bj) dxF(i,1,bi,1)
#endif

#ifdef DXF_FY
#define  _dxF(i,j,bi,bj) dxF(1,j,1,bj)
#endif

#ifndef _dxF
#define  _dxF(i,j,bi,bj) dxF(i,j,bi,bj)
#endif
