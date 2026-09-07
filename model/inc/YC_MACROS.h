!
!BOP
!    !ROUTINE: YC_MACROS.h
!    !INTERFACE:
!    include YC_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | YC_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef YC_CONST
#define  _yC(i,j,bi,bj) yC(1,1,1,1)
#endif

#ifdef YC_FX
#define  _yC(i,j,bi,bj) yC(i,1,bi,1)
#endif

#ifdef YC_FY
#define  _yC(i,j,bi,bj) yC(1,j,1,bj)
#endif

#ifndef _yC
#define  _yC(i,j,bi,bj) yC(i,j,bi,bj)
#endif
