!
!BOP
!    !ROUTINE: MASKW_MACROS.h
!    !INTERFACE:
!    include MASKW_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | MASKW_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef MASKW_CONST
#define  _maskW(i,j,k,bi,bj) maskW(1,1,1,1,1)
#endif

#ifdef MASKW_FX
#define  _maskW(i,j,k,bi,bj) maskW(i,1,1,bi,1)
#endif

#ifdef MASKW_FY
#define  _maskW(i,j,k,bi,bj) maskW(1,j,1,1,bj)
#endif

#ifdef MASKW_FXY
#define  _maskW(i,j,k,bi,bj) maskW(i,j,1,bi,bj)
#endif

#ifndef _maskW
#define  _maskW(i,j,k,bi,bj) maskW(i,j,k,bi,bj)
#endif
