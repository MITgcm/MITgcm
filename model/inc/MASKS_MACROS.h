!
!BOP
!    !ROUTINE: MASKS_MACROS.h
!    !INTERFACE:
!    include MASKS_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | MASKS_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef MASKS_CONST
#define  _maskS(i,j,k,bi,bj) maskS(1,1,1,1,1)
#endif

#ifdef MASKS_FX
#define  _maskS(i,j,k,bi,bj) maskS(i,1,1,bi,1)
#endif

#ifdef MASKS_FY
#define  _maskS(i,j,k,bi,bj) maskS(1,j,1,1,bj)
#endif

#ifdef MASKS_FXY
#define  _maskS(i,j,k,bi,bj) maskS(i,j,1,bi,bj)
#endif

#ifndef _maskS
#define  _maskS(i,j,k,bi,bj) maskS(i,j,k,bi,bj)
#endif
