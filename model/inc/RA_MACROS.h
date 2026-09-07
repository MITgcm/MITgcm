!
!BOP
!    !ROUTINE: RA_MACROS.h
!    !INTERFACE:
!    include RA_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RA_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RA_CONST
#define  _rA(i,j,bi,bj) rA(1,1,1,1)
#endif

#ifdef RA_FX
#define  _rA(i,j,bi,bj) rA(i,1,bi,1)
#endif

#ifdef RA_FY
#define  _rA(i,j,bi,bj) rA(1,j,1,bj)
#endif

#ifndef _rA
#define  _rA(i,j,bi,bj) rA(i,j,bi,bj)
#endif
