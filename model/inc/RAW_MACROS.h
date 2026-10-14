!
!BOP
!    !ROUTINE: RAW_MACROS.h
!    !INTERFACE:
!    include RAW_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RAW_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RA_CONST
#define  _rAw(i,j,bi,bj) rAw(1,1,1,1)
#endif

#ifdef RA_FX
#define  _rAw(i,j,bi,bj) rAw(i,1,bi,1)
#endif

#ifdef RA_FY
#define  _rAw(i,j,bi,bj) rAw(1,j,1,bj)
#endif

#ifndef _rAw
#define  _rAw(i,j,bi,bj) rAw(i,j,bi,bj)
#endif
