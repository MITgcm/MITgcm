!
!BOP
!    !ROUTINE: RAS_MACROS.h
!    !INTERFACE:
!    include RAS_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RAS_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RA_CONST
#define  _rAs(i,j,bi,bj) rAs(1,1,1,1)
#endif

#ifdef RA_FX
#define  _rAs(i,j,bi,bj) rAs(i,1,bi,1)
#endif

#ifdef RA_FY
#define  _rAs(i,j,bi,bj) rAs(1,j,1,bj)
#endif

#ifndef _rAs
#define  _rAs(i,j,bi,bj) rAs(i,j,bi,bj)
#endif
