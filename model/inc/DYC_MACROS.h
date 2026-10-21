!
!BOP
!    !ROUTINE: DYC_MACROS.h
!    !INTERFACE:
!    include DYC_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | DYC_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef DYC_CONST
#define  _dyC(i,j,bi,bj) dyC(1,1,1,1)
#endif

#ifdef DYC_FX
#define  _dyC(i,j,bi,bj) dyC(i,1,bi,1)
#endif

#ifdef DYC_FY
#define  _dyC(i,j,bi,bj) dyC(1,j,1,bj)
#endif

#ifndef _dyC
#define  _dyC(i,j,bi,bj) dyC(i,j,bi,bj)
#endif
