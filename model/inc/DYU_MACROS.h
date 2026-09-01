!
!BOP
!    !ROUTINE: DYU_MACROS.h
!    !INTERFACE:
!    include DYU_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | DYU_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef DYU_CONST
#define  _dyU(i,j,bi,bj) dyU(1,1,1,1)
#endif

#ifdef DYU_FX
#define  _dyU(i,j,bi,bj) dyU(i,1,bi,1)
#endif

#ifdef DYU_FY
#define  _dyU(i,j,bi,bj) dyU(1,j,1,bj)
#endif

#ifndef _dyU
#define  _dyU(i,j,bi,bj) dyU(i,j,bi,bj)
#endif
