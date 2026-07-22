!
!BOP
!    !ROUTINE: XC_MACROS.h
!    !INTERFACE:
!    include XC_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | XC_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef XC_CONST
#define  _xC(i,j,bi,bj) xC(1,1,1,1)
#endif

#ifdef XC_FX
#define  _xC(i,j,bi,bj) xC(i,1,bi,1)
#endif

#ifdef XC_FY
#define  _xC(i,j,bi,bj) xC(1,j,1,bj)
#endif

#ifndef _xC
#define  _xC(i,j,bi,bj) xC(i,j,bi,bj)
#endif
