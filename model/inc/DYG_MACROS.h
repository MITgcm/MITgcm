!
!BOP
!    !ROUTINE: DYG_MACROS.h
!    !INTERFACE:
!    include DYG_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | DYG_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef DYG_CONST
#define  _dyG(i,j,bi,bj) dyG(1,1,1,1)
#endif

#ifdef DYG_FX
#define  _dyG(i,j,bi,bj) dyG(i,1,bi,1)
#endif

#ifdef DYG_FY
#define  _dyG(i,j,bi,bj) dyG(1,j,1,bj)
#endif

#ifndef _dyG
#define  _dyG(i,j,bi,bj) dyG(i,j,bi,bj)
#endif
