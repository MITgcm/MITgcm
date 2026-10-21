!
!BOP
!    !ROUTINE: DYF_MACROS.h
!    !INTERFACE:
!    include DYF_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | DYF_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef DYF_CONST
#define  _dyF(i,j,bi,bj) dyF(1,1,1,1)
#endif

#ifdef DYF_FX
#define  _dyF(i,j,bi,bj) dyF(i,1,bi,1)
#endif

#ifdef DYF_FY
#define  _dyF(i,j,bi,bj) dyF(1,j,1,bj)
#endif

#ifndef _dyF
#define  _dyF(i,j,bi,bj) dyF(i,j,bi,bj)
#endif
