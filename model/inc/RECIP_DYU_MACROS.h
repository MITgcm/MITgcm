!
!BOP
!    !ROUTINE: RECIP_DYU_MACROS.h
!    !INTERFACE:
!    include RECIP_DYU_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_DYU_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RECIP_DYU_CONST
#define  _recip_dyU(i,j,bi,bj) recip_dyU(1,1,1,1)
#endif

#ifdef RECIP_DYU_FX
#define  _recip_dyU(i,j,bi,bj) recip_dyU(i,1,bi,1)
#endif

#ifdef RECIP_DYU_FY
#define  _recip_dyU(i,j,bi,bj) recip_dyU(1,j,1,bj)
#endif

#ifndef _recip_dyU
#define  _recip_dyU(i,j,bi,bj) recip_dyU(i,j,bi,bj)
#endif
