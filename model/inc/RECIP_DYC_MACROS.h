!
!BOP
!    !ROUTINE: RECIP_DYC_MACROS.h
!    !INTERFACE:
!    include RECIP_DYC_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_DYC_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RECIP_DYC_CONST
#define  _recip_dyC(i,j,bi,bj) recip_dyC(1,1,1,1)
#endif

#ifdef RECIP_DYC_FX
#define  _recip_dyC(i,j,bi,bj) recip_dyC(i,1,bi,1)
#endif

#ifdef RECIP_DYC_FY
#define  _recip_dyC(i,j,bi,bj) recip_dyC(1,j,1,bj)
#endif

#ifndef _recip_dyC
#define  _recip_dyC(i,j,bi,bj) recip_dyC(i,j,bi,bj)
#endif
