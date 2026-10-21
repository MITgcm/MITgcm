!
!BOP
!    !ROUTINE: RECIP_DYF_MACROS.h
!    !INTERFACE:
!    include RECIP_DYF_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | RECIP_DYF_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef RECIP_DYF_CONST
#define  _recip_dyF(i,j,bi,bj) recip_dyF(1,1,1,1)
#endif

#ifdef RECIP_DYF_FX
#define  _recip_dyF(i,j,bi,bj) recip_dyF(i,1,bi,1)
#endif

#ifdef RECIP_DYF_FY
#define  _recip_dyF(i,j,bi,bj) recip_dyF(1,j,1,bj)
#endif

#ifndef _recip_dyF
#define  _recip_dyF(i,j,bi,bj) recip_dyF(i,j,bi,bj)
#endif
