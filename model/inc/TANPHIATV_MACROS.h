!
!BOP
!    !ROUTINE: TANPHIATV_MACROS.h
!    !INTERFACE:
!    include TANPHIATV_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | TANPHIATV_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef TANPHIATV_CONST
#define  _tanPhiAtV(i,j,bi,bj) tanPhiAtV(1,1,1,1)
#endif

#ifdef TANPHIATV_FX
#define  _tanPhiAtV(i,j,bi,bj) tanPhiAtV(i,1,bi,1)
#endif

#ifdef TANPHIATV_FY
#define  _tanPhiAtV(i,j,bi,bj) tanPhiAtV(1,j,1,bj)
#endif

#ifndef _tanPhiAtV
#define  _tanPhiAtV(i,j,bi,bj) tanPhiAtV(i,j,bi,bj)
#endif
