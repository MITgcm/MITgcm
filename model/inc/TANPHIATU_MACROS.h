!
!BOP
!    !ROUTINE: TANPHIATU_MACROS.h
!    !INTERFACE:
!    include TANPHIATU_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | TANPHIATU_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef TANPHIATU_CONST
#define  _tanPhiAtU(i,j,bi,bj) tanPhiAtU(1,1,1,1)
#endif

#ifdef TANPHIATU_FX
#define  _tanPhiAtU(i,j,bi,bj) tanPhiAtU(i,1,bi,1)
#endif

#ifdef TANPHIATU_FY
#define  _tanPhiAtU(i,j,bi,bj) tanPhiAtU(1,j,1,bj)
#endif

#ifndef _tanPhiAtU
#define  _tanPhiAtU(i,j,bi,bj) tanPhiAtU(i,j,bi,bj)
#endif
