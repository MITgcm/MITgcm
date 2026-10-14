!
!BOP
!    !ROUTINE: FCORI_MACROS.h
!    !INTERFACE:
!    include FCORI_MACROS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | FCORI_MACROS.h
! *==========================================================*
! | These macros are used to reduce memory requirement and/or
! | memory references when variables are fixed along a given
! | axis or axes.
! *==========================================================*
! \ev
!EOP

#ifdef FCORI_CONST
#define  _fCori(i,j,bi,bj) fCori(1,1,1,1)
#define  _fCoriG(i,j,bi,bj) fCoriG(1,1,1,1)
#endif

#ifdef FCORI_FX
#define  _fCori(i,j,bi,bj) fCori(i,1,bi,1)
#define  _fCoriG(i,j,bi,bj) fCoriG(i,1,bi,1)
#endif

#ifdef FCORI_FY
#define  _fCori(i,j,bi,bj) fCori(1,j,1,bj)
#define  _fCoriG(i,j,bi,bj) fCoriG(1,j,1,bj)
#endif

#ifndef _fCori
#define  _fCori(i,j,bi,bj) fCori(i,j,bi,bj)
#define  _fCoriG(i,j,bi,bj) fCoriG(i,j,bi,bj)
#endif
