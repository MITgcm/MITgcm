C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE_SIZE.h,v 1.5 2012/03/06 16:51:21 jmc Exp $
C $Name:  $

#ifdef ALLOW_SEAICE

CBOP
C    !ROUTINE: SEAICE_SIZE.h
C    !INTERFACE:
C #include SEAICE_SIZE.h

C    !DESCRIPTION:
C Contains seaice array-size definition (number of tracers,categories).

C SItrMaxNum :: number of passive tracers to allocate
C MULTDIM    :: number of seaice categories to allocate
CEOP

C-    Maximum Number of categories
      INTEGER MULTDIM
      PARAMETER (MULTDIM=7)

C-    Maximum Number of tracers
      INTEGER SItrMaxNum
      PARAMETER(SItrMaxNum = 3 )

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER iicekey
      INTEGER nEVPstepMax
      PARAMETER ( nEVPstepMax=180 )
      INTEGER NMAX_TICE
      PARAMETER ( NMAX_TICE=10 )
      INTEGER SOLV_MAX_FIXED
      PARAMETER ( SOLV_MAX_FIXED=500 )
#endif

#endif /* ALLOW_SEAICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
