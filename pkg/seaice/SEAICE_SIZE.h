C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE_SIZE.h,v 1.3 2011/10/30 19:12:48 heimbach Exp $
C $Name:  $

#ifdef ALLOW_SEAICE

CBOP
C    !ROUTINE: SEAICE_SIZE.h
C    !INTERFACE:
C #include SEAICE_SIZE.h
 
C    !DESCRIPTION:
C Contains seaice tracer array size (number of tracers).

C SEAICE_num defines how many passive tracers are allocated/exist.
C  and is set here (default 4)
C
C     Number of tracers
      INTEGER SItrMaxNum
      PARAMETER(SItrMaxNum = 3 )
      INTEGER SEAICE_num
      PARAMETER(SEAICE_num = 4 )

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER iicekey
      INTEGER nEVPstepMax
      PARAMETER ( nEVPstepMax=180 )
      INTEGER NMAX_TICE
      PARAMETER ( NMAX_TICE=10 )
      INTEGER SOLV_MAX_FIXED
      PARAMETER ( SOLV_MAX_FIXED=500 )
#endif

CEOP
#endif /* ALLOW_SEAICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
