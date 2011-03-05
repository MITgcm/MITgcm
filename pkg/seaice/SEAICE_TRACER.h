C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE_TRACER.h,v 1.1 2011/03/05 18:06:49 heimbach Exp $
C $Name:  $

CBOP
C !ROUTINE: SEAICE_TRACER.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SEAICE_TRACER.h                                          |
C     | o Begin header for sea ice tracers                       |
C     \==========================================================/
C
C     IceAgeTr(1) - effective sea ice age
C             at center of grid, i.e., tracer point
C             ==> for non-zero AREA, units of ICEAGEAREA are seconds
C                 and actual ice age is ICEAGEAREA / AREA seconds
C     IceAgeTr(2) - effective sea ice age
C             at center of grid, i.e., tracer point
C             ==> for non-zero HEFF,
C                 units of ICEAGEVOL are seconds * meters
C                 and actual ice age is ICEAGEVOL / HEFF seconds
C     IceAgeTr(3), IceAgeTr(4) - effective sea ice age
C             t.b.d. (poor-man's multi-category)
C \ev
CEOP

#ifdef SEAICE_AGE
      COMMON/SEAICE_AGE_R/IceAgeTr
      _RL IceAgeTr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,SEAICE_num)
C--
C     IceAgeTrFile    - File containing initial sea ice age
      CHARACTER*(MAX_LEN_FNAM) IceAgeTrFile(SEAICE_num)
      COMMON /SEAICE_AGE_C/ IceAgeTrFile
#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
