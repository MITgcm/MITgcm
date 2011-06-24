C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE_TRACER.h,v 1.5 2011/06/24 01:30:25 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: SEAICE_TRACER.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SEAICE_TRACER.h                                          |
C     | o Begin header for sea ice tracers                       |
C     \==========================================================/
C
C     SItracer   - generic ice tracer array
C     SItrBucket - collected SItracer to be later passed to the ocean
C     SItrHEFF   - history of HEFF evolution during seaice_growth
C     SItrAREA   - history of AREA evolution during seaice_growth
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
C             t.b.d. (poor-man s multi-category)
C \ev
CEOP

#ifdef ALLOW_SITRACER
      COMMON /SEAICE_TRACER_R/
     &        SItracer, SItrBucket, SItrHEFF, SItrAREA
      _RL SItracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,SItrMaxNum)
      _RL SItrBucket (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,SItrMaxNum)
      _RL SItrHEFF (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,5)
      _RL SItrAREA (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,3)
#endif

#ifdef SEAICE_AGE
      COMMON/SEAICE_AGE_R/IceAgeTr
      _RL IceAgeTr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,SEAICE_num)
#endif

#if (defined SEAICE_AGE)||(defined ALLOW_SITRACER)
C     IceAgeTrFile    - File containing initial sea ice age
C     SItrName        - tracer name ('salinity', 'age', 'one', etc.)
C     SItrNameLong    - tracer long name
C     SItrUnit        - tracer unit ('psu','s','kg/kg', etc.)
C     SItrMate        - variable to which the tracer is associated ('HEFF','AREA',etc.)
      CHARACTER*(MAX_LEN_FNAM) IceAgeTrFile(SEAICE_num)
      CHARACTER*(MAX_LEN_FNAM) SItrName(SItrMaxNum)
      CHARACTER*(MAX_LEN_FNAM) SItrNameLong(SItrMaxNum)
      CHARACTER*(MAX_LEN_FNAM) SItrUnit(SItrMaxNum)
      CHARACTER*(4) SItrMate(SItrMaxNum)
      COMMON /SEAICE_AGE_C/ IceAgeTrFile, SItrName, SItrMate,
     &  SItrNameLong, SItrUnit
#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
