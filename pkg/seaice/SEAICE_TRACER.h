C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE_TRACER.h,v 1.6 2012/02/03 13:34:31 gforget Exp $
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
C \ev
CEOP

#ifdef ALLOW_SITRACER
      COMMON /SEAICE_TRACER_R/
     &        SItracer, SItrBucket, SItrHEFF, SItrAREA
      _RL SItracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,SItrMaxNum)
      _RL SItrBucket (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,SItrMaxNum)
      _RL SItrHEFF (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,5)
      _RL SItrAREA (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,3)

C     SItrFile        - File containing initial sea ice tracer
C     SItrName        - tracer name ('salinity', 'age', 'one', etc.)
C     SItrNameLong    - tracer long name
C     SItrUnit        - tracer unit ('psu','s','kg/kg', etc.)
C     SItrMate        - variable to which the tracer is associated ('HEFF','AREA',etc.)
      CHARACTER*(MAX_LEN_FNAM) SItrFile(SItrMaxNum)
      CHARACTER*(MAX_LEN_FNAM) SItrName(SItrMaxNum)
      CHARACTER*(MAX_LEN_FNAM) SItrNameLong(SItrMaxNum)
      CHARACTER*(MAX_LEN_FNAM) SItrUnit(SItrMaxNum)
      CHARACTER*(4) SItrMate(SItrMaxNum)
      COMMON /SEAICE_SITR_C/ SItrFile, SItrName, SItrMate,
     &  SItrNameLong, SItrUnit
#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
