CBOP
C !ROUTINE: SEAICE_TRACER.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SEAICE_TRACER.h                                          |
C     | o Begin header for sea ice tracers                       |
C     \==========================================================/
C
C \ev
CEOP

#ifdef ALLOW_SITRACER

C     SItrNumInUse - Number of tracers that are in use (must be less that SItrMaxNum)
      INTEGER SItrNumInUse
      COMMON /SEAICE_TRACER_I/ SItrNumInUse

C     SItracer   - generic ice tracer array
C     SItrBucket - collected SItracer to be later passed to the ocean
C     SItrHEFF   - history of HEFF evolution during seaice_growth
C     SItrAREA   - history of AREA evolution during seaice_growth
      _RL SItracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,SItrMaxNum)
      _RL SItrBucket (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,SItrMaxNum)
      _RL SItrHEFF (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,5)
      _RL SItrAREA (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,3)
      COMMON /SEAICE_TRACER_R/
     &        SItracer, SItrBucket, SItrHEFF, SItrAREA

C     SItrFromOcean0    - for new ice from ocean freeze - constant tracer value
C     SItrFromOceanFrac - for new ice from ocean freeze - fraction of ocean tracer
C     SItrFromFlood0    - for new ice from snow flood   - constant tracer value
C     SItrFromFloodFrac - for new ice from snow flood   - fraction of ocean tracer
C     SItrExpand0       - for ice cover thermo. exans.  - constant tracer value
      _RL SItrFromOcean0(SItrMaxNum), SItrFromOceanFrac(SItrMaxNum),
     &    SItrFromFlood0(SItrMaxNum), SItrFromFloodFrac(SItrMaxNum),
     &    SItrExpand0(SItrMaxNum)
      COMMON /SEAICE_TRACER_CONST_R/
     &    SItrFromOcean0, SItrFromOceanFrac, SItrFromFlood0,
     &    SItrFromFloodFrac, SItrExpand0

C     SItrFile        - File containing initial sea ice tracer
C     SItrName        - tracer name ('salinity', 'age', 'one', etc.)
C     SItrNameLong    - tracer long name
C     SItrUnit        - tracer unit ('g/kg','s','kg/kg', etc.)
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
