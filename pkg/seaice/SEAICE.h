C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE.h,v 1.16 2005/08/31 00:05:58 heimbach Exp $
C $Name:  $

CBOP
C !ROUTINE: SEAICE.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SEAICE.h                                                 |
C     | o Basic header for sea ice model.                        |
C     |   Contains most sea ice field declarations.              |
C     \==========================================================/
C
C     UICE  - zonal ice velocity in m/s
C             at South-West B-grid U point
C             >0 from West to East
C     VICE  - meridional ice velocity in m/s
C             at South-West B-grid U point
C             >0 from South to North
C             note: the South-West B-grid U and V points are on
C                the lower, left-hand corner of each grid cell
C     AREA  - fractional ice-covered area in m^2/m^2
C             at South-West B-grid tracer point
C             0 is no cover, 1 is 100% cover
C     HEFF  - effective ice thickness in m
C             at South-West B-grid tracer point
C             note: for non-zero AREA, actual ice
C                thickness is HEFF / AREA
C \ev
CEOP
      COMMON/SEAICE_DYNVARS_1/AREA
      _RL AREA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)

      COMMON/SEAICE_DYNVARS_2/UICE,VICE
      _RL UICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL VICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)

      COMMON/SEAICE_DYNVARS_3/
     &       ETA,ZETA,DRAGS,DRAGA,AMASS,FORCEX,FORCEY,UICEC,VICEC
      _RL ETA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL ZETA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DRAGS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DRAGA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL AMASS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL FORCEX     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL FORCEY     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL UICEC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL VICEC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

      COMMON/SEAICE_TRANS/HEFF,HSNOW
      _RL HEFF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL HSNOW      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

      COMMON/ARRAY/HEFFM,UVM
      COMMON/OFL/YNEG
      COMMON/RIV/RIVER
      COMMON/SALT_WATER/SEAICE_SALT
      _RL HEFFM      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL UVM        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL YNEG       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL RIVER      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL SEAICE_SALT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

#ifdef SEAICE_MULTILEVEL
      INTEGER MULTDIM
      PARAMETER (MULTDIM=7)
      COMMON/MULTILEVEL/TICES
      _RL TICES      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,MULTDIM,nSx,nSy)
#endif

      COMMON/MIX/TMIX,TICE
      COMMON/GWATXY/GWATX,GWATY
      COMMON/WIND/WINDX,WINDY
      COMMON/RATE/FHEFF,FICE,FO,HCORR
      COMMON/QFLUX/QNETO,QNETI,QSWO,QSWI
      _RL TMIX       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL TICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL GWATX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL GWATY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL WINDX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL WINDY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL FHEFF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL FICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL FO         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL HCORR      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL QNETO      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL QNETI      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL QSWO       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL QSWI       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

      COMMON/COUNT/ICOUNT
      COMMON/DAY/IDELT
      INTEGER ICOUNT, IDELT

C--   KGEO    Level used as a proxy for geostrophic velocity.
      COMMON/SEAICE_KGEO/KGEO
      integer KGEO   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
