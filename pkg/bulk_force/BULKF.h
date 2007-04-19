C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/BULKF.h,v 1.7 2007/04/19 21:26:49 jmc Exp $
C $Name:  $

#ifdef ALLOW_BULK_FORCE
C     !ROUTINE: BULKF.h
C -------------------------------
C   BULKF.h
C  variable for forcing using bulk formula
C -------------------------------
C   FORCING FIELD VARIABLES
C- Mandatory:
C  tair      :: air temperature (K)
C  qair      :: specific humidity at surface (kg/kg)
C  rain      :: total precipitation (= rain + snow) (m/s), (>0: rain)
C  solar     :: downward shortwave radiation (W/m^2), (>0: downward)
C  flwdwn    :: downward longwave radiation  (W/m^2), (>0: downward)
C  wspeed    :: wind speed (m/s)
C- Optional:
C  uwind     :: zonal wind speed (m/s),      at cell center (A-grid)
C  vwind     :: meridional wind speed (m/s), at cell center (A-grid)
C  runoff    :: freshwater runoff (m/s)
C  Qnetch    :: net heat flux (cheating)
C  EmPch     :: E-P (cheating)
C  cloud     :: fraction sky covered in cloud
C  thAir     :: Air potential temp. in the BL [K]
C                (used in AIM-formula stability function)

      COMMON /BULKF_FFIELDS/
     &       Tair, Qair, Rain, Solar, flwdwn,
     &       wspeed, uwind, vwind, runoff,
     &       Qnetch, EmPch, cloud
#ifdef ALLOW_FORMULA_AIM
     &     , thAir
#endif

      _RL  Tair  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Qair  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Rain  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Solar (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL flwdwn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wspeed (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uwind (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vwind (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL runoff (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Qnetch (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL EmPch  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cloud  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_FORMULA_AIM
      _RL thAir  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#endif /* ALLOW_BULK_FORCE */
