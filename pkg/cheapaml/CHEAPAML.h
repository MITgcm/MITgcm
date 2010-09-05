C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/CHEAPAML.h,v 1.3 2010/09/05 03:56:18 jmc Exp $
C $Name:  $

c #ifdef ALLOW_CHEAPAML
C     !ROUTINE: CHEAPAML.h
C -------------------------------
C   CHEAPAML.h
C   Parameters for cheap atmos mixed layer model
C -------------------------------
C     Tr :: Relaxation temperature profile for lateral boundary region
C     qr :: Relaxation specific humidity profile for lateral boundary region
C     Tair :: atmosphere boundary layer temperature
C     gTairm :: atmosphere temperature tendency
C     qair :: atmosphere specific humidity
C     uwind :: zonal wind
C     vwind :: meridional wind
C     solar :: short wave insolation

      COMMON /CHEAPAML_VARS/
     &       Tr, qr,
     &       Tair, gTairm,
c    &       qair,
     &       uwind, vwind, solar,
     &       ustress, vstress

      _RL    Tr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    qr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Tair  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gTairm  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL    qair  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    uwind (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL    vwind (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL    Solar (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL    ustress (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL    vstress (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      COMMON /CHEAPAML_PARMS_R/
     &       cheapaml_h,
     &       cheapaml_kdiff,
c    &       cheapaml_taurelax1,
     &       cheapaml_taurelax2,
c    &       cheapaml_xf,
c    &       cheapaml_dtc,
     &       rhoa,cpair,stefan,
     &       dsolms,dsolmn,
     &       xphaseinit,
     &       cdrag_1,cdrag_2,cdrag_3
      _RL    cheapaml_h
      _RL    cheapaml_kdiff
c     _RL    cheapaml_taurelax1
      _RL    cheapaml_taurelax2
c     _RL    cheapaml_xf
c     _RL    cheapaml_dtc
      _RL    rhoa, cpair, stefan
      _RL    cdrag_1, cdrag_2, cdrag_3
      _RL     dsolms,dsolmn
      _RL     xphaseinit

      COMMON /CHEAPAML_PARMS_I/
     &       cheapaml_ntim
      INTEGER cheapaml_ntim

      COMMON /CHEAPAML_PARMS_L/
     &       useFreshwaterFlux
      LOGICAL useFreshwaterFlux

      COMMON /CHEAPAML_PARMS_C/
     &       AirTempFile, AirQFile,SolarFile, UWindFile,
     &       VWindFile, TrFile, QrFile
      CHARACTER*(MAX_LEN_FNAM) AirTempFile
      CHARACTER*(MAX_LEN_FNAM) AirQFile
      CHARACTER*(MAX_LEN_FNAM) SolarFile
      CHARACTER*(MAX_LEN_FNAM) UWindFile
      CHARACTER*(MAX_LEN_FNAM) VWindFile
      CHARACTER*(MAX_LEN_FNAM) TRFile
      CHARACTER*(MAX_LEN_FNAM) QrFile
c #endif /* ALLOW_CHEAPAML */
