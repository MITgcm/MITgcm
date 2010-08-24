C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/CHEAPAML.h,v 1.2 2010/08/24 14:07:52 jmc Exp $
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
C
      COMMON /CHEAPAML_PARMS/
     &       Tr,qr,uwind,vwind,solar,
     &       ustress, vstress,Tair,gTairm,
c     &       ustress, vstress,
     &       cheapaml_h,
c     &       cheapaml_kdiff, cheapaml_taurelax1,
c     &       cheapaml_taurelax2, cheapaml_xf,
c     &       cheapaml_dtc,rhoa,cpair,stefan,
     &       cheapaml_kdiff, 
     &       cheapaml_taurelax2, 
     &       rhoa,cpair,stefan,
     &       dsolms,dsolmn,
     &       xphaseinit,
     &       cdrag_1,cdrag_2,cdrag_3,cheapaml_ntim,
     &       AirTempFile, AirQFile,SolarFile, UWindFile,
     &       VWindFile, TrFile, QrFile, useFreshwaterFlux
     
      _RL    Tr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    qr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Tair  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gTairm  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    qair  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Solar (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL    uwind (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL    vwind (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL    ustress (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL    vstress (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy) 
      _RL    cheapaml_h
      _RL    cheapaml_kdiff, cheapaml_taurelax1
      _RL    cheapaml_taurelax2, cheapaml_xf
      _RL    cheapaml_dtc,rhoa,cpair,stefan
      _RL    cdrag_1, cdrag_2, cdrag_3
      _RL     dsolms,dsolmn
      _RL     xphaseinit
      INTEGER cheapaml_ntim
      CHARACTER*(MAX_LEN_FNAM) AirTempFile
      CHARACTER*(MAX_LEN_FNAM) AirQFile
      CHARACTER*(MAX_LEN_FNAM) SolarFile
      CHARACTER*(MAX_LEN_FNAM) UWindFile
      CHARACTER*(MAX_LEN_FNAM) VWindFile
      CHARACTER*(MAX_LEN_FNAM) TRFile 
      CHARACTER*(MAX_LEN_FNAM) QrFile 
      LOGICAL useFreshwaterFlux
c #endif /* ALLOW_CHEAPAML */
