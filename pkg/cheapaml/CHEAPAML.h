C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/CHEAPAML.h,v 1.1 2008/08/05 21:49:30 jmc Exp $
C $Name:  $

C #ifdef ALLOW_CHEAPAML
cswdblk
c     !ROUTINE: CHEAPAML.h
c -------------------------------
c   CHEAPAML.h
C   Parameters for cheap atmos mixed layer model
c -------------------------------
C     Tr :: Relaxation temperature profile for lateral boundary region
c     qr :: Relaxation specific humidity profile for lateral boundary region
c     Tair :: atmosphere boundary layer temperature
c     gTairm :: atmosphere temperature tendency
c     qair :: atmosphere specific humidity
c     uwind :: zonal wind
c     vwind :: meridional wind
c     solar :: short wave insolation
c
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
C #endif /* ALLOW_CHEAPAML */
