C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/CHEAPAML.h,v 1.4 2011/02/06 00:18:05 jmc Exp $
C $Name:  $

c #ifdef ALLOW_CHEAPAML
C     !ROUTINE: CHEAPAML.h
C -------------------------------
C   CHEAPAML.h
C   Parameters for cheap atmos mixed layer model
C -------------------------------
C     Tr :: Relaxation temperature profile for lateral boundary region
C     qr :: Relaxation specific humidity profile for lateral boundary region
C     rref ::
C     Tair :: atmosphere boundary layer temperature
C     gTairm :: atmosphere temperature tendency
C     qair :: atmosphere specific humidity
C     gqairm :: atmosphere moisture tendency
C     rair ::
C     uwind :: zonal wind
C     vwind :: meridional wind
C     solar :: short wave insolation
C     Cheapmask :: open boundary condition relaxation mask

      COMMON /CHEAPAML_VARS/
     &       Tr, qr, rref,
     &       Tair, gTairm,
     &       qair, gqairm, rair,
     &       uwind, vwind, solar,
     &       ustress, vstress,
     &       wavesh, wavesp, Cheapmask

      _RL    Tr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    qr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    rref   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Tair   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gTairm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    qair   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gqairm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    rair   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    uwind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    vwind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Solar  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    ustress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    vstress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    wavesh (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    wavesp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheapmask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /CHEAPAML_PARMS_R/
     &       cheapaml_h,
     &       cheapaml_kdiff,
c    &       cheapaml_taurelax1,
     &       cheapaml_taurelax2,
c    &       cheapaml_xf,
c    &       cheapaml_dtc,
     &       rhoa, cpair, stefan,
     &       lath, xkar, gasR,
     &       dsolms, dsolmn,
     &       xphaseinit, gamma_blk, humid_fac, p0,
     &       Celcius2K, xgs, xef, hm,
     &       zu, zt, zq,
     &       cdrag_1, cdrag_2, cdrag_3
      _RL    cheapaml_h
      _RL    cheapaml_kdiff
c     _RL    cheapaml_taurelax1
      _RL    cheapaml_taurelax2
c     _RL    cheapaml_xf
c     _RL    cheapaml_dtc
      _RL    rhoa, cpair, stefan
      _RL    lath, xkar, gasR
      _RL    dsolms, dsolmn
      _RL    xphaseinit, gamma_blk, humid_fac, p0
      _RL    Celcius2K, xgs, xef, hm
      _RL    zu, zt, zq
      _RL    cdrag_1, cdrag_2, cdrag_3

      COMMON /CHEAPAML_PARMS_I/
     &       cheapaml_ntim,
     &       cheapaml_mask_width
      INTEGER cheapaml_ntim
      INTEGER cheapaml_mask_width

      COMMON /CHEAPAML_PARMS_L/
     &       useFreshwaterFlux,
     &       useFluxLimit,
     &       useStressOption,
     &       useRelativeHumidity
      LOGICAL useFreshwaterFlux
      LOGICAL useFluxLimit
      LOGICAL useStressOption
      LOGICAL useRelativeHumidity

      COMMON /CHEAPAML_PARMS_C/
     &       AirTempFile, AirQFile, AirQrelFile, SolarFile,
     &       UWindFile, VWindFile, UStressFile, VStressFile,
     &       TrFile, QrFile, QrrelFile,
     &       WaveHFile, WavePFile, FluxFormula, WaveModel

      CHARACTER*(MAX_LEN_FNAM) AirTempFile
      CHARACTER*(MAX_LEN_FNAM) AirQFile
      CHARACTER*(MAX_LEN_FNAM) AirQrelFile
      CHARACTER*(MAX_LEN_FNAM) SolarFile
      CHARACTER*(MAX_LEN_FNAM) UWindFile
      CHARACTER*(MAX_LEN_FNAM) VWindFile
      CHARACTER*(MAX_LEN_FNAM) UStressFile
      CHARACTER*(MAX_LEN_FNAM) VStressFile
      CHARACTER*(MAX_LEN_FNAM) TrFile
      CHARACTER*(MAX_LEN_FNAM) QrFile
      CHARACTER*(MAX_LEN_FNAM) QrrelFile
      CHARACTER*(MAX_LEN_FNAM) WaveHFile
      CHARACTER*(MAX_LEN_FNAM) WavePFile
      CHARACTER*(MAX_LEN_FNAM) FluxFormula
      CHARACTER*(MAX_LEN_FNAM) WaveModel

c #endif /* ALLOW_CHEAPAML */
