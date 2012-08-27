C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/CHEAPAML.h,v 1.8 2012/08/27 17:05:49 wienders Exp $
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
C     gqairm :: atmosphere moisture tendency
C     uwind :: zonal wind
C     vwind :: meridional wind
C     solar :: short wave insolation
C     Cheapmask :: open boundary condition relaxation mask
C     Cheaptracer :: passive tracer
C     CheaptracerR :: Relaxation profile for passive tracer
C     gCheaptracerm :: passive tracer tendency

      COMMON /CHEAPAML_VARS/
     &       Tr, qr,
     &       Tair, gTairm,
     &       qair, gqairm,
     &       uwind, vwind, solar,
     &       wwind,
     &       ustress, vstress,
     &       wavesh, wavesp, Cheapmask, CheapHgrid,
     &       Cheapclouds,Cheapdlongwave,
     &       Cheaptracer, CheaptracerR, gCheaptracerm,
     &       Cheapprgrid,xgs,xrelf


      _RL    Tr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    qr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Tair   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gTairm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    qair   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gqairm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    uwind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    vwind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    wwind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Solar  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    ustress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    vstress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    wavesh (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    wavesp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheapmask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    xgs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    xrelf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    CheapHgrid(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheapprgrid(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheapclouds(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheapdlongwave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheaptracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    CheaptracerR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gCheaptracerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)


      COMMON /CHEAPAML_PARMS_R/
     &       cheapaml_h,
     &       cheapaml_kdiff,
     &       cheapaml_taurelax,
     &       cheapaml_taurelaxocean,
     &       rhoa, cpair, stefan,
     &       lath, xkar, gasR,
     &       dsolms, dsolmn,
     &       xphaseinit, gamma_blk, humid_fac, p0,
     &       xef, hm,
     &       zu, zt, zq,
     &       cdrag_1, cdrag_2, cdrag_3,
     &       externForcingPeriod_cheap,
     &       externForcingCycle_cheap,
     &       cheap_pr1,cheap_pr2
      _RL    cheapaml_h
      _RL    cheapaml_kdiff
      _RL    cheapaml_taurelax
      _RL    cheapaml_taurelaxocean
      _RL    rhoa, cpair, stefan
      _RL    lath, xkar, gasR
      _RL    dsolms, dsolmn
      _RL    xphaseinit, gamma_blk, humid_fac, p0
      _RL    xef, hm
      _RL    zu, zt, zq
      _RL    cdrag_1, cdrag_2, cdrag_3
      _RL    externForcingPeriod_cheap
      _RL    externForcingCycle_cheap     
      _RL    cheap_pr1,cheap_pr2

      COMMON /CHEAPAML_PARMS_I/
     &       cheapaml_ntim,
     &       cheapaml_mask_width
      INTEGER cheapaml_ntim
      INTEGER cheapaml_mask_width

      COMMON /CHEAPAML_PARMS_L/
     &       useFreshwaterFlux,
     &       useFluxLimit,
     &       useStressOption,
     &       useRelativeHumidity,
     &       periodicExternalForcing_cheap,
     &       useCheapTracer,
     &       usetimevarblh,
     &       useclouds,
     &       usedlongwave
      LOGICAL useFreshwaterFlux
      LOGICAL useFluxLimit
      LOGICAL useStressOption
      LOGICAL useRelativeHumidity
      LOGICAL periodicExternalForcing_cheap
      LOGICAL useCheapTracer
      LOGICAL usetimevarblh
      LOGICAL useclouds
      LOGICAL usedlongwave

      COMMON /CHEAPAML_PARMS_C/
     &       AirTempFile, AirQFile, SolarFile,
     &       UWindFile, VWindFile, UStressFile, VStressFile,
     &       TrFile, QrFile,
     &       WaveHFile, WavePFile, FluxFormula, WaveModel,
     &       TracerFile, TracerRFile, CheapMaskFile, Cheap_hFile,
     &       Cheap_prfile, cheap_clfile,cheap_dlwFile

      CHARACTER*(MAX_LEN_FNAM) AirTempFile
      CHARACTER*(MAX_LEN_FNAM) AirQFile
      CHARACTER*(MAX_LEN_FNAM) SolarFile
      CHARACTER*(MAX_LEN_FNAM) UWindFile
      CHARACTER*(MAX_LEN_FNAM) VWindFile
      CHARACTER*(MAX_LEN_FNAM) UStressFile
      CHARACTER*(MAX_LEN_FNAM) VStressFile
      CHARACTER*(MAX_LEN_FNAM) TrFile
      CHARACTER*(MAX_LEN_FNAM) QrFile
      CHARACTER*(MAX_LEN_FNAM) WaveHFile
      CHARACTER*(MAX_LEN_FNAM) WavePFile
      CHARACTER*(MAX_LEN_FNAM) FluxFormula
      CHARACTER*(MAX_LEN_FNAM) WaveModel
      CHARACTER*(MAX_LEN_FNAM) TracerFile
      CHARACTER*(MAX_LEN_FNAM) TracerRFile
      CHARACTER*(MAX_LEN_FNAM) CheapMaskFile
      CHARACTER*(MAX_LEN_FNAM) Cheap_hFile
      CHARACTER*(MAX_LEN_FNAM) Cheap_prFile
      CHARACTER*(MAX_LEN_FNAM) Cheap_clFile
      CHARACTER*(MAX_LEN_FNAM) cheap_dlwFile

c #endif /* ALLOW_CHEAPAML */
