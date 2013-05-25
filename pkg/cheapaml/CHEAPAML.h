C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/CHEAPAML.h,v 1.12 2013/05/25 18:05:45 jmc Exp $
C $Name:  $

c #ifdef ALLOW_CHEAPAML
C     !ROUTINE: CHEAPAML.h
C -------------------------------
C   CHEAPAML.h
C   Parameters for cheap atmos mixed layer model
C -------------------------------
C     Tr :: Relaxation temperature profile for lateral boundary region [^oC]
C     qr :: Relaxation specific humidity profile for lateral boundary region
C     Tair :: atmosphere boundary layer temperature [^oC]
C     gTairm :: atmosphere temperature tendency
C     qair :: atmosphere specific humidity  [-]
C     gqairm :: atmosphere moisture tendency
C     uWind :: zonal wind component at grid-cell Western  edge (uVel location)
C     vWind :: meridional wind comp at grid-cell Southern edge (vVel location)
C     solar :: short wave insolation (+=dw) [W/m2]
C     ustress :: zonal wind stress component at grid-cell center (A-grid) [N/m2]
C     vstress :: meridional wind stress comp at grid-cell center (A-grid) [N/m2]
C     Cheapmask :: open boundary condition relaxation mask
C     Cheaptracer :: passive tracer
C     CheaptracerR :: Relaxation profile for passive tracer
C     gCheaptracerm :: passive tracer tendency
C     cheapPrecip   :: precipitation (+=dw) [kg/m2/s]

      COMMON /CHEAPAML_VARS/
     &       Tr, qr,
     &       Tair, gTairm,
     &       qair, gqairm,
     &       uWind, vWind, solar,
     &       wWind,
     &       ustress, vstress,
     &       wavesh, wavesp, Cheapmask, CheapHgrid,
     &       Cheapclouds, Cheapdlongwave,
     &       Cheaptracer, CheaptracerR, gCheaptracerm,
c    &       Cheapprgrid,
     &       xgs, xrelf, cheapPrecip

      _RL    Tr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    qr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Tair   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gTairm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    qair   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gqairm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    uWind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    vWind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    wWind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Solar  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    ustress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    vstress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    wavesh (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    wavesp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheapmask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    xgs     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    xrelf   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    cheapPrecip(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    CheapHgrid (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL    Cheapprgrid(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheapclouds(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheapdlongwave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    Cheaptracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    CheaptracerR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    gCheaptracerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     lath      :: latent heat (J/kg)
C     xkar      :: von Karman constant
C     gasR      :: gas constant
C     dsolms    :: Solar variation at Southern boundary
C     dsolmn    :: Solar variation at Northern boundary
C     xphaseinit :: user input initial phase of year relative to mid winter.
C                   e.g. xphaseinit = pi implies time zero is mid summer.
C     gamma_blk :: atmospheric adiabatic lapse rate
C     humid_fac :: humidity factor for computing virtual potential temperature
C     p0        :: surface pressure in mb
C     ssq[0:2]  :: coeff. used to compute saturation specific humidity
C     cheap_pr1 :: precipitation time constant
C     cheap_pr2 :: precipitation time constant
      COMMON /CHEAPAML_PARMS_R/
     &       cheapaml_h,
     &       cheapaml_kdiff,
     &       cheapaml_taurelax,
     &       cheapaml_taurelaxocean,
     &       rhoa, cpair, stefan,
     &       lath, xkar, gasR,
     &       dsolms, dsolmn,
     &       xphaseinit, gamma_blk, humid_fac, p0,
     &       ssq0, ssq1, ssq2,
     &       xef, hm,
     &       zu, zt, zq,
     &       cdrag_1, cdrag_2, cdrag_3,
     &       externForcingPeriod_cheap,
     &       externForcingCycle_cheap,
     &       cheap_pr1, cheap_pr2
      _RL    cheapaml_h
      _RL    cheapaml_kdiff
      _RL    cheapaml_taurelax
      _RL    cheapaml_taurelaxocean
      _RL    rhoa, cpair, stefan
      _RL    lath, xkar, gasR
      _RL    dsolms, dsolmn
      _RL    xphaseinit, gamma_blk, humid_fac, p0
      _RL    ssq0, ssq1, ssq2
      _RL    xef, hm
      _RL    zu, zt, zq
      _RL    cdrag_1, cdrag_2, cdrag_3
      _RL    externForcingPeriod_cheap
      _RL    externForcingCycle_cheap
      _RL    cheap_pr1,cheap_pr2

C    cheap[]StartAB :: Adams-Bashforth restart status for prognostic variable []
      COMMON /CHEAPAML_PARMS_I/
     &       cheapaml_ntim,
     &       cheapaml_mask_width,
     &       cheapTairStartAB, cheapQairStartAB, cheapTracStartAB
      INTEGER cheapaml_ntim
      INTEGER cheapaml_mask_width
      INTEGER cheapTairStartAB, cheapQairStartAB, cheapTracStartAB

C--   COMMON /CHEAPAML_PARMS_L/
C     cheapamlXperiodic :: domain (including land) is periodic in X dir
C     cheapamlYperiodic :: domain (including land) is periodic in Y dir
C     useFreshWaterFlux :: option to include evap+precip  (on  by default)
C     useFluxLimit      :: use flux limiting advection    (off by default)
C     useStressOption   :: use stress option              (off by default)
C     useCheapTracer    :: use passive tracer option      (off by default)
C     useTimeVarBLH     :: use time varying BL height option (off by default)
C     useClouds         :: use clouds option              (off by default)
C     useDLongWave      :: use imported downward longwave (off by default)
      COMMON /CHEAPAML_PARMS_L/
     &       cheapamlXperiodic,
     &       cheapamlYperiodic,
     &       useFreshWaterFlux,
     &       useFluxLimit,
     &       useStressOption,
     &       useRelativeHumidity,
     &       periodicExternalForcing_cheap,
     &       useCheapTracer,
     &       useTimeVarBLH,
     &       useClouds,
     &       useDLongWave
      LOGICAL cheapamlXperiodic
      LOGICAL cheapamlYperiodic
      LOGICAL useFreshWaterFlux
      LOGICAL useFluxLimit
      LOGICAL useStressOption
      LOGICAL useRelativeHumidity
      LOGICAL periodicExternalForcing_cheap
      LOGICAL useCheapTracer
      LOGICAL useTimeVarBLH
      LOGICAL useClouds
      LOGICAL useDLongWave

      COMMON /CHEAPAML_PARMS_C/
     &       AirTempFile, AirQFile, SolarFile,
     &       UWindFile, VWindFile, UStressFile, VStressFile,
     &       TrFile, QrFile,
     &       WaveHFile, WavePFile, FluxFormula, WaveModel,
     &       TracerFile, TracerRFile, cheapMaskFile, cheap_hFile,
     &       cheap_prFile, cheap_clFile, cheap_dlwFile

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
      CHARACTER*(MAX_LEN_FNAM) cheapMaskFile
      CHARACTER*(MAX_LEN_FNAM) cheap_hFile
      CHARACTER*(MAX_LEN_FNAM) cheap_prFile
      CHARACTER*(MAX_LEN_FNAM) cheap_clFile
      CHARACTER*(MAX_LEN_FNAM) cheap_dlwFile

c #endif /* ALLOW_CHEAPAML */
