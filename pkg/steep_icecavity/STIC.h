#ifdef ALLOW_STEEP_ICECAVITY

CBOP
C !ROUTINE: STIC.h

C !DESCRIPTION: \bv
C     *==========================================================*
C     | STIC.h
C     | o Basic header thermodnynamic shelf ice package.
C     |   Contains all STIC field declarations.
C     *==========================================================*

C-----------------------------------------------------------------------
C
C--   Constants that can be set in data.stic
C     STICtopoFile         :: File containing the topography of the
C                                 shelfice draught (unit=m)
C     STICmassFile         :: name of shelfice Mass file
C     STICloadAnomalyFile  :: name of shelfice load anomaly file
C     STICMassDynTendFile  :: file name for other mass tendency
C                                 (e.g. dynamics)
C     useISOMIPTD              :: use simple ISOMIP thermodynamics, def: F
C     STICconserve         :: use conservative form of H&O-thermodynamics
C                                 following Jenkins et al. (2001, JPO), def: F
C     STICMassStepping     :: flag to step forward ice shelf mass/thickness
C                                 accounts for melting/freezing & dynamics
C                                 (from file or from coupling), def: F
C     STICDynMassOnly      :: step ice mass ONLY with sticMassdyntendency
C                                 (not melting/freezing) def: F
C     STICboundaryLayer    :: turn on vertical merging of cells to for a
C                                 boundary layer of drF thickness, def: F
C     SHI_withBL_realFWflux    :: with above BL, allow to use real-FW flux (and
C                                 adjust advective flux at boundary accordingly)
C                                 def: F
C     SHI_withBL_uStarTopDz    :: with STICboundaryLayer, compute uStar from
C                                 uVel,vVel avergaged over top Dz thickness;
C                                 def: F
C     STICadvDiffHeatFlux  :: use advective-diffusive heat flux into the
C                                 shelf instead of default diffusive heat
C                                 flux, see Holland and Jenkins (1999),
C                                 eq.21,22,26,31; def: F
C     STICsaltToHeatRatio  :: constant ratio giving
C                                 STICsaltTransCoeff/STICheatTransCoeff
C                                 (def: 5.05e-3)
C     STICheatTransCoeff   :: constant heat transfer coefficient that
C                                 determines heat flux into shelfice
C                                 (def: 1e-4 m/s)
C     STICsaltTransCoeff   :: constant salinity transfer coefficient that
C                                 determines salt flux into shelfice
C                                 (def: STICsaltToHeatRatio * STICheatTransCoeff)
C     -----------------------------------------------------------------------
C     STICuseGammaFrict    :: use velocity dependent exchange coefficients,
C                                 see Holland and Jenkins (1999), eq.11-18,
C                                 with the following parameters (def: F):
C     STIC_oldCalcUStar    :: use old uStar averaging expression
C     shiCdrag                 :: quadratic drag coefficient to compute uStar
C                                 (def: 0.0015)
C     shiZetaN                 :: ??? (def: 0.052)
C     shiRc                    :: ??? (not used, def: 0.2)
C     shiPrandtl, shiSchmidt   :: constant Prandtl (13.8) and Schmidt (2432.0)
C                                 numbers used to compute gammaTurb
C     shiKinVisc               :: constant kinetic viscosity used to compute
C                                 gammaTurb (def: 1.95e-5)
C     STICremeshFrequency  :: Frequency (in seconds) of call to
C                                 STIC_REMESHING (def: 0. --> no remeshing)
C     STICsplitThreshold   :: Thickness fraction remeshing threshold above
C                                  which top-cell splits (no unit)
C     STICmergeThreshold   :: Thickness fraction remeshing threshold below
C                                  which top-cell merges with below (no unit)
C     -----------------------------------------------------------------------
C     STICDragLinear       :: linear drag at bottom shelfice (1/s)
C     STICDragQuadratic    :: quadratic drag at bottom shelfice (default
C                                 = shiCdrag or bottomDragQuadratic)
C     no_slip_stic         :: set slip conditions for shelfice separately,
C                                 (by default the same as no_slip_bottom, but
C                                 really should be false when there is linear
C                                 or quadratic drag)
C     STIClatentHeat       :: latent heat of fusion (def: 334000 J/kg)
C     STICwriteState       :: enable output
C     STICHeatCapacity_Cp  :: heat capacity of ice shelf (def: 2000 J/K/kg)
C     rhoSTIC              :: density of ice shelf (def: 917.0 kg/m^3)
C
C     STIC_dump_mnc        :: use netcdf for snapshot output
C     STIC_tave_mnc        :: use netcdf for time-averaged output
C     STIC_dumpFreq        :: analoguous to dumpFreq (= default)
C     STIC_taveFreq        :: analoguous to taveFreq (= default)
C
C--   Fields
C     kTopC                  :: index of the top "wet cell" beneath the ice shelf (2D)
C     K_icefront             :: index of the bottommost ice front cell (2D)
C     R_shelfIce             :: shelfice topography [m]
C     sticMassInit           :: ice-shelf mass (per unit area) (kg/m^2)
C     sticMass               :: ice-shelf mass (per unit area) (kg/m^2)
C     sticMassDynTendency    :: other mass balance tendency  (kg/m^2/s)
C                            ::  (e.g., from dynamics)
C     shelficeLoadAnomaly    :: pressure load anomaly of shelfice (Pa)
C     shelficeHeatFlux       :: upward heat flux (W/m^2)
C     shelficeFreshWaterFlux :: upward fresh water flux (virt. salt flux)
C                               (kg/m^2/s)
C     shelficeForcingT       :: analogue of surfaceForcingT
C                               units are  r_unit.Kelvin/s (=Kelvin.m/s if r=z)
C     shelficeForcingS       :: analogue of surfaceForcingS
C                               units are  r_unit.g/kg/s (=g/kg.m/s if r=z)
#ifdef ALLOW_DIAGNOSTICS
C     shelficeDragU          :: Ice-Shelf stress (for diagnostics), Zonal comp.
C                               Units are N/m^2 ;   > 0 increase top uVel
C     shelficeDragV          :: Ice-Shelf stress (for diagnostics), Merid. comp.
C                               Units are N/m^2 ;   > 0 increase top vVel
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_CTRL
C   maskSHI           ::  Mask=1 where ice shelf is present on surface
C                           layer, showing full 2D ice shelf extent.
C                           =maskC for rest of k values
C                           Used with ice shelf fwflx
C                           or shiTransCoeffT/S ctrl.
#endif
C-----------------------------------------------------------------------
C \ev
CEOP

      COMMON /STIC_PARMS_I/  kTopC,
     &     STICselectDragQuadr, K_icefront
      INTEGER kTopC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER STICselectDragQuadr
      INTEGER K_icefront (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /STIC_PARMS_R/
     &     STIC_dumpFreq, STIC_taveFreq,
     &     STICsaltToHeatRatio,
     &     STICheatTransCoeff, STICsaltTransCoeff,
     &     rhoSTIC, STICkappa,
     &     STIClatentHeat,
     &     STICheatCapacity_Cp,
     &     STICthetaSurface,
     &     STICsalinity,
     &     STICDragLinear, STICDragQuadratic,
     &     shiCdrag, shiZetaN, shiRc,
     &     shiPrandtl, shiSchmidt, shiKinVisc,
     &     iceFrontThetaHorizDiffusionLength,
     &     iceFrontThetaInterior,
     &     STICremeshFrequency,
     &     STICsplitThreshold, STICmergeThreshold

      _RL STIC_dumpFreq, STIC_taveFreq
      _RL STICsaltToHeatRatio
      _RL STICheatTransCoeff
      _RL STICsaltTransCoeff
      _RL STIClatentHeat
      _RL STICheatCapacity_Cp
      _RL rhoSTIC
      _RL STICkappa
      _RL STICDragLinear
      _RL STICDragQuadratic
      _RL STICthetaSurface
      _RL shiCdrag, shiZetaN, shiRc
      _RL shiPrandtl, shiSchmidt, shiKinVisc
      _RL STICremeshFrequency
      _RL STICsplitThreshold
      _RL STICmergeThreshold
      _RL STICsalinity
      _RL iceFrontThetaHorizDiffusionLength
      _RL iceFrontThetaInterior

      COMMON /STIC_FIELDS_RL/
     &     sticMass, sticMassInit,
     &     shelficeLoadAnomaly,
     &     shelficeForcingT, shelficeForcingS,
     &     iceFrontForcingT, iceFrontForcingS,
     &     shiCDragFld, shiDragQuadFld

      _RL sticMass           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sticMassInit       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeLoadAnomaly(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeForcingT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeForcingS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceFrontForcingT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL iceFrontForcingS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL shiCDragFld        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shiDragQuadFld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifndef ALLOW_shiTransCoeff_3d
      COMMON /STIC_GAMMA_RL/
     &     shiTransCoeffT, shiTransCoeffS
      _RL shiTransCoeffT     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shiTransCoeffS     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#else
      COMMON /STIC_GAMMA3D_RL/
     &     shiTransCoeffT3d, shiTransCoeffS3d
      _RL shiTransCoeffT3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL shiTransCoeffS3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

      COMMON /STIC_FIELDS_RS/
     &     R_shelfIce,
     &     shelficeHeatFlux,
     &     shelfIceFreshWaterFlux,
     &     sticMassDynTendency,
     &     iceFrontHeatFlux, iceFrontFreshWaterFlux,
     &     SHIICFHeatFlux, SHIICFFreshWaterFlux

      _RS R_shelfIce            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS shelficeHeatFlux      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS shelficeFreshWaterFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS
     &   sticMassDynTendency(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceFrontHeatFlux
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr, nSx,nSy)
      _RL iceFrontFreshWaterFlux
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr, nSx,nSy)
      _RL SHIICFHeatFlux
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SHIICFFreshWaterFlux
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_CTRL
      COMMON /STIC_MASKS_CTRL/ maskSHI
      _RS maskSHI  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_CTRL */

C ow - 06/29/2018
C ow - maskSHI above is not consistent with the spirit of gencost.
C ow -   Use the following masks below instead.
C ow - mask2dSHIICF: 2d shelf-ice & ice-front mask:
C         1 for having shelf-ice and/or ice-front at one or more vertical levels
C         and 0 otherwise.
C      mask3dSHIICF: 3d shelf-ice & ice-front mask.
C      mask2dSHI: 2d shelf-ice mask
C      mask3dSHI: 3d shelf-ice mask
C      mask2dICF: 2d ice-front mask: 1 for having ice-front at one or more vertical levels.
C      mask3dICF: 3d ice-front mask
      COMMON /STIC_MASKS/ mask2dSHIICF, mask3dSHIICF,
     &        mask2dSHI, mask3dSHI, mask2dICF, mask3dICF
      _RS mask2dSHIICF  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask3dSHIICF  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS mask2dSHI  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask3dSHI  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS mask2dICF  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask3dICF  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C ow - 07/23/2020
C ow - CURI_ARR, CURJ_AA,
C      CURI_ARR: i-index for neighboring ice-front points
C      CURJ_ARR: j-index for neighboring ice-front points
C      icefrontwidth_arr: ice-front width in meters
      COMMON /STIC_ICEFRONT_I/CURI_ARR, CURJ_ARR
      INTEGER CURI_ARR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4)
      INTEGER CURJ_ARR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4)
      COMMON /STIC_ICEFRONT_R/icefrontwidth_arr
      _RL icefrontwidth_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4)

#ifdef ALLOW_DIAGNOSTICS
      COMMON /STIC_DIAG_DRAG/ shelficeDragU, shelficeDragV
      _RS shelficeDragU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS shelficeDragV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_DIAGNOSTICS */

      LOGICAL STICisOn
      LOGICAL useISOMIPTD
      LOGICAL STICconserve
      LOGICAL STICboundaryLayer
      LOGICAL SHI_withBL_realFWflux
      LOGICAL SHI_withBL_uStarTopDz
      LOGICAL no_slip_stic
      LOGICAL STICwriteState
      LOGICAL STIC_dump_mdsio
      LOGICAL STIC_tave_mdsio
      LOGICAL STIC_dump_mnc
      LOGICAL STIC_tave_mnc
      LOGICAL STICadvDiffHeatFlux
      LOGICAL STICuseGammaFrict
      LOGICAL STIC_oldCalcUStar
      LOGICAL STICMassStepping
      LOGICAL STICDynMassOnly
      COMMON /STIC_PARMS_L/
     &     STICisOn,
     &     useISOMIPTD,
     &     STICconserve,
     &     STICboundaryLayer,
     &     SHI_withBL_realFWflux,
     &     SHI_withBL_uStarTopDz,
     &     no_slip_stic,
     &     STICwriteState,
     &     STIC_dump_mdsio,
     &     STIC_tave_mdsio,
     &     STIC_dump_mnc,
     &     STIC_tave_mnc,
     &     STICadvDiffHeatFlux,
     &     STICuseGammaFrict,
     &     STIC_oldCalcUStar,
     &     STICMassStepping,
     &     STICDynMassOnly

      CHARACTER*(MAX_LEN_FNAM) STICloadAnomalyFile
      CHARACTER*(MAX_LEN_FNAM) STICmassFile
      CHARACTER*(MAX_LEN_FNAM) STICtopoFile
      CHARACTER*(MAX_LEN_FNAM) STICMassDynTendFile
      CHARACTER*(MAX_LEN_FNAM) STICTransCoeffTFile
      COMMON /STIC_PARM_C/
     &     STICloadAnomalyFile,
     &     STICmassFile,
     &     STICtopoFile,
     &     STICMassDynTendFile,
     &     STICTransCoeffTFile

      COMMON /ICEFRONT_FIELDS_RS/
     &     R_icefront, icefrontlength
      _RS R_icefront     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS icefrontlength (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      CHARACTER*(MAX_LEN_FNAM) ICEFRONTlengthFile
      CHARACTER*(MAX_LEN_FNAM) ICEFRONTdepthFile
      COMMON /ICEFRONT_PARM_C/
     &     ICEFRONTlengthFile,
     &     ICEFRONTdepthFile

#endif /* ALLOW_STEEP_ICECAVITY */
