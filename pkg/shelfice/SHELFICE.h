#ifdef ALLOW_SHELFICE

CBOP
C !ROUTINE: SHELFICE.h

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SHELFICE.h
C     | o Basic header thermodnynamic shelf ice package.
C     |   Contains all SHELFICE field declarations.
C     *==========================================================*

C-----------------------------------------------------------------------
C
C--   Constants that can be set in data.shelfice
C     SHELFICEtopoFile         :: File containing the topography of the
C                                 shelfice draught (unit=m)
C     SHELFICEmassFile         :: name of shelfice Mass file
C     SHELFICEloadAnomalyFile  :: name of shelfice load anomaly file
C     SHELFICEMassDynTendFile  :: file name for other mass tendency
C                                 (e.g. dynamics)
C     useISOMIPTD              :: use simple ISOMIP thermodynamics, def: F
C     SHELFICEconserve         :: use conservative form of H&O-thermodynamics
C                                 following Jenkins et al. (2001, JPO), def: F
C     SHELFICEMassStepping     :: flag to step forward ice shelf mass/thickness
C                                 accounts for melting/freezing & dynamics
C                                 (from file or from coupling), def: F
C     SHELFICEDynMassOnly      :: step ice mass ONLY with Shelficemassdyntendency
C                                 (not melting/freezing) def: F
C     SHELFICEboundaryLayer    :: turn on vertical merging of cells to for a
C                                 boundary layer of drF thickness, def: F
C     SHI_withBL_realFWflux    :: with above BL, allow to use real-FW flux (and
C                                 adjust advective flux at boundary accordingly)
C                                 def: F
C     SHI_withBL_uStarTopDz    :: with SHELFICEboundaryLayer, compute uStar from
C                                 uVel,vVel avergaged over top Dz thickness;
C                                 def: F
C     SHELFICEadvDiffHeatFlux  :: use advective-diffusive heat flux into the
C                                 ice shelf instead of default diffusive heat
C                                 flux, see Holland and Jenkins (1999),
C                                 eq.21,22,26,31; def: F
C     SHELFICEheatTransCoeff   :: constant heat transfer coefficient that
C                                 determines heat flux into shelfice
C                                 (def: 1e-4 m/s)
C     SHELFICEsaltTransCoeff   :: constant salinity transfer coefficient that
C                                 determines salt flux into shelfice
C                                 (def: 5.05e-3 * 1e-4 m/s)
C     -----------------------------------------------------------------------
C     SHELFICEuseGammaFrict    :: use velocity dependent exchange coefficients,
C                                 see Holland and Jenkins (1999), eq.11-18,
C                                 with the following parameters (def: F):
C     SHELFICE_oldCalcUStar    :: use old uStar averaging expression
C     shiCdrag                 :: quadratic drag coefficient to compute uStar
C                                 (def: 0.0015)
C     shiZetaN                 :: ??? (def: 0.052)
C     shiRc                    :: ??? (not used, def: 0.2)
C     shiPrandtl, shiSchmidt   :: constant Prandtl (13.8) and Schmidt (2432.0)
C                                 numbers used to compute gammaTurb
C     shiKinVisc               :: constant kinetic viscosity used to compute
C                                 gammaTurb (def: 1.95e-5)
C     SHELFICEremeshFrequency  :: Frequency (in seconds) of call to
C                                 SHELFICE_REMESHING (def: 0. --> no remeshing)
C     SHELFICEsplitThreshold   :: Thickness fraction remeshing threshold above
C                                  which top-cell splits (no unit)
C     SHELFICEmergeThreshold   :: Thickness fraction remeshing threshold below
C                                  which top-cell merges with below (no unit)
C     -----------------------------------------------------------------------
C     SHELFICEDragLinear       :: linear drag at bottom shelfice (1/s)
C     SHELFICEDragQuadratic    :: quadratic drag at bottom shelfice (default
C                                 = shiCdrag or bottomDragQuadratic)
C     no_slip_shelfice         :: set slip conditions for shelfice separately,
C                                 (by default the same as no_slip_bottom, but
C                                 really should be false when there is linear
C                                 or quadratic drag)
C     SHELFICElatentHeat       :: latent heat of fusion (def: 334000 J/kg)
C     SHELFICEwriteState       :: enable output
C     SHELFICEHeatCapacity_Cp  :: heat capacity of ice shelf (def: 2000 J/K/kg)
C     rhoShelfIce              :: density of ice shelf (def: 917.0 kg/m^3)
C
C     SHELFICE_dump_mnc        :: use netcdf for snapshot output
C     SHELFICE_tave_mnc        :: use netcdf for time-averaged output
C     SHELFICE_dumpFreq        :: analoguous to dumpFreq (= default)
C     SHELFICE_taveFreq        :: analoguous to taveFreq (= default)
C
C--   Fields
C     kTopC                  :: index of the top "wet cell" (2D)
C     K_icefront             :: index of the bottommost ice front cell (2D) 
C     R_shelfIce             :: shelfice topography [m]
C     shelficeMassInit       :: ice-shelf mass (per unit area) (kg/m^2)
C     shelficeMass           :: ice-shelf mass (per unit area) (kg/m^2)
C     shelfIceMassDynTendency :: other mass balance tendency  (kg/m^2/s)
C                            ::  (e.g., from dynamics)
C     shelficeLoadAnomaly    :: pressure load anomaly of shelfice (Pa)
C     shelficeHeatFlux       :: upward heat flux (W/m^2)
C     shelficeFreshWaterFlux :: upward fresh water flux (virt. salt flux)
C                               (kg/m^2/s)
C     shelficeForcingT       :: analogue of surfaceForcingT
C                               units are  r_unit.Kelvin/s (=Kelvin.m/s if r=z)
C     shelficeForcingS       :: analogue of surfaceForcingS
C                               units are  r_unit.psu/s (=psu.m/s if r=z)
#ifdef ALLOW_DIAGNOSTICS
C     shelficeDragU          :: Ice-Shelf stress (for diagnostics), Zonal comp.
C                               Units are N/m^2 ;   > 0 increase top uVel
C     shelficeDragV          :: Ice-Shelf stress (for diagnostics), Merid. comp.
C                               Units are N/m^2 ;   > 0 increase top vVel
#endif /* ALLOW_DIAGNOSTICS */

C-----------------------------------------------------------------------
C \ev
CEOP

      COMMON /SHELFICE_PARMS_I/  kTopC,
     &     SHELFICEselectDragQuadr, K_icefront
      INTEGER kTopC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER SHELFICEselectDragQuadr
      INTEGER K_icefront (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      
      
      COMMON /SHELFICE_PARMS_R/
     &     SHELFICE_dumpFreq, SHELFICE_taveFreq,
     &     SHELFICEheatTransCoeff, SHELFICEsaltTransCoeff,
     &     rhoShelfice, SHELFICEkappa,
     &     SHELFICElatentHeat,
     &     SHELFICEheatCapacity_Cp,
     &     SHELFICEthetaSurface,
     &     SHELFICEsalinity,
     &     SHELFICEDragLinear, SHELFICEDragQuadratic,
     &     shiCdrag, shiZetaN, shiRc,
     &     shiPrandtl, shiSchmidt, shiKinVisc,
     &     SHELFICEremeshFrequency,
     &     SHELFICEsplitThreshold, SHELFICEmergeThreshold,
     &     iceFrontThetaHorizDiffusionLength,
     &     iceFrontThetaInterior

      _RL SHELFICE_dumpFreq, SHELFICE_taveFreq
      _RL SHELFICEheatTransCoeff
      _RL SHELFICEsaltTransCoeff
      _RL SHELFICElatentHeat
      _RL SHELFICEheatCapacity_Cp
      _RL rhoShelfice
      _RL SHELFICEkappa
      _RL SHELFICEDragLinear
      _RL SHELFICEDragQuadratic
      _RL SHELFICEthetaSurface
      _RL SHELFICEsalinity
      _RL shiCdrag, shiZetaN, shiRc
      _RL shiPrandtl, shiSchmidt, shiKinVisc
      _RL SHELFICEremeshFrequency
      _RL SHELFICEsplitThreshold
      _RL SHELFICEmergeThreshold
      _RL iceFrontThetaHorizDiffusionLength
      _RL iceFrontThetaInterior
      
      COMMON /SHELFICE_FIELDS_RL/
     &     shelficeMass, shelficeMassInit,
     &     shelficeLoadAnomaly,
     &     shelficeForcingT, shelficeForcingS,
     &     shiTransCoeffT, shiTransCoeffS, 
     &     iceFrontForcingT, iceFrontForcingS

      _RL shelficeMass          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeMassInit      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeLoadAnomaly   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeForcingT      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeForcingS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifndef ALLOW_shiTransCoeff_3d
      _RL shiTransCoeffT        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shiTransCoeffS        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#else
      _RL shiTransCoeffT     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL shiTransCoeffS     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
      _RL iceFrontForcingT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL iceFrontForcingS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      COMMON /SHELFICE_FIELDS_RS/
     &     R_shelfIce,
     &     shelficeHeatFlux,
     &     shelfIceFreshWaterFlux,
     &     shelfIceMassDynTendency,
     &     iceFrontHeatFlux, iceFrontFreshWaterFlux,
     &     SHIICFHeatFlux, SHIICFFreshWaterFlux

      _RL R_shelfIce            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeHeatFlux      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeFreshWaterFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SHIICFHeatFlux      
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SHIICFFreshWaterFlux
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL shelfIceMassDynTendency
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceFrontHeatFlux      
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr, nSx,nSy)
      _RL iceFrontFreshWaterFlux
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr, nSx,nSy)

#ifdef ALLOW_SHIFWFLX_CONTROL
      COMMON /SHELFICE_MASKS_CTRL/ maskSHI
      _RS maskSHI  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_SHIFWFLX_CONTROL */

#ifdef ALLOW_DIAGNOSTICS
      COMMON /SHELFICE_DIAG_DRAG/ shelficeDragU, shelficeDragV
      _RS shelficeDragU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS shelficeDragV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_DIAGNOSTICS */

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
      COMMON /SHELFICE_MASKS/ mask2dSHIICF, mask3dSHIICF, 
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
      COMMON /SHELFICE_ICEFRONT_I/CURI_ARR, CURJ_ARR
      INTEGER CURI_ARR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4) 
      INTEGER CURJ_ARR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4) 
      COMMON /SHELFICE_ICEFRONT_R/icefrontwidth_arr
      _RL icefrontwidth_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4) 

      LOGICAL SHELFICEisOn
      LOGICAL useISOMIPTD
      LOGICAL SHELFICEconserve
      LOGICAL SHELFICEboundaryLayer
      LOGICAL SHI_withBL_realFWflux
      LOGICAL SHI_withBL_uStarTopDz
      LOGICAL no_slip_shelfice
      LOGICAL SHELFICEwriteState
      LOGICAL SHELFICE_dump_mdsio
      LOGICAL SHELFICE_tave_mdsio
      LOGICAL SHELFICE_dump_mnc
      LOGICAL SHELFICE_tave_mnc
      LOGICAL SHELFICEadvDiffHeatFlux
      LOGICAL SHELFICEuseGammaFrict
      LOGICAL SHELFICE_oldCalcUStar
      LOGICAL SHELFICEMassStepping
      LOGICAL SHELFICEDynMassOnly
      COMMON /SHELFICE_PARMS_L/
     &     SHELFICEisOn,
     &     useISOMIPTD,
     &     SHELFICEconserve,
     &     SHELFICEboundaryLayer,
     &     SHI_withBL_realFWflux,
     &     SHI_withBL_uStarTopDz,
     &     no_slip_shelfice,
     &     SHELFICEwriteState,
     &     SHELFICE_dump_mdsio,
     &     SHELFICE_tave_mdsio,
     &     SHELFICE_dump_mnc,
     &     SHELFICE_tave_mnc,
     &     SHELFICEadvDiffHeatFlux,
     &     SHELFICEuseGammaFrict,
     &     SHELFICE_oldCalcUStar,
     &     SHELFICEMassStepping,
     &     SHELFICEDynMassOnly

      CHARACTER*(MAX_LEN_FNAM) SHELFICEloadAnomalyFile
      CHARACTER*(MAX_LEN_FNAM) SHELFICEmassFile
      CHARACTER*(MAX_LEN_FNAM) SHELFICEtopoFile
      CHARACTER*(MAX_LEN_FNAM) SHELFICEMassDynTendFile
      CHARACTER*(MAX_LEN_FNAM) SHELFICETransCoeffTFile
      COMMON /SHELFICE_PARM_C/
     &     SHELFICEloadAnomalyFile,
     &     SHELFICEmassFile,
     &     SHELFICEtopoFile,
     &     SHELFICEMassDynTendFile,
     &     SHELFICETransCoeffTFile

      COMMON /ICEFRONT_FIELDS_RS/
     &     R_icefront,
     &     icefrontlength
      _RS R_icefront     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS icefrontlength (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      CHARACTER*(MAX_LEN_FNAM) ICEFRONTlengthFile
      CHARACTER*(MAX_LEN_FNAM) ICEFRONTdepthFile
      COMMON /ICEFRONT_PARM_C/
     &     ICEFRONTlengthFile,
     &     ICEFRONTdepthFile

#endif /* ALLOW_SHELFICE */
