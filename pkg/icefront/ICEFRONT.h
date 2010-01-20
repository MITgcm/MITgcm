C $Header: /u/gcmpack/MITgcm/pkg/icefront/ICEFRONT.h,v 1.1 2010/01/20 23:33:45 dimitri Exp $
C $Name:  $

#ifdef ALLOW_ICEFRONT

CBOP
C !ROUTINE: ICEFRONT.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | ICEFRONT.h                                               |
C     | o Basic header thermodnynamic shelf ice package.         |
C     |   Contains all ICEFRONT field declarations.              |
C     \==========================================================/

C-----------------------------------------------------------------------
C
C--   Constants that can be set in data.icefront
C     ICEFRONTtopoFile         - File containing the topography of the 
C                                icefront draught (unit=m)
C     ICEFRONTloadAnomalyFile  - name of icefront load anomaly file
C     ICEFRONTDragLinear       - linear drag at bottom icefront (1/s)
C     ICEFRONTDragQuadratic    - quadratic drag at bottom icefront (1/m)
C     ICEFRONTheatTransCoeff   - heat transfer coefficient that determines
C                                 heat flux into icefront (m/s)
C     ICEFRONTsaltTransCoeff   - salinity transfer coefficient that determines
C                                salt flux into icefront (m/s)
C     ICEFRONTlatentHeat       - latent heat of fusion (J/kg)
C     useISOMIPTD              - use simple ISOMIP thermodynamics
C     ICEFRONTconserve         - use conservative form of H&O-thermodynamics 
C                                following Jenkins et al. (2001, JPO)
C     ICEFRONTboundaryLayer    - turn on vertical merging of cells to for a 
C                                boundary layer of drF thickness
C     no_slip_icefront         - set slip conditions for icefront separately,
C                                (by default the same as no_slip_bottom) 
C     ICEFRONTwriteState       - enable output
C     ICEFRONT_dump_mnc        - use netcdf for snapshot output
C     ICEFRONT_tave_mnc        - use netcdf for time-averaged output
C     ICEFRONT_dumpFreq        - analoguous to dumpFreq (= default)
C     ICEFRONT_taveFreq        - analoguous to taveFreq (= default)
C
C--   Fields
C     ktopC                  - index of the top "wet cell" (2D)
C     R_icefront             - icefront topography [m]
C     icefrontLoadAnomaly    - pressure load anomaly of icefront [Pa]
C     icefrontHeatFlux       - upward heat flux [W/m^2]
C     icefrontFreshWaterFlux - upward fresh water flux (virt. salt flux) [m/s]
C     icefrontForcingT       - analogue of surfaceForcingT
C     icefrontForcingS       - analogue of surfaceForcingS
C-----------------------------------------------------------------------
C \ev
CEOP

      COMMON /ICEFRONT_PARMS_I/  kTopC
      INTEGER kTopC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /ICEFRONT_PARMS_R/ 
     &     ICEFRONT_dumpFreq, ICEFRONT_taveFreq,
     &     ICEFRONTheatTransCoeff, ICEFRONTsaltTransCoeff,
     &     rhoIcefront, ICEFRONTkappa,
     &     ICEFRONTlatentHeat, recip_ICEFRONTlatentHeat,
     &     ICEFRONTheatCapacity_Cp,
     &     ICEFRONTthetaSurface,
     &     ICEFRONTDragLinear, ICEFRONTDragQuadratic
      _RL ICEFRONT_dumpFreq, ICEFRONT_taveFreq
      _RL ICEFRONTheatTransCoeff
      _RL ICEFRONTsaltTransCoeff
      _RL ICEFRONTlatentHeat
      _RL ICEFRONTheatCapacity_Cp
      _RL rhoIcefront
      _RL ICEFRONTkappa
      _RL recip_ICEFRONTlatentHeat
      _RL ICEFRONTDragLinear
      _RL ICEFRONTDragQuadratic
      _RL ICEFRONTthetaSurface

      COMMON /ICEFRONT_FIELDS_RL/ 
     &     icefrontForcingT,
     &     icefrontForcingS
      _RL icefrontForcingT      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL icefrontForcingS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /ICEFRONT_FIELDS_RS/ 
     &     R_icefront,
     &     icefrontLoadAnomaly, 
     &     icefrontHeatFlux,
     &     icefrontFreshWaterFlux
      _RS R_icefront            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS icefrontLoadAnomaly   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS icefrontHeatFlux      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS icefrontFreshWaterFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      
      LOGICAL ICEFRONTisOn
      LOGICAL useISOMIPTD
      LOGICAL ICEFRONTconserve
      LOGICAL ICEFRONTboundaryLayer
      LOGICAL no_slip_icefront
      LOGICAL ICEFRONTwriteState
      LOGICAL ICEFRONT_dump_mdsio
      LOGICAL ICEFRONT_tave_mdsio
      LOGICAL ICEFRONT_dump_mnc
      LOGICAL ICEFRONT_tave_mnc
      COMMON /ICEFRONT_PARMS_L/
     &     ICEFRONTisOn,
     &     useISOMIPTD,
     &     ICEFRONTconserve,
     &     ICEFRONTboundaryLayer,
     &     no_slip_icefront,
     &     ICEFRONTwriteState,
     &     ICEFRONT_dump_mdsio,
     &     ICEFRONT_tave_mdsio,
     &     ICEFRONT_dump_mnc,
     &     ICEFRONT_tave_mnc

      CHARACTER*(MAX_LEN_FNAM) ICEFRONTloadAnomalyFile
      CHARACTER*(MAX_LEN_FNAM) ICEFRONTtopoFile
      COMMON /ICEFRONT_PARM_C/ 
     &     ICEFRONTloadAnomalyFile, 
     &     ICEFRONTtopoFile

#endif /* ALLOW_ICEFRONT */
