C $Header: /u/gcmpack/MITgcm/pkg/shelfice/SHELFICE.h,v 1.4 2006/08/14 16:52:46 mlosch Exp $
C $Name:  $

#ifdef ALLOW_SHELFICE

CBOP
C !ROUTINE: SHELFICE.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SHELFICE.h                                               |
C     | o Basic header thermodnynamic shelf ice package.         |
C     |   Contains all SHELFICE field declarations.              |
C     \==========================================================/

C-----------------------------------------------------------------------
C
C--   Constants that can be set in data.shelfice
C     SHELFICEloadAnomalyFile  - name of shelfice load anomaly file
C     SHELFICEDragLinear       - linear drag at bottom shelfice (1/s)
C     SHELFICEDragQuadratic    - quadratic drag at bottom shelfice (1/m)
C     SHELFICEheatTransCoeff   - heat transfer coefficient that determines
C                                 heat flux into shelfice (m/s)
C     SHELFICEsaltTransCoeff   - salinity transfer coefficient that determines
C                                salt flux into shelfice (m/s)
C     SHELFICElatentHeat       - latent heat of fusion (J/kg)
C     useISOMIPTD              - use simple ISOMIP thermodynamics
C     SHELFICEboundaryLayer    - turn on vertical merging of cells to for a 
C                                boundary layer of drF thickness
C     no_slip_shelfice         - set slip conditions for shelfice separately,
C                                (by default the same as no_slip_bottom) 
C     SHELFICEwriteState       - enable output
C     SHELFICE_dump_mnc        - use netcdf for snapshot output
C     SHELFICE_tave_mnc        - use netcdf for time-averaged output
C     SHELFICE_dumpFreq        - analoguous to dumpFreq (= default)
C     SHELFICE_taveFreq        - analoguous to taveFreq (= default)
C
C--   Fields
C     ktopC                  - index of the top "wet cell" (2D)
C     R_shelfIce             - shelfice topography [m]
C     shelficeLoadAnomaly    - pressure load anomaly of shelfice [Pa]
C     shelficeHeatFlux       - upward heat flux [W/m^2]
C     shelficeFreshWaterFlux - upward fresh water flux (virt. salt flux) [m/s]
C     shelficeForcingT       - analogue of surfaceForcingT
C     shelficeForcingS       - analogue of surfaceForcingS
C-----------------------------------------------------------------------
C \ev
CEOP

      COMMON /SHELFICE_PARMS_I/  kTopC
      INTEGER kTopC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /SHELFICE_PARMS_R/ 
     &     SHELFICE_dumpFreq, SHELFICE_taveFreq,
     &     SHELFICEheatTransCoeff, SHELFICEsaltTransCoeff,
     &     rhoShelfice, SHELFICEkappa,
     &     SHELFICElatentHeat, recip_SHELFICElatentHeat,
     &     SHELFICEheatCapacity_Cp,
     &     SHELFICEthetaSurface,
     &     SHELFICEDragLinear, SHELFICEDragQuadratic
      _RL SHELFICE_dumpFreq, SHELFICE_taveFreq
      _RL SHELFICEheatTransCoeff
      _RL SHELFICEsaltTransCoeff
      _RL SHELFICElatentHeat
      _RL SHELFICEheatCapacity_Cp
      _RL rhoShelfice
      _RL SHELFICEkappa
      _RL recip_SHELFICElatentHeat
      _RL SHELFICEDragLinear
      _RL SHELFICEDragQuadratic
      _RL SHELFICEthetaSurface

      COMMON /SHELFICE_FIELDS_RL/ 
     &     shelficeForcingT,
     &     shelficeForcingS
      _RL shelficeForcingT      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL shelficeForcingS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /SHELFICE_FIELDS_RS/ 
     &     R_shelfIce,
     &     shelficeLoadAnomaly, 
     &     shelficeHeatFlux,
     &     shelfIceFreshWaterFlux
      _RS R_shelfIce            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS shelficeLoadAnomaly   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS shelficeHeatFlux      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS shelficeFreshWaterFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      
      LOGICAL SHELFICEisOn
      LOGICAL useISOMIPTD
      LOGICAL SHELFICEboundaryLayer
      LOGICAL no_slip_shelfice
      LOGICAL SHELFICEwriteState
      LOGICAL SHELFICE_dump_mdsio
      LOGICAL SHELFICE_tave_mdsio
      LOGICAL SHELFICE_dump_mnc
      LOGICAL SHELFICE_tave_mnc
      COMMON /SHELFICE_PARMS_L/
     &     SHELFICEisOn,
     &     useISOMIPTD,
     &     SHELFICEboundaryLayer,
     &     no_slip_shelfice,
     &     SHELFICEwriteState,
     &     SHELFICE_dump_mdsio,
     &     SHELFICE_tave_mdsio,
     &     SHELFICE_dump_mnc,
     &     SHELFICE_tave_mnc

      CHARACTER*(MAX_LEN_FNAM) SHELFICEloadAnomalyFile
      COMMON /SHELFICE_PARM_C/ 
     &     SHELFICEloadAnomalyFile

#endif /* ALLOW_SHELFICE */
