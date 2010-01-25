C $Header: /u/gcmpack/MITgcm/pkg/icefront/ICEFRONT.h,v 1.3 2010/01/25 22:37:19 dimitri Exp $
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
C     ICEFRONTheatTransCoeff   - heat transfer coefficient that determines
C                                 heat flux into icefront (m/s)
C     ICEFRONTsaltTransCoeff   - salinity transfer coefficient that determines
C                                salt flux into icefront (m/s)
C     ICEFRONTlatentHeat       - latent heat of fusion (J/kg)
C     useISOMIPTD              - use simple ISOMIP thermodynamics
C     ICEFRONTconserve         - use conservative form of H&O-thermodynamics 
C                                following Jenkins et al. (2001, JPO)
C
C--   Fields
C     R_icefront             - icefront topography [m]
C     icefrontHeatFlux       - upward heat flux [W/m^2]
C     icefrontFreshWaterFlux - upward fresh water flux (virt. salt flux) [m/s]
C     icefrontForcingT       - analogue of surfaceForcingT
C     icefrontForcingS       - analogue of surfaceForcingS
C-----------------------------------------------------------------------
C \ev
CEOP

      COMMON /ICEFRONT_PARMS_R/ 
     &     ICEFRONTheatTransCoeff, ICEFRONTsaltTransCoeff,
     &     rhoIcefront, ICEFRONTkappa,
     &     ICEFRONTlatentHeat, recip_ICEFRONTlatentHeat,
     &     ICEFRONTheatCapacity_Cp,
     &     ICEFRONTthetaSurface
      _RL ICEFRONTheatTransCoeff
      _RL ICEFRONTsaltTransCoeff
      _RL ICEFRONTlatentHeat
      _RL ICEFRONTheatCapacity_Cp
      _RL rhoIcefront
      _RL ICEFRONTkappa
      _RL recip_ICEFRONTlatentHeat
      _RL ICEFRONTthetaSurface

      COMMON /ICEFRONT_FIELDS_RL/ 
     &     icefrontForcingT,
     &     icefrontForcingS
      _RL icefrontForcingT      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL icefrontForcingS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      COMMON /ICEFRONT_FIELDS_RS/ 
     &     R_icefront,
     &     icefrontHeatFlux,
     &     icefrontFreshWaterFlux
      _RS R_icefront            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS icefrontHeatFlux      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS icefrontFreshWaterFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      
      LOGICAL ICEFRONTisOn
      LOGICAL useISOMIPTD
      LOGICAL ICEFRONTconserve
      COMMON /ICEFRONT_PARMS_L/
     &     ICEFRONTisOn,
     &     useISOMIPTD,
     &     ICEFRONTconserve

      CHARACTER*(MAX_LEN_FNAM) ICEFRONTtopoFile
      COMMON /ICEFRONT_PARM_C/ 
     &     ICEFRONTtopoFile

#endif /* ALLOW_ICEFRONT */
