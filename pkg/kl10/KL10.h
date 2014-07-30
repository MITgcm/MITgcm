C $Header: /u/gcmpack/MITgcm/pkg/kl10/KL10.h,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $

#ifdef ALLOW_KL10

CBOP
C !ROUTINE: KL10.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | KL10.h                                                   |
C     | o Basic header for Klymak and Legg (2010)                |
C     |   vertical mixing parameterization for breaking          |
C     |   internal waves.                                        |
C     |   Contains all KL10 field declarations.                  |
C     \==========================================================/

C-----------------------------------------------------------------------
C
C Constants that can be set in data.kl10
C     KLdumpFreq, KLtaveFreq - analogue to dumpFreq and taveFreq
C                              (=default)
C     KLmixingMaps - if true, include KL diagnostic maps in STDOUT
C     KLwriteState - if true, write KL state to file
C
C Time varying parameters computed by subroutine kl_calc
C     KLviscAr - Vertical eddy viscosity coefficient         (m^2/s)
C     KLdiffKr - Vertical diffusion coefficient for heat,
C                salt and tracers                            (m^2/s)
C     KLeps    - Turbulence dissipation estimate             (m^2/s^3)
C
C-----------------------------------------------------------------------
C \ev
CEOP

C      INTEGER KLnRi
C      COMMON /KL10_PARMS_I/
C     &     KLnRi
      _RL    KLdumpFreq, KLtaveFreq, KLviscMax
      COMMON /KL10_PARMS_R/
     &     KLdumpFreq, KLtaveFreq, KLviscMax

      _RL KLviscAr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KLdiffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KLeps (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /KL_FIELDS/ KLviscAr, KLdiffKr, KLeps

      LOGICAL KL10isOn, KLwriteState
      COMMON /KL10_PARMS_L/
     &     KL10isOn, KLwriteState

#endif /* ALLOW_KL10 */
