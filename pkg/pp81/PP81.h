#ifdef ALLOW_PP81

CBOP
C !ROUTINE: PP81.h

C !DESCRIPTION: \bv
C     *==========================================================*
C     | PP81.h                                                   |
C     | o Basic header for Pacanowski and Philander (1981)       |
C     |   vertical mixing parameterization.                      |
C     |   Contains all PP81 field declarations.                  |
C     *==========================================================*

C-----------------------------------------------------------------------
C
C Constants that can be set in data.pp
C     PPnRi - exponent of denominator of Eq(1) in PP81
C     PPviscMin, PPdiffMin   - minimum viscosity/diffusivity in
C                              surface layer.
C                              Only used if ALLOW_PP81MOD is defined
C     PPviscMax              - maximum allowed viscosity
C     PPnu0, PPalpha         - further parameters
C     RiLimit                - minimum Richardson number,
C                              follows from PPviscMax
C     PPdumpFreq             - analogue to dumpFreq (=default)
C     PPmixingMaps - if true, include PP diagnostic maps in STDOUT
C     PPwriteState - if true, write PP state to file
C
C Time varying parameters computed by subroutine pp_calc
C     PPviscAr - Vertical eddy viscosity coefficient         (m^2/s)
C     PPdiffKr - Vertical diffusion coefficient for heat,
C                salt and tracers                            (m^2/s)
C
C-----------------------------------------------------------------------
C \ev
CEOP

      INTEGER PPnRi
      COMMON /PP81_PARMS_I/
     &     PPnRi
      _RL    PPviscMin, PPdiffMin, PPviscMax
      _RL    PPnu0, PPalpha, RiLimit
      _RL    PPdumpFreq
      COMMON /PP81_PARMS_R/
     &     PPviscMin, PPdiffMin, PPviscMax,
     &     PPnu0, PPalpha, RiLimit,
     &     PPdumpFreq

      _RL PPviscAr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL PPdiffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /PP_FIELDS/ PPviscAr, PPdiffKr

      LOGICAL PP81isOn, PPmixingMaps, PPwriteState
      COMMON /PP81_PARMS_L/
     &     PP81isOn, PPmixingMaps, PPwriteState

#endif /* ALLOW_PP81 */
