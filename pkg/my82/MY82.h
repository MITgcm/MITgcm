#ifdef ALLOW_MY82

CBOP
C !ROUTINE: MY82.h

C !DESCRIPTION: \bv
C     *==========================================================*
C     | MY82.h                                                   |
C     | o Basic header for Mellor & Yamada (1982), level 2.0     |
C     |   vertical mixing parameterization.                      |
C     |   Contains all MY82 field declarations.                  |
C     *==========================================================*

C-----------------------------------------------------------------------
C
C Constants that can be set in data.my82
C     MYviscMax, MYdiffMax   - maximum allowed viscosity, diffusivity
C     MYhblScale             - scale the boundary length scale
C     RiMax                  - Maximum of Richardson number
C     MYdumpFreq       - analogue to dumpFreq (=default)
C     MYmixingMaps     - if true, include MY diagnostic maps in STDOUT
C     MYwriteState     - if true, write MY state to file
C
C Time varying parameters computed by subroutine pp_calc
C     MYviscAr - Vertical eddy viscosity coefficient                (m^2/s)
C     MYdiffKr - Vertical diffusion coefficient for heat,
C                salt and tracers                                   (m^2/s)
C
C-----------------------------------------------------------------------
C \ev
CEOP

C     Magic parameters of Mellor&Yamada(1982):
C     (M. Satoh, p.315)
      _RL A1, A2, B1, B2, C1
      PARAMETER( A1 = 0.92D0 )
      PARAMETER( A2 = 0.74D0 )
      PARAMETER( B1 = 16.6D0 )
      PARAMETER( B2 = 10.1D0 )
      PARAMETER( C1 = 0.08D0 )

      _RL    alpha1, alpha2
      _RL    beta1, beta2, beta3, beta4
      _RL    RiMax
      _RL    MYhblScale
      _RL    MYviscMax, MYdiffMax
      _RL    MYdumpFreq
      COMMON /MY_PARMS_R/
     &     alpha1, alpha2, beta1, beta2, beta3, beta4,
     &     RiMax, MYhblScale,
     &     MYviscMax, MYdiffMax,
     &     MYdumpFreq

      _RL MYhbl    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)
      _RL MYviscAr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL MYdiffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /MY_FIELDS/ MYhbl, MYviscAr, MYdiffKr

      LOGICAL MYisOn, MYmixingMaps, MYwriteState
      COMMON /MY_PARMS_L/
     &     MYisOn, MYmixingMaps, MYwriteState

#endif /* ALLOW_MY82 */
