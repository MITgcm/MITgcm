C $Header: /u/gcmpack/MITgcm/pkg/ggl90/GGL90.h,v 1.6 2012/08/08 22:22:42 gforget Exp $
C $Name:  $

#ifdef ALLOW_GGL90

CBOP
C !ROUTINE: GGL90.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | GGL90.h                                                  |
C     | o Basic header for Garpar et al. (1990)                  |
C     |   vertical mixing parameterization. Implementation       |
C     |   follows OPA implementation of Blanke+Delecuse (1993)   |
C     |   Contains all GGL90 field declarations.                 |
C     \==========================================================/

C-----------------------------------------------------------------------
C
C Parameters that can be set in data.ggl90
C     unless otherwise noted, equation numbers refer to Gaspar et al. 
C     (1990), JGR, 95(C9) pp. 16,179ff.
C     
C     GGL90ck         - constant in viscosity coefficient (eq.10)
C     GGL90ceps       - dissipation constant according to Kolmogorov (1942)
C     GGL90m2         - constant relating wind stress to vertical stress of TKE
C                      in K [d(TKE)/dz](0)=m2*ustar^3
C     GGL90alpha      - constant relating viscosity to GGL90 diffusivity
C                      (default=1 in Gaspar etal.)
C     GGL90TKEsurfmin - minimum of surface kinetic energy boundary condition
C                      (default=GGL90TKEmin)
C     GGL90TKEmin     - minimum kinetic energy, leads to minimum mixing if TKE=0.
C     GGL90TKEbottom  - bottom bounardy condition for kinetic energy 
C                      (default=GGL90TKEmin)
C     GGL90TKEFile    - File with initial field of TKE
C     GGL90mixingLengthMin - Mininum mixing length
C     mxlMaxFlag      - Flag for limiting mixing-length method (default=0)
C     mxlSurfFlag     - Flag to force mixing near ocean surface (default=.FALSE.)
C
C     GGL90dumpFreq   - analogue of dumpFreq (= default)
C     GGL90taveFreq   - analogue of taveFreq (= default)
C     GGL90mixingMaps - output to standard out (default = .FALSE.)
C     GGL90writeState - output to files        (default = .FALSE.)
C
C Time varying parameters computed by subroutine ggl90_calc
C     GGL90TKE    - prognostic variable stepped forward in time ((m/s)^2)
C     GGL90viscAr - Vertical eddy viscosity coefficient         (m^2/s)
C     GGL90diffKr - Vertical diffusion coefficient for heat, 
C                salt and tracers                            (m^2/s)
C
C-----------------------------------------------------------------------
C \ev
CEOP
      _RL    SQRTTWO
      PARAMETER ( SQRTTWO = 1.41421356237310D0 )
      _RL    GGL90eps
      PARAMETER ( GGL90eps = 2.23D-16 )

      CHARACTER*(MAX_LEN_FNAM) GGL90TKEFile
      COMMON /GGL90_PARMS_C/ GGL90TKEFile
     

      _RL    GGL90ck, GGL90ceps
      _RL    GGL90alpha, GGL90m2
      _RL    GGL90diffTKEh
      _RL    GGL90mixingLengthMin
      _RL    GGL90TKEmin, GGL90TKEsurfMin, GGL90TKEbottom
      _RL    GGL90viscMax, GGL90diffMax
      _RL    GGL90dumpFreq, GGL90taveFreq
      INTEGER mxlMaxFlag
      COMMON /GGL90_PARMS_R/  
     &     GGL90ck, GGL90ceps,
     &     GGL90alpha, GGL90m2,
     &     GGL90diffTKEh,
     &     GGL90mixingLengthMin,
     &     GGL90TKEmin, GGL90TKEsurfMin, GGL90TKEbottom,
     &     GGL90viscMax, GGL90diffMax,
     &     GGL90dumpFreq, GGL90taveFreq, mxlMaxFlag

      _RL GGL90TKE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90viscArU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90viscArV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c      _RL recip_hFacI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GGL90_FIELDS/ GGL90TKE,
     &     GGL90viscArU, GGL90viscArV, GGL90diffKr
c     &     ,recip_hFacI

      LOGICAL GGL90isOn, GGL90mixingMaps, GGL90writeState
      LOGICAL mxlSurfFlag
      COMMON /GGL90_PARMS_L/
     &     GGL90isOn, GGL90mixingMaps, GGL90writeState,
     &     mxlSurfFlag

#ifdef ALLOW_GGL90_SMOOTH
      COMMON /GGL90_CORNER/ mskCor
      _RL mskCor(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#endif /* ALLOW_GGL90 */
