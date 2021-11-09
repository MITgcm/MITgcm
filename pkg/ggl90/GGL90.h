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
C     mxlSurfFlag     - Flag to force mixing near ocean surface (default= F )
C     calcMeanVertShear :: calculate the mean (@ grid-cell center) of vertical
C                          shear compon. (instead of vert. shear of mean flow);
C                          also applies to surface stress (uStarSquare)
C
C     useIDEMIX       - turn on internal wave mixing contribution modeled by
C                       IDEMIX version 1:
C                     - Olbers, D. and Eden, C. (2013), J. Phys. Oceano.
C                       doi:10.1175/JPO-D-12-0207.1
C
C     GGL90dumpFreq   - analogue of dumpFreq (= default)
C     GGL90mixingMaps - output to standard out (default = .FALSE.)
C     GGL90writeState - output to files        (default = .FALSE.)
C
C Time varying parameters computed by subroutine ggl90_calc, defined at
C interface (wVel-like)
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
      _RL    GGL90dumpFreq
      INTEGER mxlMaxFlag
      COMMON /GGL90_PARMS_R/
     &     GGL90ck, GGL90ceps,
     &     GGL90alpha, GGL90m2,
     &     GGL90diffTKEh,
     &     GGL90mixingLengthMin,
     &     GGL90TKEmin, GGL90TKEsurfMin, GGL90TKEbottom,
     &     GGL90viscMax, GGL90diffMax,
     &     GGL90dumpFreq, mxlMaxFlag

      _RL GGL90TKE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90viscArU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90viscArV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GGL90_FIELDS/ GGL90TKE,
     &     GGL90viscArU, GGL90viscArV, GGL90diffKr

      LOGICAL GGL90isOn, GGL90mixingMaps, GGL90writeState
      LOGICAL GGL90_dirichlet, mxlSurfFlag, calcMeanVertShear
      LOGICAL useIDEMIX
      COMMON /GGL90_PARMS_L/
     &     GGL90isOn, GGL90mixingMaps, GGL90writeState,
     &     GGL90_dirichlet, mxlSurfFlag, calcMeanVertShear,
     &     useIDEMIX

#ifdef ALLOW_GGL90_SMOOTH
      COMMON /GGL90_CORNER/ mskCor
      _RL mskCor(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_GGL90_IDEMIX
C-----------------------------------------------------------------------
C
C     IDEMIX parameters
C     IDEMIX_tau_v :: time scale for vertical symmetrisation (s), def: 1d
C     IDEMIX_tau_h :: time scale for horizontal symmetrisation (s), def: 10d
C     IDEMIX_gamma :: scaling factor (see Olbers and Eden 2013, App) def: 1.57
C     IDEMIX_jstar :: spectral bandwidth in modes (default=10.)
C     IDEMIX_mu0   :: dissipation parameter (wrong default=4/3, should be 1/3)
C     IDEMIX_diff_min :: minimum diffusivity, not used (def: 1e-9)
C     IDEMIX_mixing_efficiency :: used for diagnosing Osborn diff (def: 0.1666)
C     IDEMIX_diff_max :: maximum Osborn diffusivity (def: 1)
C     IDEMIX_frac_F_b :: scaling factor for  bottom forcing (def: 1)
C     IDEMIX_frac_F_s :: scaling factor for surface forcing (def: 0.2,
C                        because only 20% of the surface wind energy input is
C                        assumed to reach the interior below the mixed layer.)
C     IDEMIX_tidal_file :: file containing tidal forcing
C     IDEMIX_wind_file  :: file containing surface wind forcing
C     IDEMIX_include_GM :: include eddy contribution parameterized by GMredi
C     IDEMIX_include_GM_bottom :: include eddy contribution only at the bottom
C
C-----------------------------------------------------------------------
      _RL IDEMIX_tau_v, IDEMIX_tau_h, IDEMIX_gamma, IDEMIX_jstar
      _RL IDEMIX_mu0, IDEMIX_diff_min
      _RL IDEMIX_mixing_efficiency, IDEMIX_diff_max
      _RL IDEMIX_frac_F_b, IDEMIX_frac_F_s
C-----------------------------------------------------------------------
C
C     IDEMIX 3-d fields, defined at interfaces (wVel-like)
C     IDEMIX_E   :: total energy of internal waves
C
C     IDEMIX 2-d fields
C     IDEMIX_F_B :: internal wave energy bottom forcing field (e.g., tides)
C     IDEMIX_F_S :: IW energy surface forcing field (e.g., wind stress)
C
C-----------------------------------------------------------------------
      _RL IDEMIX_E  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL IDEMIX_F_B(1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)
      _RL IDEMIX_F_S(1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)

      COMMON /GGL90_IDEMIX_VARS/ IDEMIX_E, IDEMIX_F_b, IDEMIX_F_S

      COMMON /GGL90_IDEMIX_R/
     &     IDEMIX_tau_v, IDEMIX_tau_h, IDEMIX_gamma, IDEMIX_jstar,
     &     IDEMIX_mu0, IDEMIX_mixing_efficiency, IDEMIX_diff_max,
     &     IDEMIX_diff_min, IDEMIX_frac_F_b, IDEMIX_frac_F_s

      CHARACTER*(MAX_LEN_FNAM)
     &            IDEMIX_tidal_file, IDEMIX_wind_file
      COMMON /GLL90_IDEMIX_C/
     &            IDEMIX_tidal_file, IDEMIX_wind_file

      LOGICAL     IDEMIX_include_GM, IDEMIX_include_GM_bottom
      COMMON /GLL90_IDEMIX_L/
     &            IDEMIX_include_GM, IDEMIX_include_GM_bottom
#endif /* ALLOW_GGL90_IDEMIX */

#endif /* ALLOW_GGL90 */
