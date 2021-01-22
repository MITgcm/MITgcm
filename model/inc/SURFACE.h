C
CBOP
C    !ROUTINE: SURFACE.h
C    !INTERFACE:
C    include SURFACE.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | SURFACE.h
C     | o Header file defining surface-related model variables
C     *==========================================================*
C     | Contains variables relative to the surface position
C     | that are held fixed in linear free-surface formulation
C     | but can vary with time with a non-linear free-surface.
C     *==========================================================*
C     \ev
CEOP

C--   COMMON /SURF_FIXED/  fixed surface arrays (Real)
C     Bo_surf  :: Buoyancy|1/rho [ocean|atmos] at surface level [=g|alpha(p_o)]
C     recip_Bo :: 1/Bo_surf
C     topoZ    :: topographic height [m] (used mainly for atmosphere)
C     phi0surf :: starting point for integrating phi_Hyd
      COMMON /SURF_FIXED/ Bo_surf, recip_Bo, topoZ, phi0surf
      _RL  Bo_surf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  recip_Bo(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  topoZ   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  phi0surf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   COMMON /SURF_CORREC/ Common block for correction of source/sink of
C--                        Tracer due to W at the surface with Linear
C--                        Free Surface
C     TsurfCor :: Pot.Temp Linear-Free-Surface correction term [K.r_Unit/s]
C     SsurfCor :: Salinity Linear-Free-Surface correction term [g/kg.r_Unit/s]
      COMMON /SURF_CORREC/ TsurfCor, SsurfCor
      _RL TsurfCor
      _RL SsurfCor

#ifdef EXACT_CONSERV
C     etaHnm1 :: surface r-anomaly, etaH, at previous time level
C     dEtaHdt :: time derivative of total column height [r_unit/s = w unit]
C     PmEpR   :: keep the fresh water input (=-EmPmR) of the previous time step
      COMMON /EXACT_ETA_LOCAL/ etaHnm1, dEtaHdt, PmEpR
      _RL etaHnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dEtaHdt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  PmEpR (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef NONLIN_FRSURF
C--   COMMON /SURF_CHANGE/ transient variables used for Non-Lin Free-Surf
C     hFac_surfC ::  New thickness factor of the surface level
C                        center (Tracer point)
C     hFac_surfW ::  idem, West  interface (U point)
C     hFac_surfS ::  idem, South interface (V point)
C     hFac_surfNm1C, etc. :: prior values
      COMMON /SURF_CHANGE/
     &     hFac_surfC, hFac_surfW, hFac_surfS,
     &     hFac_surfNm1C, hFac_surfNm1W, hFac_surfNm1S
      _RS  hFac_surfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfNm1C(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfNm1W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfNm1S(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     Local variables in common block
C     Rmin_surf :: minimum r_value of the free surface position
C                  that satisfy  the hFacInf criteria
      COMMON /LOCAL_CALC_SURF_DR/ Rmin_surf
      _RL Rmin_surf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   COMMON /RSTAR_CHANGE/ transient variables used with r* Coordinate
C     rStarFacC :: = dr/dr* = ratio of r-thickness / r*-thickness = h^n / H
C     rStarFacW :: same but for West  face
C     rStarFacS :: same but for South face
C     pStarFacK :: rStarFacC**atm_kappa (for atmosphere in p* coords)
C     rStarFacNm1C, etc. :: prior values
C     rStarExpC :: column expansion factor = h^n+1/h^n , Centered
C     rStarExpW :: column expansion factor = h^n+1/h^n , Western  face
C     rStarExpS :: column expansion factor = h^n+1/h^n , Southern face
C     rStarDhCDt:: relative time derivative of h_Center = d.eta/dt / H
C     rStarDhWDt:: relative time derivative of h_West_face  (u.point)
C     rStarDhSDt:: relative time derivative of h_South_face (v.point)
      COMMON /RSTAR_CHANGE/
     &     rStarFacC, rStarFacW, rStarFacS, pStarFacK,
     &     rStarFacNm1C, rStarFacNm1W, rStarFacNm1S,
     &     rStarExpC, rStarExpW, rStarExpS,
     &     rStarDhCDt,rStarDhWDt,rStarDhSDt
      _RL  rStarFacC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarFacW (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarFacS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  pStarFacK (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarFacNm1C (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarFacNm1W (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarFacNm1S (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarExpC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarExpW (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarExpS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarDhCDt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarDhWDt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rStarDhSDt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   COMMON /RSTAR_FIXED/ fixed thickness ratio ( r* discretization )
C     h0FacC :: initial (and fixed in time) hFacC factor
C     h0FacW :: initial (and fixed in time) hFacW factor
C     h0FacS :: initial (and fixed in time) hFacS factor
      COMMON /RSTAR_FIXED/
     & h0FacC, h0FacW, h0FacS
      _RS h0FacC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS h0FacW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS h0FacS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C--   COMMON /SIGMA_CHANGE/ transient variables used with r* Coordinate
C     etaHw    :: surface r-anomaly (etaH) at Western  edge (U location)
C     etaHs    :: surface r-anomaly (etaH) at Southern edge (V location)
C     dEtaWdt  :: time derivative of etaH at Western  edge (U location)
C     dEtaSdt  :: time derivative of etaH at Southern edge (V location)
      COMMON /SIGMA_CHANGE/
     &  etaHw, etaHs,
     &  dEtaWdt, dEtaSdt
      _RL  etaHw  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  etaHs  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  dEtaWdt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  dEtaSdt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* NONLIN_FRSURF */
