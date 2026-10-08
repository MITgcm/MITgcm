!
!BOP
!    !ROUTINE: SURFACE.h
!    !INTERFACE:
!    include SURFACE.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | SURFACE.h
! | o Header file defining surface-related model variables
! *==========================================================*
! | Contains variables relative to the surface position
! | that are held fixed in linear free-surface formulation
! | but can vary with time with a non-linear free-surface.
! *==========================================================*
! \ev
!EOP

!--   COMMON /SURF_FIXED/  fixed surface arrays (Real)
! Bo_surf  :: Buoyancy|1/rho [ocean|atmos] at surface level [=g|alpha(p_o)]
! recip_Bo :: 1/Bo_surf
! topoZ    :: topographic height [m] (used mainly for atmosphere)
! phi0surf :: starting point for integrating phi_Hyd
      COMMON /SURF_FIXED/ Bo_surf, recip_Bo, topoZ, phi0surf
      _RL  Bo_surf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  recip_Bo(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  topoZ   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  phi0surf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

!--   COMMON /SURF_CORREC/ Common block for correction of source/sink of
!--                        Tracer due to W at the surface with Linear
!--                        Free Surface
! TsurfCor :: Pot.Temp Linear-Free-Surface correction term [K.r_Unit/s]
! SsurfCor :: Salinity Linear-Free-Surface correction term [g/kg.r_Unit/s]
      COMMON /SURF_CORREC/ TsurfCor, SsurfCor
      _RL TsurfCor
      _RL SsurfCor

!--   COMMON /ETA_UPDATES/ variables related to Eta updates (with exactConserv)
! etaHnm1 :: surface r-anomaly, etaH, at previous time level
! dEtaHdt :: time derivative of total column height [r_unit/s = w unit]
! PmEpR   :: keep the fresh water input (=-EmPmR) of the previous time step
      COMMON /ETA_UPDATES/ etaHnm1, dEtaHdt, PmEpR
      _RL etaHnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dEtaHdt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL PmEpR  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef NONLIN_FRSURF
!--   COMMON /SURF_CHANGE/ transient variables used for Non-Lin Free-Surf
! hFac_surfC ::  New thickness factor of the surface level
!                    center (Tracer point)
! hFac_surfW ::  idem, West  interface (U point)
! hFac_surfS ::  idem, South interface (V point)
! hFac_surfNm1C, etc. :: prior values
      COMMON /SURF_CHANGE/                                                        &
     &      hFac_surfC, hFac_surfW, hFac_surfS,                                   &
     &      hFac_surfNm1C, hFac_surfNm1W, hFac_surfNm1S
      _RS  hFac_surfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfNm1C(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfNm1W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfNm1S(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

! Local variables in common block
! Rmin_surf :: minimum r_value of the free surface position
!              that satisfy  the hFacInf criteria
      COMMON /LOCAL_CALC_SURF_DR/ Rmin_surf
      _RL Rmin_surf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

!--   COMMON /RSTAR_CHANGE/ transient variables used with r* Coordinate
! rStarFacC :: = dr/dr* = ratio of r-thickness / r*-thickness = h^n / H
! rStarFacW :: same but for West  face
! rStarFacS :: same but for South face
! pStarFacK :: rStarFacC**atm_kappa (for atmosphere in p* coords)
! rStarFacNm1C, etc. :: prior values
! rStarExpC :: column expansion factor = h^n+1/h^n , Centered
! rStarExpW :: column expansion factor = h^n+1/h^n , Western  face
! rStarExpS :: column expansion factor = h^n+1/h^n , Southern face
! rStarDhCDt:: relative time derivative of h_Center = d.eta/dt / H
! rStarDhWDt:: relative time derivative of h_West_face  (u.point)
! rStarDhSDt:: relative time derivative of h_South_face (v.point)
      COMMON /RSTAR_CHANGE/                                                       &
     &      rStarFacC, rStarFacW, rStarFacS, pStarFacK,                           &
     &      rStarFacNm1C, rStarFacNm1W, rStarFacNm1S,                             &
     &      rStarExpC, rStarExpW, rStarExpS,                                      &
     &      rStarDhCDt,rStarDhWDt,rStarDhSDt
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

!--   COMMON /RSTAR_FIXED/ fixed thickness ratio ( r* discretization )
! h0FacC :: initial (and fixed in time) hFacC factor
! h0FacW :: initial (and fixed in time) hFacW factor
! h0FacS :: initial (and fixed in time) hFacS factor
      COMMON /RSTAR_FIXED/                                                        &
     &      h0FacC, h0FacW, h0FacS
      _RS h0FacC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS h0FacW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS h0FacS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

!--   COMMON /SIGMA_CHANGE/ transient variables used with r* Coordinate
! etaHw    :: surface r-anomaly (etaH) at Western  edge (U location)
! etaHs    :: surface r-anomaly (etaH) at Southern edge (V location)
! dEtaWdt  :: time derivative of etaH at Western  edge (U location)
! dEtaSdt  :: time derivative of etaH at Southern edge (V location)
      COMMON /SIGMA_CHANGE/                                                       &
     &      etaHw, etaHs,                                                         &
     &      dEtaWdt, dEtaSdt
      _RL  etaHw  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  etaHs  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  dEtaWdt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  dEtaSdt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* NONLIN_FRSURF */
