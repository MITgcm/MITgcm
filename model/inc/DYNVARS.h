!BOP
! !ROUTINE: DYNVARS.h
! !INTERFACE:
! include "DYNVARS.h"
! !DESCRIPTION:
! \bv
! *==========================================================*
! | DYNVARS.h
! | o Dynamical model variables (common block DYNVARS_R)
! *==========================================================*
! | The value and two levels of time tendency are held for
! | each prognostic variable.
! *==========================================================*
! \ev
!EOP

! State Variables:
! etaN  :: free-surface r-anomaly (r unit) at current time level
! uVel  :: zonal velocity (m/s, i=1 held at western face)
! vVel  :: meridional velocity (m/s, j=1 held at southern face)
! theta :: potential temperature (oC, held at pressure/tracer point)
! salt  :: salinity (g/kg, held at pressure/tracer point; note that
!          salinity is either a conductivity ratio or, if using TEOS10,
!          a mass ratio;here we assume it is a mass ratio even though
!          it is only correct for TEOS10)
! gX, gxNm1 :: Time tendencies at current and previous time levels.
! etaH  :: surface r-anomaly, advanced in time consistently
!          with 2.D flow divergence (Exact-Conservation):
!            etaH^n+1 = etaH^n - delta_t*Div.(H^n U^n+1)
!  note: a) used with "exactConserv", necessary for Non-Lin free-surf and mixed
!       forward/backward free-surf time stepping (e.g., Crank-Nickelson)
!    b) same as etaN but not necessarily at the same time, e.g.:
!       implicDiv2DFlow=1 => etaH=etaN ; =0 => etaH=etaN^(n-1);

#ifdef ALLOW_ADAMSBASHFORTH_3
      COMMON /DYNVARS_R/                                                          &
     &      etaN,                                                                 &
     &      uVel,vVel,wVel,theta,salt,                                            &
     &      gU,   gV,                                                             &
     &      guNm, gvNm, gtNm, gsNm
#else /* ALLOW_ADAMSBASHFORTH_3 */
      COMMON /DYNVARS_R/                                                          &
     &      etaN,                                                                 &
     &      uVel,vVel,wVel,theta,salt,                                            &
     &      gU,   gV,                                                             &
     &      guNm1,gvNm1,gtNm1,gsNm1
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      _RL  etaN  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef ALLOW_ADAMSBASHFORTH_3
      _RL  guNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL  gvNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL  gtNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL  gsNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
#else /* ALLOW_ADAMSBASHFORTH_3 */
      _RL  guNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gvNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gtNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gsNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_ADAMSBASHFORTH_3 */

#ifdef USE_OLD_EXTERNAL_FORCING
      COMMON /DYNVARS_OLD/                                                        &
     &      gT,   gS
      _RL  gT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

      COMMON /DYNVARS_R_2/                                                        &
     &      etaH
      _RL  etaH  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#if (defined (ALLOW_3D_DIFFKR) || defined (ALLOW_DIFFKR_CONTROL))
! diffKr :: full 3D specification of Laplacian diffusion coeff.
!           for mixing of tracers vertically ( units of r^2/s )
      COMMON /DYNVARS_DIFFKR/                                                     &
     &      diffKr
      _RL  diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
! smag3D_diffK :: isotropic 3D diffusivity from Smagorisky viscosity
!                 at grid-cell center (units: m^2/s )
      COMMON /DYNVARS_DIFFK_SMAG3D/ smag3D_diffK
      _RL smag3D_diffK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

!   The following blocks containing requires anomaly fields of control vars
!   and related to Options:
!   ALLOW_KAPGM_CONTROL , ALLOW_KAPREDI_CONTROL and ALLOW_BOTTOMDRAG_CONTROL
!   have been moved to header file "CTRL_FIELDS.h"

#ifdef ALLOW_BL79_LAT_VARY
! BL79LatArray :: is used for latitudinal dependence of
!                 BryanLewis79 vertical diffusivity
      COMMON /DYNVARS_BL79LatArray/ BL79LatArray
      _RL BL79LatArray (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

! Diagnostic Variables:
! rhoInSitu    :: In-Situ density anomaly [kg/m^3] at cell center level.
! totPhiHyd    :: total hydrostatic Potential (anomaly, for now),
!                 at cell center level ; includes surface contribution.
!                 (for diagnostic + used in Z-coord with EOS_funct_P)
! phiHydLow    :: Phi-Hydrostatic at r-lower boundary
!                 (bottom in z-coordinates, top in p-coordinates)
! hMixLayer    :: Mixed layer depth [m]
!                 (for diagnostic + used GMRedi "fm07")
! IVDConvCount :: Impl.Vert.Diffusion convection counter:
!                 = 0 (not convecting) or 1 (convecting)
      COMMON /DYNVARS_DIAG/                                                       &
     &      rhoInSitu,                                                            &
     &      totPhiHyd, phiHydLow,                                                 &
     &      hMixLayer, IVDConvCount
      _RL  rhoInSitu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  totPhiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  phiHydLow(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  hMixLayer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  IVDConvCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef ALLOW_LEITH_QG
! Leith QG dynamic viscosity scheme requires buoyancy frequency.
! Store sigmaRfield to avoid computing density multiple times and give
! access to all levels during the k-loop
! sigmaRfield           :: vertical gradient of buoyancy.
      COMMON /DYNVARS_sigmaR/ sigmaRfield
      _RL sigmaRfield  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_LEITH_QG */

#ifdef ALLOW_SOLVE4_PS_AND_DRAG
! Variables for Implicit friction (& vert. visc) in 2-D pressure solver
! dU_psFacX    :: zonal velocity increment factor from (implicit) surf.
!                 pressure gradient in X (no Units)
! dV_psFacY    :: merid velocity increment factor from (implicit) surf.
!                 pressure gradient in Y (no Units)

      COMMON /DYNVARS_DRAG_IN_PS/                                                 &
     &      dU_psFacX, dV_psFacY
      _RL  dU_psFacX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  dV_psFacY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_SOLVE4_PS_AND_DRAG */

#ifdef INCLUDE_SOUNDSPEED_CALC_CODE
! cSound       :: Speed of sound in seawater (m/s)
!                 following Del Grosso (1974)
      COMMON /DYNVARS_cSound/ cSound
      _RL  cSound(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* INCLUDE_SOUNDSPEED_CALC_CODE */

