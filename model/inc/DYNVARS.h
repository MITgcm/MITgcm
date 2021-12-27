CBOP
C     !ROUTINE: DYNVARS.h
C     !INTERFACE:
C     include "DYNVARS.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | DYNVARS.h
C     | o Dynamical model variables (common block DYNVARS_R)
C     *==========================================================*
C     | The value and two levels of time tendency are held for
C     | each prognostic variable.
C     *==========================================================*
C     \ev
CEOP

C     State Variables:
C     etaN  :: free-surface r-anomaly (r unit) at current time level
C     uVel  :: zonal velocity (m/s, i=1 held at western face)
C     vVel  :: meridional velocity (m/s, j=1 held at southern face)
C     theta :: potential temperature (oC, held at pressure/tracer point)
C     salt  :: salinity (g/kg, held at pressure/tracer point; note that
C              salinity is either a conductivity ratio or, if using TEOS10,
C              a mass ratio;here we assume it is a mass ratio even though
C              it is only correct for TEOS10)
C     gX, gxNm1 :: Time tendencies at current and previous time levels.
C     etaH  :: surface r-anomaly, advanced in time consistently
C              with 2.D flow divergence (Exact-Conservation):
C                etaH^n+1 = etaH^n - delta_t*Div.(H^n U^n+1)
C  note: a) used with "exactConserv", necessary for Non-Lin free-surf and mixed
C           forward/backward free-surf time stepping (e.g., Crank-Nickelson)
C        b) same as etaN but not necessarily at the same time, e.g.:
C           implicDiv2DFlow=1 => etaH=etaN ; =0 => etaH=etaN^(n-1);

#ifdef ALLOW_ADAMSBASHFORTH_3
      COMMON /DYNVARS_R/
     &                   etaN,
     &                   uVel,vVel,wVel,theta,salt,
     &                   gU,   gV,
     &                   guNm, gvNm, gtNm, gsNm
#else /* ALLOW_ADAMSBASHFORTH_3 */
      COMMON /DYNVARS_R/
     &                   etaN,
     &                   uVel,vVel,wVel,theta,salt,
     &                   gU,   gV,
     &                   guNm1,gvNm1,gtNm1,gsNm1
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
      COMMON /DYNVARS_OLD/
     &                   gT,   gS
      _RL  gT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

      COMMON /DYNVARS_R_2/
     &                   etaH
      _RL  etaH  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#if (defined (ALLOW_3D_DIFFKR) || defined (ALLOW_DIFFKR_CONTROL))
C     diffKr :: full 3D specification of Laplacian diffusion coeff.
C               for mixing of tracers vertically ( units of r^2/s )
      COMMON /DYNVARS_DIFFKR/
     &                       diffKr
      _RL  diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
C     smag3D_diffK :: isotropic 3D diffusivity from Smagorisky viscosity
C                     at grid-cell center (units: m^2/s )
      COMMON /DYNVARS_DIFFK_SMAG3D/ smag3D_diffK
      _RL smag3D_diffK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

C   The following blocks containing requires anomaly fields of control vars
C   and related to Options:
C   ALLOW_KAPGM_CONTROL , ALLOW_KAPREDI_CONTROL and ALLOW_BOTTOMDRAG_CONTROL
C   have been moved to header file "CTRL_FIELDS.h"

#ifdef ALLOW_BL79_LAT_VARY
C     BL79LatArray :: is used for latitudinal dependence of
C                     BryanLewis79 vertical diffusivity
      COMMON /DYNVARS_BL79LatArray/ BL79LatArray
      _RL BL79LatArray (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

C     Diagnostic Variables:
C     rhoInSitu    :: In-Situ density anomaly [kg/m^3] at cell center level.
C     totPhiHyd    :: total hydrostatic Potential (anomaly, for now),
C                     at cell center level ; includes surface contribution.
C                     (for diagnostic + used in Z-coord with EOS_funct_P)
C     phiHydLow    :: Phi-Hydrostatic at r-lower boundary
C                     (bottom in z-coordinates, top in p-coordinates)
C     hMixLayer    :: Mixed layer depth [m]
C                     (for diagnostic + used GMRedi "fm07")
C     IVDConvCount :: Impl.Vert.Diffusion convection counter:
C                     = 0 (not convecting) or 1 (convecting)
      COMMON /DYNVARS_DIAG/
     &                rhoInSitu,
     &                totPhiHyd, phiHydLow,
     &                hMixLayer, IVDConvCount
      _RL  rhoInSitu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  totPhiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  phiHydLow(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  hMixLayer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  IVDConvCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#if (defined (ALLOW_SIGMAR_COST_CONTRIBUTION) || defined (ALLOW_LEITH_QG))
C     Leith QG dynamic viscosity scheme requires buoyancy frequency.
C     ECCO sometimes uses sigmaR (with ALLOW_SIGMAR_COST_CONTRIBUTION).
C     Store sigmaRfield to avoid computing density multiple times and give
C     access to all levels during the k-loop
C     sigmaRfield           :: vertical gradient of buoyancy.
      COMMON /DYNVARS_sigmaR/ sigmaRfield
      _RL sigmaRfield  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_SIGMAR_COST_CONTRIBUTION or ALLOW_LEITH_QG */

#ifdef ALLOW_SOLVE4_PS_AND_DRAG
C     Variables for Implicit friction (& vert. visc) in 2-D pressure solver
C     dU_psFacX    :: zonal velocity increment factor from (implicit) surf.
C                     pressure gradient in X (no Units)
C     dV_psFacY    :: merid velocity increment factor from (implicit) surf.
C                     pressure gradient in Y (no Units)

      COMMON /DYNVARS_DRAG_IN_PS/
     &                dU_psFacX, dV_psFacY
      _RL  dU_psFacX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  dV_psFacY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_SOLVE4_PS_AND_DRAG */

#ifdef INCLUDE_SOUNDSPEED_CALC_CODE
C     cSound       :: Speed of sound in seawater (m/s)
C                     following Del Grosso (1974)
      COMMON /DYNVARS_cSound/ cSound
      _RL  cSound(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* INCLUDE_SOUNDSPEED_CALC_CODE */

