C $Header: /u/gcmpack/MITgcm/model/inc/DYNVARS.h,v 1.31 2007/04/14 18:32:32 dimitri Exp $
C $Name:  $

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
C
C     etaN  - free-surface r-anomaly (r unit) at current time level
C     uVel  - zonal velocity (m/s, i=1 held at western face)
C     vVel  - meridional velocity (m/s, j=1 held at southern face)
C     theta - potential temperature (oC, held at pressure/tracer point)
C     salt  - salinity (ppt, held at pressure/tracer point)
C     gX, gxNm1 - Time tendencies at current and previous time levels.
C     etaH   - surface r-anomaly, advanced in time consistently 
C              with 2.D flow divergence (Exact-Conservation): 
C                etaH^n+1 = etaH^n - delta_t*Div.(H^n U^n)   
C  note: a) used with "exactConserv" but strictly necessary for NonLinFreeSurf
C        b) same as etaN but not necessarely at the same time, e.g.:
C           implicDiv2DFlow=0 => etaH=etaN ; =1 => etaH=etaNm1 ;

#ifdef ALLOW_ADAMSBASHFORTH_3
      COMMON /DYNVARS_R/
     &                   etaN,
     &                   uVel,vVel,wVel,theta,salt,
     &                   gU,   gV,   gT,   gS,
     &                   guNm, gvNm, gtNm, gsNm
#else /* ALLOW_ADAMSBASHFORTH_3 */
      COMMON /DYNVARS_R/
     &                   etaN,
     &                   uVel,vVel,wVel,theta,salt,
     &                   gU,   gV,   gT,   gS,
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
      _RL  gT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
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

      COMMON /DYNVARS_R_2/
     &                   etaH
      _RL  etaH  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

cph(
cph the following block will eventually move to a separate
cph header file containing requires anomaly fields of control vars.
cph
#if (defined ALLOW_3D_DIFFKR || \
     (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL)))
C     diffKr :: full 3D specification of Laplacian diffusion coeff.
C               for mixing of tracers vertically ( units of r^2/s )
      COMMON /DYNVARS_DIFFKR/
     &                       diffKr
      _RL  diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPGM_CONTROL))
      COMMON /DYNVARS_KAPGM/
     &                       kapgm
      _RL  kapgm  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_BOTTOMDRAG_CONTROL))
      COMMON /DYNVARS_BOTTOMDRAG/
     &                       bottomdragfld
      _RL  bottomdragfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
cph
cph)

#ifdef ALLOW_BL79_LAT_VARY
C     BL79LatArray :: is used for latitudinal dependence of
C                     BryanLewis79 vertical diffusivity
      _RL BL79LatArray (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

C     diagnostic variables: 
C     phiHydLow  :: Phi-Hydrostatic at r-lower boundary
C                  (bottom in z-coordinates, top in p-coordinates)
C     totPhiHyd :: total hydrostatic Potential (anomaly, for now), 
C                  at cell center level ; includes surface contribution.
C                 (for diagnostic + used in Z-coord with EOS_funct_P)
C     IVDConvCount :: Impl.Vert.Diffusion convection counter:
C                   = 0 (not convecting) or 1 (convecting)
      COMMON /DYNVARS_DIAG/ phiHydLow, totPhiHyd, IVDConvCount
      _RL  phiHydLow(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  totPhiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  IVDConvCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
	
