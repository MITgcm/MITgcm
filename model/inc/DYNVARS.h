C $Header: /u/gcmpack/MITgcm/model/inc/DYNVARS.h,v 1.17 2001/09/21 03:54:36 cnh Exp $
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
C     gX, gXNM1 - Time tendencies at current and prvious time levels.
C     uVelD  - D grid zonal velocity
C     vVelD  - D grid meridional velocity
C     etaH   - surface r-anomaly, advanced in time consistently 
C              with 2.D flow divergence (Exact-Conservation): 
C                etaH^n+1 = etaH^n - delta_t*Div.(H^n U^n)   
C  note: a) used with "exactConserv" but strictly necessary for NonLinFreeSurf
C        b) same as etaN but not necessarely at the same time, e.g.:
C           implicDiv2DFlow=0 => etaH=etaN ; =1 => etaH=etaNm1 ;

      COMMON /DYNVARS_R/
     &                   etaN, etaH,
     &                   uVel,vVel,wVel,theta,salt,
     &                   gu,gv,gt,gs,guNm1,gvNm1,gtNm1,gsNm1
      _RL  etaN  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  etaH  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  guNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gvNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gtNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gsNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef INCLUDE_CD_CODE
      COMMON /DYNVARS_CD/ 
     &                   uVelD, vVelD,
     &                   etaNm1,
     &                   uNM1,  vNM1,
     &                   guCD, gvCD
      _RL  uVeld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVeld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  etaNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uNm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vNm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  guCD  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gvCD  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_NONHYDROSTATIC
      COMMON /DYNVARS_NH/ phi_nh
      _RL  phi_nh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_NONHYDROSTATIC */

#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
      COMMON /DYNVARS_DIFFKR/
     &                       diffKr
      _RL  diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPGM_CONTROL))
      COMMON /DYNVARS_KAPGM/
     &                       kapgm
      _RL  kapgm  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
