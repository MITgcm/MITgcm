C $Header: /u/gcmpack/MITgcm/pkg/timeave/TIMEAVE_STATV.h,v 1.3 2002/01/03 16:25:44 jmc Exp $
C $Name:  $

#ifdef ALLOW_TIMEAVE

CBOP
C     !ROUTINE: TIMEAVE_STATV.h
C     !INTERFACE:
C     include "TIMEAVE_STATV.h"
C     !DESCRIPTION: \bw
C     *================================================================*
C     | TIMEAVE_STATV.h
C     | o Time averages of model state-variables 
C     |   (common block TAVE_STATEVARS)
C     *================================================================*
C     | Time average of state variables is (generally) centered on the
C     |  middle of the time step (time average interval = TimeAve_half)
C     | Time average of intermediate and tandancy variables is centered 
C     |  on the time step (time average interval=TimeAve_full)
C     *================================================================*
C     \ev
CEOP

C     TimeAve_*    :: time of temporal integration (s) *** for each thread ***
C     TimeAve_half :: half time_step multiple (used for state variables)
C     TimeAve_full :: full time_step multiple (used for for intermediate var.) 
C     etatave      :: surface displacement (r unit, i.e. ocean:z, atmos:p)
C     uVeltave     :: zonal velocity (m/s, i=1 held at western face)
C     vVeltave     :: meridional velocity (m/s, j=1 held at southern face)
C     wVeltave     :: vertical velocity ([r]/s, i.e.: ocean:m/s atmos:Pa/s)
C     thetatave    :: potential temperature (oC, held at pressure/tracer point)
C     salttave     :: salinity (ppt, held at pressure/tracer point)
C     Eta2tave     ::  eta * eta
C     TTtave       :: theta * theta
C     UUtave       :: uVel * uVel (used to compute the averaged KE)
C     VVtave       :: vVel * vVel (used to compute the averaged KE)
C     KEtave       :: Kinetic Energy
C     UTtave       :: uVel * theta (* hFacW)
C     VTtave       :: vVel * theta (* hFacS)
C     WTtave       :: wVel * theta
C     phiHydtave   :: Hydrostatic (ocean) pressure / (atmos) geo- Potential
C     ConvectCountTave :: Average number of convective adjustment event

      COMMON /TAVE_TIME/ TimeAve_half,TimeAve_full
      _RL TimeAve_half(Nr,nSx,nSy)
      _RL TimeAve_full(Nr,nSx,nSy)

      COMMON /TAVE_STATEVARS/ 
     &                  etatave,Eta2tave,
     &                  uVeltave,vVeltave,wVeltave,
     &                  thetatave,salttave,
     &                  TTtave,UUtave,VVtave,
     &                  UTtave,VTtave,WTtave,
     &                  phiHydtave,ConvectCountTave
c    &                 ,KEtave
      _RL  etatave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  eta2Tave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  thetatave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  salttave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  TTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  UUtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  VVtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL  KEtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  UTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  VTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  WTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phiHydtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL ConvectCountTave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef NONLIN_FRSURF
C     hUtave       :: average zonal flow (=hFacW*uVel) (still in m/s !)
C     hVtave       :: average merid.flow (=hFacS*vVel) (still in m/s !)
C     hFacCtave    :: average thickness fraction of open water, Center
C     hFacWtave    :: average thickness fraction of open water, West side
C     hFacStave    :: average thickness fraction of open water, South side

      COMMON /TAVE_THICKNESS/ 
     &              hUtave, hVtave
c    &            , hFacCtave, hFacWtave, hFacStave 
      _RL  hUtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  hVtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL  hFacCtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL  hFacWtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL  hFacStave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* NONLIN_FRSURF */

#endif /* ALLOW_TIMEAVE */ 
