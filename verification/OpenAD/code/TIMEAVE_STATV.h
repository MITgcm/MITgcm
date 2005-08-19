C $Header: /u/gcmpack/MITgcm/verification/OpenAD/code/Attic/TIMEAVE_STATV.h,v 1.1 2005/08/19 21:48:51 heimbach Exp $
C $Name:  $

#include "TIMEAVE_OPTIONS.h"

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
C     | Time average of intermediate and tendency variables is centered 
C     |  on the time step (time average interval=TimeAve_full)
C     *================================================================*
C     \ev
CEOP

C     TimeAve_*    :: time of temporal integration (s) *** for each thread ***
C     TimeAve_half :: half time_step multiple (used for state variables)
C     TimeAve_full :: full time_step multiple (used for for intermediate var.) 
C     uFluxtave    :: zonal surface wind stress (N/m^2,
C                     >0 for increase in uVel, i=1 held at western face)
C     vFluxtave    :: meridional surface wind stress (N/m^2,
C                     >0 for increase in vVel, j=1 held at southern face)
C     tFluxtave    :: net surface heat flux (W/m^2, >0 for increase in theta)
C     sFluxtave    :: net surface salt flux (g/m^2/s, >0 for increase in salt)
C     etatave      :: surface displacement (r unit, i.e. ocean:z, atmos:p)
C     uVeltave     :: zonal velocity (m/s, i=1 held at western face)
C     vVeltave     :: meridional velocity (m/s, j=1 held at southern face)
C     wVeltave     :: vertical velocity ([r]/s, i.e.: ocean:m/s atmos:Pa/s)
C     thetatave    :: potential temperature (oC, held at pressure/tracer point)
C     salttave     :: salinity (ppt, held at pressure/tracer point)
C     Eta2tave     :: eta * eta
C     TTtave       :: theta * theta
C     UUtave       :: uVel * uVel (used to compute the averaged KE)
C     VVtave       :: vVel * vVel (used to compute the averaged KE)
C     UVtave       :: uVel * vVel (at vorticity point, i.e. grid-corner)
C     KEtave       :: Kinetic Energy
C     UTtave       :: uVel * theta (* hFacW)
C     VTtave       :: vVel * theta (* hFacS)
C     WTtave       :: wVel * theta
C     UStave       :: uVel * salt (* hFacW)
C     VStave       :: vVel * salt (* hFacS)
C     WStave       :: wVel * salt
C     tDiffRtave   :: vertical diffusion flux of Temperature (theta)
C     uZetatave    :: uVel*Relativ_Vorticity_3 (computed at v point)
C     vZetatave    :: vVel*Relativ_Vorticity_3 (computed at u point)
C     phiHydtave   :: Hydrostatic (ocean) pressure / (atmos) geo- Potential
C     phiHydLowtave:: Hydrostatic (ocean) pressure / (atmos) geo- Potential
C                     at the fixed boundary: (ocean) bottom pressure
C                     (atmos) geo- Potential
C     ConvectCountTave :: Average number of convective adjustment event

      COMMON /TAVE_TIME/ TimeAve_half,TimeAve_full
      _RL TimeAve_half(Nr,nSx,nSy)
      _RL TimeAve_full(Nr,nSx,nSy)

      COMMON /TAVE_STATEVARS_r1/
     &                  uFluxtave,vFluxtave,tFluxtave,sFluxtave
     &                 ,etatave,uVeltave,vVeltave,wVeltave
     &                 ,thetatave,salttave,phiHydLowtave
#ifndef MINIMAL_TAVE_OUTPUT
      COMMON /TAVE_STATEVARS_r2/
     &                  UTtave,VTtave,WTtave,UStave,VStave,WStave
     &                 ,Eta2tave,TTtave,UUtave,VVtave,UVtave
     &                 ,TdiffRtave
#ifdef ALLOW_MOM_VECINV
      COMMON /TAVE_STATEVARS_r3/
     &                  uZetatave, vZetatave
#endif /* ALLOW_MOM_VECINV */
      COMMON /TAVE_STATEVARS_r4/
     &                  phiHydtave
     &                 ,phiHydLow2Tave
     &                 ,ConvectCountTave
#endif /* ndef MINIMAL_TAVE_OUTPUT */
      _RL  uFluxtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vFluxtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  tFluxtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sFluxtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  etatave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  thetatave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  salttave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phiHydLowtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifndef MINIMAL_TAVE_OUTPUT
      _RL  UTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  VTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  WTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  UStave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  VStave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  WStave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  eta2Tave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  UUtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  VVtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  UVtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TdiffRtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef ALLOW_MOM_VECINV
      _RL uZetatave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vZetatave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_MOM_VECINV */
      _RL phiHydtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phiHydLow2Tave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ConvectCountTave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ndef MINIMAL_TAVE_OUTPUT */

#ifdef NONLIN_FRSURF
C     hUtave       :: average zonal flow (=hFacW*uVel) (still in m/s !)
C     hVtave       :: average merid.flow (=hFacS*vVel) (still in m/s !)
C     hFacCtave    :: average thickness fraction of open water, Center
C     hFacWtave    :: average thickness fraction of open water, West side
C     hFacStave    :: average thickness fraction of open water, South side

#ifndef MINIMAL_TAVE_OUTPUT
      COMMON /TAVE_THICKNESS/ 
     &              hUtave, hVtave
c    &            , hFacCtave, hFacWtave, hFacStave 
      _RL  hUtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  hVtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL  hFacCtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL  hFacWtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL  hFacStave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ndef MINIMAL_TAVE_OUTPUT */
#endif /* NONLIN_FRSURF */

#endif /* ALLOW_TIMEAVE */ 
