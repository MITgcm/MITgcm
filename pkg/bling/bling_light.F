C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_light.F,v 1.1 2016/05/19 20:29:26 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_LIGHT(
     I               mld,
     U               irr_inst, irr_eff,
     I               bi, bj, imin, imax, jmin, jmax,
     I               myIter, myTime, myThid )
     
C     =================================================================
C     | subroutine bling_light
C     | o calculate effective light for phytoplankton growth
C     |   There are multiple types of light.
C     | - irr_inst is the instantaneous irradiance field.
C     | - irr_mix is the same, but with the irr_inst averaged throughout   
C     |   the mixed layer. This quantity is intended to represent the 
C     |   light to which phytoplankton subject to turbulent transport in 
C     |   the mixed-layer would be exposed.
C     | - irr_mem is a temporally smoothed field carried between 
C     |   timesteps, to represent photoadaptation.
C     | - irr_eff is the effective irradiance for photosynthesis, 
C     |   given either by irr_inst or irr_mix, depending on model
C     |   options and location.
C     =================================================================

      implicit none
      
C     === Global variables ===
C     irr_inst      :: Instantaneous irradiance
C     irr_mem       :: Phyto irradiance memory

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "BLING_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_AUTODIFF
# include "tamc.h"
#endif

C     === Routine arguments ===
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation domain: 1rst index range
C     jMin,jMax     :: computation domain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
C     === Input ===
      _RL mld       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     === Output ===
C      irr_inst     :: instantaneous light 
C      irr_eff      :: effective light for photosynthesis
      _RL irr_inst  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irr_eff   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C     === Local variables ===
      _RL solar, albedo
      _RL dayfrac, yday, delta
      _RL lat, sun1, dayhrs
      _RL cosz, frac, fluxi
      _RL atten
      _RL irr_surf  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ML_MEAN_LIGHT      
      _RL irr_mix   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SumMLIrr
      _RL tmp_ML
#endif
#ifndef READ_PAR
#ifndef USE_QSW
      _RL sfac      (1-OLy:sNy+OLy)
#endif
#endif
       integer i,j,k
CEOP

       DO k=1,Nr
        DO j=jmin,jmax
          DO i=imin,imax
              irr_eff(i,j,k)        = 0. _d 0
          ENDDO
        ENDDO
       ENDDO

c ---------------------------------------------------------------------
c  Surface insolation

#ifndef USE_EXFQSW
c  From pkg/dic/dic_insol
c  find light as function of date and latitude
c  based on paltridge and parson

      solar  = 1360. _d 0   !solar constant
      albedo = 0.6 _d 0     !planetary albedo

C     Case where a 2-d output array is needed: for now, stop here.
      IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
       STOP 'ABNORMAL END: S/R INSOL: 2-D output not implemented'
      ENDIF

C find day (****NOTE for year starting in winter*****)
        dayfrac=mod(myTime,360. _d 0*86400. _d 0)
     &                    /(360. _d 0*86400. _d 0)  !fraction of year
        yday = 2. _d 0*PI*dayfrac                    !convert to radians
        delta = (0.006918 _d 0
     &         -(0.399912 _d 0*cos(yday))            !cosine zenith angle
     &         +(0.070257 _d 0*sin(yday))            !(paltridge+platt)
     &         -(0.006758 _d 0*cos(2. _d 0*yday))
     &         +(0.000907 _d 0*sin(2. _d 0*yday))
     &         -(0.002697 _d 0*cos(3. _d 0*yday))
     &         +(0.001480 _d 0*sin(3. _d 0*yday)) )
       DO j=1-OLy,sNy+OLy
C latitude in radians
          lat=YC(1,j,1,bj)*deg2rad
C     latitute in radians, backed out from coriolis parameter
C     (makes latitude independent of grid)
          IF ( usingCartesianGrid .OR. usingCylindricalGrid )
     &         lat = asin( fCori(1,j,1,bj)/(2. _d 0*omega) )
          sun1 = -sin(delta)/cos(delta) * sin(lat)/cos(lat)
          IF (sun1.LE.-0.999 _d 0) sun1=-0.999 _d 0
          IF (sun1.GE. 0.999 _d 0) sun1= 0.999 _d 0
          dayhrs = abs(acos(sun1))
          cosz = ( sin(delta)*sin(lat)+              !average zenith angle
     &            (cos(delta)*cos(lat)*sin(dayhrs)/dayhrs) )
          IF (cosz.LE.5. _d -3) cosz= 5. _d -3
          frac = dayhrs/PI                           !fraction of daylight in day
C daily average photosynthetically active solar radiation just below surface
         fluxi = solar*(1. _d 0-albedo)*cosz*frac*parfrac

C convert to sfac
          sfac(j) = MAX(1. _d -5,fluxi)
       ENDDO !j

#endif

c ---------------------------------------------------------------------
c  instantaneous light, mixed layer averaged light

      DO j=jmin,jmax
       DO i=imin,imax
       
c  Photosynthetically-available radiations (PAR)
#ifdef USE_EXFQSW
        irr_surf(i,j) = max(epsln,
     &                 -parfrac*Qsw(i,j,bi,bj)*maskC(i,j,1,bi,bj))
#else
        irr_surf(i,j) = sfac(j)
#endif
cav        IF ( .NOT. QSW_underice ) THEN
c  if using Qsw but not seaice/thsice or coupled, then
c  ice fraction needs to be taken into account
cav         irr_surf(i,j) = irr_surf(i,j)*(1. _d 0 - FIce(i,j,bi,bj))
cav        ENDIF

#ifdef ML_MEAN_LIGHT
        SumMLIrr   = 0. _d 0
        tmp_ML     = 0. _d 0
#endif

        DO k=1,Nr

         IF (hFacC(i,j,k,bi,bj).gt.0) THEN

         IF (k.eq.1) THEN
c  Light attenuation in middle of top layer
          atten = k0*drF(1)/2. _d 0*hFacC(i,j,1,bi,bj)
          irr_inst(i,j,1) = irr_surf(i,j)*exp(-atten)
         ELSE
c  Attenuation from one more layer
          atten = k0*drF(k)/2. _d 0*hFacC(i,j,k,bi,bj)
     &           + k0*drF(k-1)/2. _d 0*hFacC(i,j,k-1,bi,bj)
          irr_inst(i,j,k) =
     &           irr_inst(i,j,k-1)*exp(-atten)
         ENDIF

#ifdef ML_MEAN_LIGHT
c  Mean irradiance in the mixed layer
         IF ((-rf(k+1) .le. mld(i,j)).and.
     &               (-rf(k+1).lt.200. _d 0)) THEN
          SumMLIrr = SumMLIrr+drF(k)*irr_inst(i,j,k)
          tmp_ML = tmp_ML + drF(k)
          irr_mix(i,j) = SumMLIrr/tmp_ML
         ENDIF
#endif

         ENDIF

        ENDDO
       ENDDO
      ENDDO


      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax  

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

          irr_eff(i,j,k) = irr_inst(i,j,k)
#ifdef ML_MEAN_LIGHT
c  Inside mixed layer, effective light is set to mean mixed layer light 
         IF ((-rf(k+1) .le. mld(i,j)).and.
     &               (-rf(k+1).lt.200. _d 0)) THEN
           irr_eff(i,j,k) = irr_mix(i,j)
          ENDIF
#endif 

         ENDIF

        ENDDO
       ENDDO
      ENDDO
     
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(Qsw,'BLGQSW  ',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(irr_inst,'BLGIRRIS',0,Nr,2,bi,bj,myThid)
      ENDIF 
#endif

      RETURN
      END
      
