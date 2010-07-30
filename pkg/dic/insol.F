C $Header: /u/gcmpack/MITgcm/pkg/dic/insol.F,v 1.14 2010/07/30 12:06:44 mlosch Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: INSOL

C !INTERFACE: ==========================================================
      SUBROUTINE insol(Time,sfac,bi,bj,myThid)

C !DESCRIPTION:
C find light as function of date and latitude
C based on paltridge and parson


C !USES: ===============================================================
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "DIC_VARS.h"

C !INPUT PARAMETERS: ===================================================
C Time                 :: current time
       _RL Time
       INTEGER bi,bj
       INTEGER myThid

C !OUPUT PARAMETERS: ===================================================
       _RL sfac(1-OLy:sNy+OLy)

#ifdef DIC_BIOTIC

C !LOCAL VARIABLES: ====================================================
       _RL  solar, albedo
       _RL  dayfrac, yday, delta
       _RL  lat, sun1, dayhrs
       _RL  cosz, frac, fluxi
       integer j
CEOP

      solar = 1360. _d 0   !solar constant
      albedo= 0.6 _d 0     !planetary albedo

C     Case where a 2-d output array is needed: for now, stop here.
      IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
       STOP 'ABNORMAL END: S/R INSOL: 2-D output not implemented'
      ENDIF

C find day (****NOTE for year starting in winter*****)
        dayfrac=mod(Time,360. _d 0*86400. _d 0)
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

#endif /* DIC_BIOTIC */

      RETURN
      END
