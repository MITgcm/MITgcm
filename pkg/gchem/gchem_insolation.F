#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_INSOLATION

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_INSOLATION(
     O     solar_radiation,
     I     iMin, iMax, jMin, jMax, bi, bj,
     I     myTime, myIter, myThid )

C !DESCRIPTION:
C find daily mean insolation as function of date and latitude based on
C Paltridge, G. W. and C. M. R. Platt. 1976. Radiative Processes in
C Meteorology and Climatology. Elsevier Sci. 318 p.

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation domain: 1rst index range
C     jMin,jMax     :: computation domain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, iMin, iMax, jMin, jMax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !OUPUT PARAMETERS: ===================================================
      _RL     solar_radiation(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !FUNCTIONS: ==========================================================

C !LOCAL VARIABLES: ====================================================
      INTEGER i,j
      _RL solar, albedo
      _RL dayfrac, yday, delta
      _RL lat, sun1, dayhrs
      _RL cosz, frac
CEOP

C constants
      solar  = 1360. _d 0   !solar constant
      albedo = 0.6 _d 0     !planetary albedo

C find day (****NOTE for year starting in winter*****)
C fraction of year
      dayfrac=MOD( myTime,gchem_secondsPerYear )/ gchem_secondsPerYear
C convert to radians
      yday = 2. _d 0*PI*dayfrac
C cosine zenith angle (paltridge+platt)
      delta = (0.006918 _d 0
     &       -(0.399912 _d 0*COS(yday))
     &       +(0.070257 _d 0*SIN(yday))
     &       -(0.006758 _d 0*COS(2. _d 0*yday))
     &       +(0.000907 _d 0*SIN(2. _d 0*yday))
     &       -(0.002697 _d 0*COS(3. _d 0*yday))
     &       +(0.001480 _d 0*SIN(3. _d 0*yday)) )

      DO j = jMin, jMax
       DO i = iMin, iMax
C latitude in radians
        lat=YC(i,j,bi,bj)*deg2rad
C     latitute in radians, backed out from coriolis parameter
C     (makes latitude independent of grid)
        IF ( usingCartesianGrid .OR. usingCylindricalGrid )
     &       lat = ASIN( fCori(i,j,bi,bj)/(2. _d 0*omega) )
C--    sun1 = -TAN(delta) * TAN(lat) ! why not use that?
        sun1 = -SIN(delta)/COS(delta) * SIN(lat)/COS(lat)
        IF (sun1.LE.-0.999 _d 0) sun1=-0.999 _d 0
        IF (sun1.GE. 0.999 _d 0) sun1= 0.999 _d 0
        dayhrs = ABS(ACOS(sun1))
C average zenith angle
        cosz = ( SIN(delta)*SIN(lat)
     &         +(COS(delta)*COS(lat)*SIN(dayhrs)/dayhrs) )
        IF (cosz.LE.5. _d -3) cosz= 5. _d -3
C fraction of daylight in day
        frac = dayhrs/PI

        solar_radiation(i,j) = solar*(1. _d 0-albedo)*cosz*frac

       ENDDO
      ENDDO

      RETURN
      END
