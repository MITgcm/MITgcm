
#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: INSOL

C !INTERFACE: ==========================================================
      SUBROUTINE insol(Time,sfac,bj)

C !DESCRIPTION:
C find light as function of date and latitude
c based on paltridge and parson


C !USES: ===============================================================
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "DIC_BIOTIC.h"

C !INPUT PARAMETERS: ===================================================
C time                 :: current time
       _RL time
       integer bj

C !OUPUT PARAMETERS: ===================================================
       _RL sfac(1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
       _RL  solar, albedo
       _RL  dayfrac, yday, delta
       _RL  lat, sun1, dayhrs
       _RL  cosz, frac, fluxi
       integer j
CEOP

c
      solar = 1360.   !solar constant
      albedo = 0.6    !planetary albedo

#ifndef READ_PAR
C     latitute in radians, backed out from coriolis parameter
C     (makes latitude independent of grid)
      IF ( usingCurvilinearGrid ) THEN
       STOP 'S/R INSOL: use #define READ_PAR with usingCurvilinearGrid'
      ENDIF
#endif /* READ_PAR */

c
c find day (****NOTE for year starting in winter*****)
        dayfrac=mod(Time,360. _d 0 *86400. _d 0)
     &                    /(360. _d 0 *86400. _d 0)          !fraction of year
        yday = 2.0*3.1416*dayfrac                         !convert to radians
        delta = (0.006918 - (0.399912*cos(yday))      !cosine zenith angle
     &          +(0.070257*sin(yday))                 !(paltridge+platt)
     &          -(0.006758*cos(2.0*yday))
     &          +(0.000907*sin(2.0*yday))
     &          -(0.002697*cos(3.0*yday))
     &          +(0.001480*sin(3.0*yday)) )
       do j=1-OLy,sNy+OLy
c latitude in radians
          lat=YC(1,j,1,bj)/180.d0*3.1416
#ifndef READ_PAR
C     latitute in radians, backed out from coriolis parameter
C     (makes latitude independent of grid)
          IF ( .NOT. usingSphericalPolarGrid )
     &         lat = asin( fCori(1,j,1,bj)/(2. _d 0 *omega) )*deg2rad
#endif /* READ_PAR */
          sun1 = -sin(delta)/cos(delta) * sin(lat)/cos(lat)
          if (sun1.le.-0.999) sun1=-0.999
          if (sun1.ge. 0.999) sun1= 0.999
          dayhrs = abs(acos(sun1))
          cosz = ( sin(delta)*sin(lat)+              !average zenith angle
     &            (cos(delta)*cos(lat)*sin(dayhrs)/dayhrs) )
          if (cosz.le.0.005) cosz=0.005
          frac = dayhrs/3.1416               !fraction of daylight in day
c daily average photosynthetically active solar radiation just below surface
          fluxi = solar*(1.0-albedo)*cosz*frac*parfrac
c
c convert to sfac
          if (fluxi.gt.0.0) sfac(j)=fluxi
c very large for polar night
          if (fluxi.lt.0.00001) sfac(j)=0.00001
       enddo !j
c
      return
      end
c
C==========================================================================
