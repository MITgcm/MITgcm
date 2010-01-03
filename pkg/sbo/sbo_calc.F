C $Header: /u/gcmpack/MITgcm/pkg/sbo/sbo_calc.F,v 1.14 2010/01/03 20:01:36 jmc Exp $
C $Name:  $

#include "SBO_OPTIONS.h"

CBOP
C !ROUTINE: SBO_CALC

C !INTERFACE: ==========================================================
      SUBROUTINE SBO_CALC( myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE SBO_CALC                                      |
C     | o Do SBO diagnostic output.                              |
C     |==========================================================|
C     | NOTE: The following subtleties are ignored for time      |
C     | being but may need revisiting at some point in time.     |
C     | 1) The model is volume-preserving and Boussinesq so      |
C     |    quantities like oceanic mass need to be interpreted   |
C     |    with some care.                                       |
C     | 2) The sea surface height variable etaN lags other       |
C     |    prognostic variables by half a time step.  This lag   |
C     |    is ignored in SBO computations.                       |
C     | 3) Density is computed using function SBO_RHO which is   |
C     |    not exaclty equivalent to the model s FIND_RHO.       |
C     \==========================================================/
      IMPLICIT NONE

C=======================================================================
C
C     Written  by Richard Gross (Richard.Gross@jpl.nasa.gov)
C     June 10, 2001: Modified for online computations in MIT GCM UV
C              by Dimitris Menemenlis (Menemenlis@jpl.nasa.gov)
C
C       Purpose
C           calc_sbo calculates the core products of the IERS Special Bureau
C           for the Oceans including oceanic mass, center-of-mass, angular
C           momentum, and bottom pressure.
C
C       Usage
C           1. calc_sbo must be called, and the results saved, at each time step
C              in order to create a time series of the IERS SBO core products
C           2. it is suggested that after the time series have been generated
C              and before saving the results to a file, time-mean values be
C              computed and removed from all of the calculated core products
C              and that the mean values be reported along with the demeaned
C              time series
C
C       Availability
C           ftp://euler.jpl.nasa.gov/sbo/software/calc_sbo.f
C
C       Reference
C           Gross, R. S., F. O. Bryan, Y. Chao, J. O. Dickey, S. L. Marcus,
C           R. M. Ponte, and R. Tokmakian, The IERS Special Bureau for the
C           Oceans, in IERS Technical Note on the IERS Global Geophysical
C           Fluids Center, edited by B. Chao, in press, Observatoire de Paris,
C           Paris, France, 2000.
C
C       Required inputs
C           gridded values of horizontal velocity (u,v), temperature,
C           salinity, and sea surface height along with the latitude,
C           and longitude of the grid points and the thicknesses of the
C           vertical layers
C
C       External routines called by calc_sbo
C           real function rho1(s, t)
C               returns density of sea water given salinity s and temperature t
C               (a default version of rho1 has been included with calc_sbo,
C               however in general this should be replaced by a function that
C               returns the density of the model ocean so that the same density
C               as the model s is used to compute the sbo products)
C
C       Assumptions
C           1. the input velocity, temperature, salinity, and sea surface
C              height fields are assumed to be defined on the same grid
C           2. the horizontal grid is assumed to be equally spaced in
C              latitude and longitude
C           3. land is flagged in the input quantities by a salinity or
C              temperature value greater than or equal to 999.99
C           4. input quantities are assumed to have the following units:
C                 salinity (s)              parts per thousand
C                 temperature (t)           degrees centigrade
C                 eastwards  velocity (u)   centimeters per second
C                 northwards velocity (v)   centimeters per second
C                 sea surface height (ssh)  meters
C                 latitude  of grid point   degrees N
C                 longitude of grid point   degrees E
C                 thickness of layer        meters
C           5. input quantities are passed to calc_sbo via common blocks
C              /ogcm/ and /vgrid/
C           6. land is flagged in the output ocean-bottom pressure (obp)
C              by a value of -999.99
C           7. calulated products have the units:
C                 mass of oceans (mass)           kilograms (kg)
C                 center-of-mass of oceans (com)  meters (m)
C                 oceanic angular momentum (oam)  kg-m**2/second
C                 ocean-bottom pressure    (obp)  Pascals (Newton/m**2)
C           8. calculated products are passed out of calc_sbo via common
C              block /sbo/
C           9. the sea surface height layer is assumed to have the same
C              velocity, temperature, and salinity as the first depth layer
C
C       For questions regarding calc_sbo or the IERS SBO, please contact:
C           Richard Gross                 Richard.Gross@jpl.nasa.gov
C           Jet Propulsion Laboratory     ph. +1 818-354-4010
C           Mail Stop 238-332             fax +1 818-393-6890
C           4800 Oak Grove Drive
C           Pasadena, Ca 91109-8099
C           USA
C
C=======================================================================
C \ev

C !USES: ===============================================================
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SBO.h"

C !INPUT PARAMETERS: ===================================================
C     == Routine arguments ==
C     myTime  :: Current time of simulation ( s )
C     myIter  :: Iteration number
C     myThid  :: Number of this instance of SBO_CALC
      _RL     myTime
      INTEGER myIter, myThid

#ifdef ALLOW_SBO

C !LOCAL VARIABLES: ====================================================
C     external function called by calc_sbo
C     returns density of sea water
      _RL rhoK(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     internal variables
C     bi, bj    :: array indices
C     i         :: index over longitude grid points
C     j         :: index over latitude  grid points
C     k         :: index over layers
C     lat       :: latitude  of grid point (radians)
C     lat_deg   :: latitude  of grid point (degrees)
C     lon       :: longitude of grid point (radians)
C     radius    :: radius of bottom of layer (m)
C     darea     :: element of surface area (unit radius)
C     dradius   :: element of radius (m)
C     dvolume   :: element of volume (m**3)
C     s         :: salinity at grid point (ppt)
C     t         :: temperature at grid point (deg C)
C     u         :: eastward  velocity at grid point (m/s)
C     v         :: northward velocity at grid point (m/s)
C     density   :: density at grid point (kg/m**3)
C     ae        :: earth s mean radius  (m) (PREM value)
C     grav      :: earth s mean gravity (m/s**2) (PREM)
C     sbo_omega :: earth s mean angular velocity (rad/s)
      integer bi, bj, i, j, k, kn0
      _RL lat, lat_deg, lon, radius, darea, dradius, dvolume, depth
      _RL u, v, density
      _RL ae, grav, sbo_omega
      PARAMETER ( ae        = 6.3710 _d 6    )
      PARAMETER ( grav      = 9.8156         )
      PARAMETER ( sbo_omega = 7.292115 _d -5 )
CEOP

C     initialize variables to be computed
      xoamc = 0.0
      yoamc = 0.0
      zoamc = 0.0
      xoamp = 0.0
      yoamp = 0.0
      zoamp = 0.0
      mass  = 0.0
      xcom  = 0.0
      ycom  = 0.0
      zcom  = 0.0
      sbobp       = 0.0
      sboarea     = 0.0
      sboempmrwet = 0.0
      sboqnetwet  = 0.0

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
            obp(i,j,bi,bj) = 0.0
          ENDDO
         ENDDO
       ENDDO
      ENDDO

C     loop over all grid points, accumulating mass, com, oam, and obp

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 0, Nr
          kn0 = max(k,1)

          CALL FIND_RHO_2D(
     I           1, sNx, 1, sNy, kn0,
     I           theta(1-OLx,1-OLy,kn0,bi,bj),
     I           salt(1-OLx,1-OLy,kn0,bi,bj),
     O           rhoK,
     I           kn0, bi, bj, myThid )

C--
          DO j = 1, sNy
           DO i = 1, sNx
            IF ( maskC(i,j,kn0,bi,bj) .NE. 0. ) THEN

C     latitude (rad)
              lat_deg = yC(i,j,bi,bj)
              lat = yC(i,j,bi,bj) * pi / 180.0
C     longitude (rad)
              lon = xC(i,j,bi,bj) * pi / 180.0
C     unit radius : should be using rA like this:
c             darea = rA(i,j,bi,bj)*maskC(i,j,kn0,bi,bj) /ae/ae
              darea = dyF(i,j,bi,bj) * dxF(i,j,bi,bj) / ae / ae
     &              * maskC(i,j,kn0,bi,bj)

              IF ( k .EQ. 0 ) THEN
                sboarea = sboarea + darea
                sboempmrwet = sboempmrwet + empmr(i,j,bi,bj)*darea
                sboqnetwet  = sboqnetwet  + qnet(i,j,bi,bj) *darea
C     k=0 => ssh
                radius = ae
                dradius = etaN(i,j,bi,bj) * maskC(i,j,kn0,bi,bj)

              ELSE
C-- k > 0

C     radius to center of cell (m)
                radius = ae - ABS(rC(k))
                dradius = drF(k) * maskC(i,j,k,bi,bj)
C-- end of k-if
              ENDIF


cph              s = salt(i,j,kn0,bi,bj)
cph              t = theta(i,j,kn0,bi,bj)
              u =(uvel(i,j,kn0,bi,bj)+uvel(i+1,j,kn0,bi,bj))*0.5 _d 0
              v =(vvel(i,j,kn0,bi,bj)+vvel(i,j+1,kn0,bi,bj))*0.5 _d 0

C     cell volume (m**3)
              dvolume = darea * radius**2 * dradius

C     get density
              depth = ae - radius
cph(
cph compute density consistent with EOS used by model
cph              density = sbo_rho(depth,lat_deg,s,t)
              density = rhoConst + rhoK(i,j)
cph)

C     accumulate mass of oceans
              mass = mass + density * dvolume

C     accumulate center-of-mass of oceans
              xcom = xcom + density * COS(lat) * COS(lon)
     &             * radius * dvolume
              ycom = ycom + density * COS(lat) * SIN(lon)
     &             * radius * dvolume
              zcom = zcom + density * SIN(lat)
     &             * radius * dvolume

C     accumulate oceanic angular momentum due to currents
              xoamc = xoamc + ( v*SIN(lon)-u*SIN(lat)*COS(lon))
     &              * density * radius * dvolume
              yoamc = yoamc + (-v*COS(lon)-u*SIN(lat)*SIN(lon))
     &              * density * radius * dvolume
              zoamc = zoamc +   u*COS(lat)
     &              * density * radius * dvolume

C     accumulate oceanic angular momentum due to pressure
              xoamp = xoamp - SIN(lat) * COS(lat) * COS(lon)
     &              * sbo_omega * density * radius*radius * dvolume
              yoamp = yoamp - SIN(lat) * COS(lat) * SIN(lon)
     &              * sbo_omega * density * radius*radius * dvolume
              zoamp = zoamp + COS(lat)**2
     &              * sbo_omega * density * radius*radius * dvolume

C     accumulate ocean-bottom pressure
              obp(i,j,bi,bj) = obp(i,j,bi,bj)
     &              + grav * density * dradius

C     end if wet
            ENDIF
C     end loop over i,j
           ENDDO
          ENDDO

C     end loop over k
        ENDDO

C     accumulate for global-mean ocean-bottom pressure
        DO j = 1, sNy
          DO i = 1, sNx
            sbobp = sbobp + obp(i,j,bi,bj)*rA(i,j,bi,bj)
          ENDDO
        ENDDO

C     end loop over bi,bj
       ENDDO
      ENDDO

C     sum all values across model tiles
C- note: GLOBAL_SUM applied to var. in common block <= wrong if Muti-threaded
      _GLOBAL_SUM_RL( mass  , myThid )
      _GLOBAL_SUM_RL( xcom  , myThid )
      _GLOBAL_SUM_RL( ycom  , myThid )
      _GLOBAL_SUM_RL( zcom  , myThid )
      _GLOBAL_SUM_RL( xoamc , myThid )
      _GLOBAL_SUM_RL( yoamc , myThid )
      _GLOBAL_SUM_RL( zoamc , myThid )
      _GLOBAL_SUM_RL( xoamp , myThid )
      _GLOBAL_SUM_RL( yoamp , myThid )
      _GLOBAL_SUM_RL( zoamp , myThid )
cph(
      _GLOBAL_SUM_RL( sbobp, myThid )
      _GLOBAL_SUM_RL( sboarea, myThid )
      _GLOBAL_SUM_RL( sboempmrwet, myThid )
      _GLOBAL_SUM_RL( sboqnetwet, myThid )
cph)

C     finish calculating center-of-mass of oceans
      xcom = xcom / mass
      ycom = ycom / mass
      zcom = zcom / mass
cph(
c     sbobp       = sbobp / sboarea
      sbobp       = sbobp / globalArea
      sboempmrwet = sboempmrwet / sboarea
      sboqnetwet  = sboqnetwet / sboarea
cph)

#endif /* ALLOW_SBO */

      RETURN
      END
