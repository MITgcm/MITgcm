C $Header:

#include "SBO_OPTIONS.h"

      SUBROUTINE SBO_CALC( myCurrentTime, myIter, myThid )
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

c=======================================================================
c
c     Written  by Richard Gross (Richard.Gross@jpl.nasa.gov)
c     June 10, 2001: Modified for online computations in MIT GCM UV
c              by Dimitris Menemenlis (Menemenlis@jpl.nasa.gov)
c
c       Purpose
c           calc_sbo calculates the core products of the IERS Special Bureau
c           for the Oceans including oceanic mass, center-of-mass, angular
c           momentum, and bottom pressure.
c
c       Usage
c           1. calc_sbo must be called, and the results saved, at each time step
c              in order to create a time series of the IERS SBO core products
c           2. it is suggested that after the time series have been generated
c              and before saving the results to a file, time-mean values be
c              computed and removed from all of the calculated core products
c              and that the mean values be reported along with the demeaned
c              time series
c
c       Availability
c           ftp://euler.jpl.nasa.gov/sbo/software/calc_sbo.f
c
c       Reference
c           Gross, R. S., F. O. Bryan, Y. Chao, J. O. Dickey, S. L. Marcus,
c           R. M. Ponte, and R. Tokmakian, The IERS Special Bureau for the
c           Oceans, in IERS Technical Note on the IERS Global Geophysical
c           Fluids Center, edited by B. Chao, in press, Observatoire de Paris,
c           Paris, France, 2000.
c
c       Required inputs
c           gridded values of horizontal velocity (u,v), temperature,
c           salinity, and sea surface height along with the latitude,
c           and longitude of the grid points and the thicknesses of the
c           vertical layers
c
c       External routines called by calc_sbo
c           real function rho1(s, t)
c               returns density of sea water given salinity s and temperature t
c               (a default version of rho1 has been included with calc_sbo,
c               however in general this should be replaced by a function that
c               returns the density of the model ocean so that the same density
c               as the model s is used to compute the sbo products)
c
c       Assumptions
c           1. the input velocity, temperature, salinity, and sea surface
c              height fields are assumed to be defined on the same grid
c           2. the horizontal grid is assumed to be equally spaced in
c              latitude and longitude
c           3. land is flagged in the input quantities by a salinity or
c              temperature value greater than or equal to 999.99
c           4. input quantities are assumed to have the following units:
c                 salinity (s)              parts per thousand
c                 temperature (t)           degrees centigrade
c                 eastwards  velocity (u)   centimeters per second
c                 northwards velocity (v)   centimeters per second
c                 sea surface height (ssh)  meters
c                 latitude  of grid point   degrees N
c                 longitude of grid point   degrees E
c                 thickness of layer        meters
c           5. input quantities are passed to calc_sbo via common blocks
c              /ogcm/ and /vgrid/
c           6. land is flagged in the output ocean-bottom pressure (obp)
c              by a value of -999.99
c           7. calulated products have the units:
c                 mass of oceans (mass)           kilograms (kg)
c                 center-of-mass of oceans (com)  meters (m)
c                 oceanic angular momentum (oam)  kg-m**2/second
c                 ocean-bottom pressure    (obp)  Pascals (Newton/m**2)
c           8. calculated products are passed out of calc_sbo via common
c              block /sbo/
c           9. the sea surface height layer is assumed to have the same
c              velocity, temperature, and salinity as the first depth layer
c
c       For questions regarding calc_sbo or the IERS SBO, please contact:
c           Richard Gross                 Richard.Gross@jpl.nasa.gov
c           Jet Propulsion Laboratory     ph. +1 818-354-4010
c           Mail Stop 238-332             fax +1 818-393-6890
c           4800 Oak Grove Drive
c           Pasadena, Ca 91109-8099
c           USA
c
c=======================================================================

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "CG2D.h"
#include "SBO.h"

C     == Routine arguments ==
C     myCurrentTime - Current time of simulation ( s )
C     myIter        - Iteration number
C     myThid        - Number of this instance of SBO_CALC
      _RL     myCurrentTime
      INTEGER myIter, myThid

#ifdef ALLOW_SBO

c     external function called by calc_sbo
c     returns density of sea water
      _RL sbo_rho

c     internal variables
c     bi, bj    - array indices
c     I         - index over longitude grid points
c     J         - index over latitude  grid points
c     K         - index over layers
c     lat       - latitude  of grid point (radians)
c     lat_deg   - latitude  of grid point (degrees)
c     lon       - longitude of grid point (radians)
c     radius    - radius of bottom of layer (m)
c     darea     - element of surface area (unit radius)
c     dradius   - element of radius (m)
c     dvolume   - element of volume (m**3)
c     s         - salinity at grid point (ppt)
c     t         - temperature at grid point (deg C)
c     u         - eastward  velocity at grid point (m/s)
c     v         - northward velocity at grid point (m/s)
c     density   - density at grid point (kg/m**3)
c     ae        - earth s mean radius  (m) (PREM value)
c     grav      - earth s mean gravity (m/s**2) (PREM)
c     sbo_omega - earth s mean angular velocity (rad/s)
      integer bi, bj, I, J, K
      _RL lat, lat_deg, lon, radius, darea, dradius, dvolume, depth
      _RL s, t, u, v, density
      _RL ae        /6.3710e6/
      _RL grav      /9.8156/
      _RL sbo_omega /7.292115e-5/

c     initialize variables to be computed
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
      DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
            DO J = 1-OLy, sNy+OLy
               DO I = 1-OLx, sNx+OLx
                  obp(I,J,bi,bj) = 0.0
               ENDDO
            ENDDO
         ENDDO
      ENDDO

c     loop over all grid points, accumulating mass, com, oam, and obp

      do bj = myByLo(myThid), myByHi(myThid)
         do bi = myBxLo(myThid), myBxHi(myThid)
            do J = 1, sNy
               do I = 1, sNx

c     latitude (rad)
               lat_deg = yC(I,J,bi,bj)
               lat = yC(I,J,bi,bj) * pi / 180.0

c     longitude (rad)
                  lon = xC(I,J,bi,bj) * pi / 180.0

c     unit radius
                  darea = dyF(I,J,bi,bj) * dxF(I,J,bi,bj) / ae / ae

                  do K = 0, Nr
c     K=0 => ssh
                     if (K .eq. 0) then

c     if land, flag it in obp and skip it
                        if (_hFacC(i,j,1,bi,bj).eq.0.) then
                           obp(I,J,bi,bj) = -999.99
                           goto 10
                        end if

                        radius = ae
                        dradius = etaN(I,J,bi,bj)
c     assume surface has same vel and density as first layer
                        s = salt(I,J,1,bi,bj)
                        t = theta(I,J,1,bi,bj)
                        u =(uvel(I,J,1,bi,bj)+uvel(I+1,J,1,bi,bj))/2.
                        v =(vvel(I,J,1,bi,bj)+vvel(I,J+1,1,bi,bj))/2.

                     else

c     if land, skip it
                        if (_hFacC(i,j,k,bi,bj).eq.0.) goto 10

c     radius to center of cell (m)
                        radius = ae - abs(rC(K))
                        dradius = drF(K)
                        s = salt(I,J,K,bi,bj)
                        t = theta(I,J,K,bi,bj)
                        u =(uvel(I,J,K,bi,bj)+uvel(I+1,J,K,bi,bj))/2.
                        v =(vvel(I,J,K,bi,bj)+vvel(I,J+1,K,bi,bj))/2.
                     end if

c     cell volume (m**3)
                     dvolume = darea * radius**2 * dradius

c     get density
                     depth = ae - radius
                     density = sbo_rho(depth,lat_deg,s,t)

c     accumulate mass of oceans
                     mass = mass + density * dvolume

c     accumulate center-of-mass of oceans
                     xcom = xcom + density * cos(lat) * cos(lon)
     &                    * radius * dvolume
                     ycom = ycom + density * cos(lat) * sin(lon)
     &                    * radius * dvolume
                     zcom = zcom + density * sin(lat) *
     &                    radius * dvolume

c     accumulate oceanic angular momentum due to currents
                     xoamc = xoamc + ( v*sin(lon)-u*sin(lat)*cos(lon))
     &                    * density * radius * dvolume
                     yoamc = yoamc + (-v*cos(lon)-u*sin(lat)*sin(lon))
     &                    * density * radius * dvolume
                     zoamc = zoamc +   u*cos(lat)
     &                    * density * radius * dvolume

c     accumulate oceanic angular momentum due to pressure
                     xoamp = xoamp - sin(lat) * cos(lat) * cos(lon)
     &                    * sbo_omega * density * radius**2 * dvolume
                     yoamp = yoamp - sin(lat) * cos(lat) * sin(lon)
     &                    * sbo_omega * density * radius**2 * dvolume
                     zoamp = zoamp + cos(lat)**2
     &                    * sbo_omega * density * radius**2 * dvolume

c     accumulate ocean-bottom pressure
                     obp(I,J,bi,bj) = obp(I,J,bi,bj) +
     &                    grav * density * dradius

c     end loop over depth
                  end do

 10               continue

c     end loop over longitude
               end do

c     end loop over latitude
            end do

c     end loop over bi
         end do

c     end loop over bj
      end do

c     sum all values across model tiles
      _GLOBAL_SUM_R8( mass  , myThid )
      _GLOBAL_SUM_R8( xcom  , myThid )
      _GLOBAL_SUM_R8( ycom  , myThid )
      _GLOBAL_SUM_R8( zcom  , myThid )
      _GLOBAL_SUM_R8( xoamc , myThid )
      _GLOBAL_SUM_R8( yoamc , myThid )
      _GLOBAL_SUM_R8( zoamc , myThid )
      _GLOBAL_SUM_R8( xoamp , myThid )
      _GLOBAL_SUM_R8( yoamp , myThid )
      _GLOBAL_SUM_R8( zoamp , myThid )

c     finish calculating center-of-mass of oceans
      xcom = xcom / mass
      ycom = ycom / mass
      zcom = zcom / mass

#endif ALLOW_SBO

      return
      end
