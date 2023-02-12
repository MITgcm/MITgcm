#include "ICEPLUME_OPTIONS.h"

      SUBROUTINE ICEPLUME_MELTRATE(
     I           temperature, salinity, velocity, depth,
     O           meltrate_b, Tb, Sb )

      IMPLICIT NONE

C Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ICEPLUME.h"

c Input: properties in the boundary layer, either from
c subglacial plume or submarine background smaller plumes:

      _RL temperature
      _RL salinity
      _RL velocity
      _RL depth
c Output: properties of the glacial front or ice/ocean boundary
      _RL meltrate_b, Tb, Sb

C Local variables
      _RL a, b, c, absVelocity, mdot, factor_Cd
c      _RL secInday

c       secInday = 86400 . _d 0

#ifdef ALLOW_ICEPLUME

C Routine cannot cope with zero velocity. Unlikely to occur anyway with currents,
C waves, convection etc. This isn't very physical, but will do for now.
catn 30.Nov.2022 : this check below did not account for abs(velocity)
catn so i am assuming the velocity passed in is already abs?
ckiki to be sure, I inserted the abs
      IF ( abs(velocity) .LT. backgroundVelThresh ) THEN
       absVelocity = backgroundVelThresh
      ELSE
       absVelocity = abs(velocity)
      ENDIF

C Calculate melt rate from 3 equation formualtion (as for plume models)

C     Equations for Sb, Tb and mdot

      factor_Cd = (Cd**HalfRL)*absVelocity

      a = lambda1*(GamT*HeatCapacity_Cp-GamS*c_i)

      b = GamS*c_i*(lambda1*salinity-lambda2-lambda3*depth+
     &         iceTemp-(ICEPLUMElatentHeat/c_i))
     &        -GamT*HeatCapacity_Cp*(temperature-lambda2-lambda3*depth)

      c = GamS*salinity*(c_i*(lambda2+lambda3*depth-iceTemp)
     &       +ICEPLUMElatentHeat)

      Sb   = (oneRL/(twoRL*a))*(-b-((b**twoRL-4.*a*c)**HalfRL)) !Sb
      Tb   = lambda1*Sb+lambda2+lambda3*depth !Tb
      mdot = GamS*(Cd**HalfRL)*absVelocity*(salinity-Sb)/Sb ! mdot

catn: to make this function more general, we remove conversion to m/d
cC Meltrate is in m d^-1
c       meltrate_b = secInday*mdot
       meltrate_b = mdot

#endif /* ALLOW_ICEPLUME */

      RETURN
      END
