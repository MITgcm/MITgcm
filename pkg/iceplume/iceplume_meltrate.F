
      SUBROUTINE ICEPLUME_MELTRATE(
     I           temperature, salinity, velocity, depth,
     O           meltrate )

      IMPLICIT NONE

C Global variables
#include "ICEPLUME_OPTIONS.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ICEPLUME.h"

C Local variables

      _RL temperature
      _RL salinity
      _RL velocity
      _RL meltrate
      _RL depth
      _RL absVelocity
      _RL a,b,c, tb, sb, mdot

#ifdef ALLOW_ICEPLUME

C Routine can't cope with zero velocity. Unlikely to occur anyway with currents,
C waves, convection etc. This isn't very physical, but will do for now.
      IF ( velocity .LT. backgroundVel ) velocity = backgroundVel

      absVelocity = abs(velocity)

C Calculate melt rate from 3 equation formualtion (as for plume models)

C     Equations for Sb, Tb and mdot

      a = lambda1*(GamT*c_w-GamS*c_i)

      b = GamS*c_i*(lambda1*salinity-lambda2-lambda3*depth+
     &         iceTemp-(L/c_i))
     &        -GamT*c_w*(temperature-lambda2-lambda3*depth)

      c = GamS*salinity*(c_i*(lambda2+lambda3*depth-iceTemp)+L)

      Sb   = (1./(2.*a))*(-b-((b**2.-4.*a*c)**0.5)) !Sb
      Tb   = lambda1*Sb+lambda2+lambda3*depth !Tb
      mdot = GamS*(Cd**0.5)*absVelocity*(salinity-Sb)/Sb ! mdot

C Meltrate is in m d^-1
       meltrate = 86400.*mdot

#endif /* ALLOW_ICEPLUME */

      RETURN
      END
