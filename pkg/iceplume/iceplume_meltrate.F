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
#include "ICEPLUME_PARAMS.h"

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
C 30.Nov.2022 : this check below did not account for abs(velocity)
C so atn assumes the velocity passed in is already abs?
Ckiki to be sure, I inserted the abs
      IF ( abs(velocity) .LT. backgroundVelThresh ) THEN
       absVelocity = backgroundVelThresh
      ELSE
       absVelocity = abs(velocity)
      ENDIF

C Calculate melt rate from 3 equation formualtion (as for plume models)

C     Equations for Sb, Tb and mdot

C This setting of Gam[T,S]const were moved to iceplume_readparm.F
c        GamSconst = 0.07*GamTconst

#ifndef ICEPLUME_ALLOW_SCHULZ22
      factor_Cd = (Cd**HalfRL)*absVelocity
#else
      factor_Cd = (Cd**HalfRL)*(absVelocity-backgroundVelThresh)
#endif

      a = lambda1*(GamT     *HeatCapacity_Cp-GamS     *c_i)
#ifdef ICEPLUME_ALLOW_SCHULZ22
      a = a*factor_Cd
     &  + lambda1*(GamTconst*HeatCapacity_Cp-GamSconst*c_i)
#endif

      b = GamS     *c_i*(lambda1*salinity-lambda2-lambda3*depth+
     &                   iceTemp-(ICEPLUMElatentHeat/c_i))
     &   -GamT     *HeatCapacity_Cp*(temperature-lambda2-lambda3*depth)
#ifdef ICEPLUME_ALLOW_SCHULZ22
      b = b*factor_Cd
     &   +GamSconst*c_i*(lambda1*salinity-lambda2-lambda3*depth+
     &                   iceTemp-(ICEPLUMElatentHeat/c_i))
     &   -GamTconst*HeatCapacity_Cp*(temperature-lambda2-lambda3*depth)
#endif

      c = GamS     *salinity*(c_i*(lambda2+lambda3*depth-iceTemp)
     &       +ICEPLUMElatentHeat)
#ifdef ICEPLUME_ALLOW_SCHULZ22
      c = c*factor_Cd
     &   +GamSconst*salinity*(c_i*(lambda2+lambda3*depth-iceTemp)
     &       +ICEPLUMElatentHeat)
#endif

      Sb   = (oneRL/(twoRL*a))*(-b-((b**twoRL-4.*a*c)**HalfRL))
      Tb   = lambda1*Sb+lambda2+lambda3*depth
      mdot = GamS*(Cd**HalfRL)*absVelocity*(salinity-Sb)/Sb
#ifdef ICEPLUME_ALLOW_SCHULZ22
      mdot = mdot 
     &                 + GamSconst*(salinity-Sb)/Sb
#endif

C To make this function more general, we remove conversion to m/d
C Meltrate is in m d^-1
c       meltrate_b = secInday*mdot
       meltrate_b = mdot

#endif /* ALLOW_ICEPLUME */

      RETURN
      END
