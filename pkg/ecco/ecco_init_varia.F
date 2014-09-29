C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_init_varia.F,v 1.2 2014/09/29 16:47:50 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"
#include "AD_CONFIG.h"

      subroutine ecco_init_varia( mythid )

c     ==================================================================
c     SUBROUTINE ecco_init_varia
c     ==================================================================
c
c     o Initialise ecco variables.
c
c     ==================================================================
c     SUBROUTINE ecco_cost_init_varia
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "ecco_cost.h"
#ifdef ALLOW_COST
# include "cost.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

c     == external functions ==

c     == end of interface ==

#ifdef ALLOW_PSBAR_STERIC
      RHOsumGlob_0=0. _d 0
      VOLsumGlob_0=0. _d 0
#endif

      CALL ECCO_PHYS( myThid )

#ifdef ALLOW_PSBAR_STERIC
      RHOsumGlob_0=RHOsumGlob
      VOLsumGlob_0=VOLsumGlob
#endif

      CALL ECCO_COST_INIT_VARIA( myThid )

      _BARRIER

      return
      end

