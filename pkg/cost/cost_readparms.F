C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_readparms.F,v 1.6 2004/09/27 18:00:19 heimbach Exp $

#include "COST_CPPOPTIONS.h"

      subroutine cost_readparms( mythid )

c     ==================================================================
c     SUBROUTINE cost_readparms
c     ==================================================================
c
c     o This routine initialises the package cost.
c
c     started: Ralf Giering 18-Jan-2001
c
c     ==================================================================
c     SUBROUTINE cost_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "cost.h"

c     == routine arguments ==
      integer mythid

c     == local variables ==

c     == end of interface ==

c--   Read the namelist input.
      namelist /cost_nml/ 
     &                   mult_atl, mult_test, mult_tracer
     &                 , lastinterval

      _BEGIN_MASTER( myThid )

c--     Set default values.
        mult_atl              =      0. _d 0
        mult_test             =      0. _d 0
        mult_tracer           =      0. _d 0

c--     30*24*3600 = 2592000 = 1 model month
        lastinterval = 2592000.

c--     Next, read the cost data file.
        call nml_filter( 'data.cost', scrunit1, myThid )
        if (scrunit1 .eq. 0) then
           stop 'cost_readparms: reading namelist failed'
        end if
        read(  scrunit1, nml = cost_nml )
        close( scrunit1 )

      _END_MASTER( mythid )

      _BARRIER

      end
