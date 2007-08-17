
#include "COST_CPPOPTIONS.h"


      subroutine cost_internal_params( myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_internal_params
c     ==================================================================
c
c     o Evaluate cost function contributions of internal parameters adjustments.
c
c     started: Gael Forget 17-Aug-2007
c
c     ==================================================================
c     SUBROUTINE cost_internal_params
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "ecco_cost.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == end of interface ==


c--   Eddy stress penalty term
#if (defined (ALLOW_TAUEDDY_COST_CONTRIBUTION) || defined (ALLOW_COST_TAU_EDDY))
      call timer_start('cost_tau_eddy      [ECCO SPIN-DOWN]', mythid)
      call cost_tau_eddy( myiter, mytime, mythid )
      call timer_stop ('cost_tau_eddy      [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

c--   GM coeffs penalty term
#ifdef ALLOW_KAPGM_COST_CONTRIBUTION
      call timer_start('cost_kapgm      [ECCO SPIN-DOWN]', mythid)
      call cost_kapgm( myiter, mytime, mythid )
      call timer_stop ('cost_kapgm      [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

c--   DIFFKR coeffs penalty term
#ifdef ALLOW_DIFFKR_COST_CONTRIBUTION
      call timer_start('cost_diffkr      [ECCO SPIN-DOWN]', mythid)
      call cost_diffkr( myiter, mytime, mythid )
      call timer_stop ('cost_diffkr      [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

c--   Bottom Drag penalty term
#ifdef ALLOW_BOTTOMDRAG_COST_CONTRIBUTION
      call timer_start('cost_bottomdrag      [ECCO SPIN-DOWN]', mythid)
      call cost_bottomdrag( myiter, mytime, mythid )
      call timer_stop ('cost_bottomdrag      [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

      end
