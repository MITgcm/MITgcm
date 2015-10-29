C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_internal_params.F,v 1.10 2015/10/29 03:41:06 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


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
#ifdef ECCO_CTRL_DEPRECATED
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CAL
# include "cal.h"
#endif

#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#endif

#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif
#endif

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#ifdef ECCO_CTRL_DEPRECATED

      logical dodimensionalcost2D, dodimensionalcost3D

c     == end of interface ==

      dodimensionalcost2D=.NOT.ctrlSmoothCorrel2D
      dodimensionalcost3D=.NOT.ctrlSmoothCorrel3D

c--   Eddy stress penalty term
#ifdef ALLOW_EDDYPSI_COST_CONTRIBUTION
      call timer_start('cost_tau_eddy      [ECCO SPIN-DOWN]', mythid)
      call ctrl_cost_gen3d ( xx_edtaux_file,
     &     xx_edtaux_dummy, wedtauxFld, dodimensionalcost3D,
     &     num_zero_mean, objf_eddytau, maskC, myThid )
      call ctrl_cost_gen3d (
     &     xx_edtauy_file,xx_edtauy_dummy, wedtauyFld,
     &     num_zero_mean, objf_eddytau, maskC, myThid )
      call timer_stop ('cost_tau_eddy      [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

      if (.NOT.ctrlUseGen) then

c--   GM coeffs penalty term
#ifdef ALLOW_diffkr_COST_CONTRIBUTION
      call timer_start('cost_kapgm      [ECCO SPIN-DOWN]', mythid)
      call ctrl_cost_gen3d ( xx_kapgm_file,
     &     xx_kapgm_dummy, wkapgmFld, dodimensionalcost3D,
     &     num_kapgm, objf_kapgm, maskC, myThid )
      call timer_stop ('cost_kapgm      [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

c--   REDI coeffs penalty term
#ifdef ALLOW_KAPREDI_COST_CONTRIBUTION
      call timer_start('cost_kapredi      [ECCO SPIN-DOWN]', mythid)
      call ctrl_cost_gen3d ( xx_kapredi_file,
     &     xx_kapredi_dummy, wkaprediFld, dodimensionalcost3D,
     &     num_kapredi, objf_kapredi, maskC, myThid )
      call timer_stop ('cost_kapredi      [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

c--   DIFFKR coeffs penalty term
#ifdef ALLOW_DIFFKR_COST_CONTRIBUTION
      call timer_start('cost_diffkr      [ECCO SPIN-DOWN]', mythid)
      call ctrl_cost_gen3d ( xx_diffkr_file,
     &     xx_diffkr_dummy, wdiffkrFld, dodimensionalcost3D,
     &     num_diffkr, objf_diffkr, maskC, myThid )
      call timer_stop ('cost_diffkr      [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

      endif !if (.NOT.ctrlUseGen) then

c--   Bottom Drag penalty term
#ifdef ALLOW_BOTTOMDRAG_COST_CONTRIBUTION
      call timer_start('cost_bottomdrag      [ECCO SPIN-DOWN]', mythid)
      call ctrl_cost_gen2d ( 1, 1, xx_bottomdrag_file,
     &     xx_bottomdrag_dummy, zeroRL, wbottomdrag,
     &     dodimensionalcost2D, num_bottomdrag, objf_bottomdrag,
#ifdef ECCO_CTRL_DEPRECATED
     &     zeroRL, num_zero_mean, objf_zero_mean,
     &     objf_zero_smoo, zeroRL, zeroRL,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, myThid )
      call timer_stop ('cost_bottomdrag      [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

#endif

      end
