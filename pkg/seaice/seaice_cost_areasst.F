C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_areasst.F,v 1.8 2014/10/20 03:20:57 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_COST
#include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
#include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
#include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

      subroutine seaice_cost_areasst(
     &     nnzbar, localbarfile, localbar, xx_localbar_mean_dummy,
     &     nnzobs, localobsfile, localobs, mult_local,
     &     nrecloc, localstartdate, localperiod,
     &     localmask, localweight,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_local, num_local,
     &     myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE seaice_cost_areasst
c     ==================================================================
c
c     o Based on cost_generic
c     o in case where there is observed sea-ice we not only constrain
c       model(area)=obs(area) but also model(sst)@freezing point
c
c     ==================================================================
c     SUBROUTINE seaice_cost_areasst
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_COST.h"
#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#endif
#ifdef ALLOW_CTRL
# include "optim.h"
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif

c     == routine arguments ==

      integer nnzbar
      integer nnzobs
      integer nrecloc
      integer myiter
      integer mythid
      integer localstartdate(4)

      _RL localbar   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL localobs   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL localweight(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL localmask  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xx_localbar_mean_dummy
      _RL mult_local
      _RL mytime
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL objf_local(nsx,nsy)
      _RL num_local(nsx,nsy)

      character*(MAX_LEN_FNAM) localbarfile
      character*(MAX_LEN_FNAM) localobsfile

#ifdef ALLOW_ECCO
#ifdef ALLOW_COST
c     == local variables ==

#endif /* ifdef ALLOW_COST */
#endif /* ifdef ALLOW_ECCO */

      return
      end
