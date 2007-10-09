C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_driver.F,v 1.8 2007/10/09 00:10:13 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_driver( myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE seaice_cost_driver
c     ==================================================================
c
c     o Evaluate cost function contributions of
c       seaice observations
c
c     ==================================================================
c     SUBROUTINE seaice_cost_driver
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_COST
# include "ctrl.h"
# include "ctrl_dummy.h"
# ifdef ALLOW_ECCO
#  include "ecco_cost.h"
# endif
# include "SEAICE_COST.h"
#endif

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#ifdef ALLOW_ECCO
c     == local variables ==
      character*(max_len_mbuf) msgbuf

      integer nnzbar
      integer nnzobs
      integer nrecloc
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL localperiod

c     == end of interface ==

cph      if (useSEAICE) then

#ifdef ALLOW_SEAICE_COST_SMR_AREA

      write(msgbuf,'(a)') 'ph-cost call cost_smrarea'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = 1
      nnzobs = 1
cph(
      nrecloc = ndaysrec
cph hard-coding for ECCO
cph      nrecloc = 4383
cph)
      spminloc = 0.
      spmaxloc = 1.
      spzeroloc = -9999.
      localperiod = 86400.
      call cost_generic(
     &     nnzbar, smrareabarfile, smrareabar, xx_smrareabar_mean_dummy,
     &     nnzobs, smrareadatfile, smrareadat, mult_smrarea,
     &     nrecloc, modelstartdate, localperiod,
     &     maskC, wsmrarea,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_smrarea, num_smrarea,
     &     myiter, mytime, mythid )

      write(msgbuf,'(a,1P2E18.10)')
     &     'ph-cost smrarea ', objf_smrarea(1,1), num_smrarea(1,1)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

#endif

cph      endif

#endif /* ALLOW_ECCO */

      end
