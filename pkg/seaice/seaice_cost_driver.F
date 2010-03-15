C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_driver.F,v 1.11 2010/03/15 18:31:11 heimbach Exp $
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

      if (useSEAICE) then

#ifdef ALLOW_SEAICE_COST_SMR_AREA

      write(msgbuf,'(a)') 'ph-cost call cost_smrarea'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = 1
      nnzobs = 1

      nrecloc = ndaysrec
      spminloc = 0.0
      spmaxloc = 1.0
      spzeroloc = -1.
      localperiod = 86400.

      call seaice_cost_concentration(
     &     nnzbar, smrareabarfile, smrareabar, xx_smrareabar_mean_dummy,
     &     nnzobs, smrareadatfile, smrareadat, mult_smrarea,
     &     nrecloc, modelstartdate, localperiod,
     &     maskC, wsmrarea,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_smrarea, num_smrarea,
     &     myiter, mytime, mythid )

c read the area dat file and compare against the averaged sst file
      call seaice_cost_sst(
     &     nnzbar, smrsstbarfile,  smrsstbar, xx_smrsstbar_mean_dummy,
     &             smrareabarfile, smrareabar, xx_smrareabar_mean_dummy,
     &     nnzobs, smrareadatfile, smrareadat, mult_smrsst,
     &     nrecloc, modelstartdate, localperiod,
     &     maskC, wsmrarea,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_smrsst, num_smrsst,
     &     myiter, mytime, mythid )

c read the area dat file and compare against the averaged salinity file
      call seaice_cost_sss(
     &     nnzbar, smrsssbarfile, smrsssbar, xx_smrsssbar_mean_dummy,
     &             smrareabarfile, smrareabar, xx_smrareabar_mean_dummy,
     &     nnzobs, smrareadatfile, smrareadat, mult_smrsss,
     &     nrecloc, modelstartdate, localperiod,
     &     maskC, wsmrarea,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_smrsss, num_smrsss,
     &     myiter, mytime, mythid )

      print *,' SICD cost smrarea/sst/sss ',objf_smrarea,
     &          objf_smrsst,objf_smrsss
      print *,' SICD num points smrarea/sst/sss ',num_smrarea,
     &          num_smrsst,num_smrsss

# ifndef ALLOW_SEAICE_COST_AREASST
c--      write(msgbuf,'(a)') 'ph-cost call cost_smrarea'
c--      call print_message( msgbuf, standardmessageunit,
c--     &     SQUEEZE_RIGHT , mythid)
c--      nnzbar = 1
c--      nnzobs = 1
c--cph(
c--      nrecloc = ndaysrec
c--cph hard-coding for ECCO
c--cph      nrecloc = 4383
c--cph)
c--      spminloc = 0.
c--      spmaxloc = 1.
c--      spzeroloc = -9999.
c--      localperiod = 86400.
c--      call cost_generic(
c--     &     nnzbar, smrareabarfile, smrareabar, xx_smrareabar_mean_dummy,
c--     &     nnzobs, smrareadatfile, mult_smrarea,
c--     &     nrecloc, modelstartdate, localperiod,
c--     &     'C', wsmrarea,
c--     &     spminloc, spmaxloc, spzeroloc,
c--     &     objf_smrarea, num_smrarea,
c--     &     myiter, mytime, mythid )

c--      write(msgbuf,'(a,1P2E18.10)')
c--     &     'ph-cost smrarea ', objf_smrarea(1,1), num_smrarea(1,1)
c--      call print_message( msgbuf, standardmessageunit,
c--     &     SQUEEZE_RIGHT , mythid)
# endif /* ALLOW_SEAICE_COST_AREASST */

#endif

      endif

#endif /* ALLOW_ECCO */

      end
