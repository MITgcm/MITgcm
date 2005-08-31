C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_hyd.F,v 1.4 2005/08/31 19:00:52 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_hyd( myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_hyd
c     ==================================================================
c
c     o Evaluate cost function contributions of temperature, salt, and
c       sea surface temperature contributions.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_hyd
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

c     == local variables ==
      character*(max_len_mbuf) msgbuf

      integer nnzbar
      integer nnzobs
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL localperiod

c     == end of interface ==


#ifdef ALLOW_SST_COST_CONTRIBUTION 
      write(msgbuf,'(a)') 'ph-cost call cost_sst'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = nr
      nnzobs = 1
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      call cost_generic(
     &     nnzbar, tbarfile, tbar, xx_tbar_mean_dummy,
     &     nnzobs, sstdatfile, sstdat,
     &     nmonsrec, modelstartdate, localperiod,
     &     maskC, wsst, 
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_sst, num_sst,
     &     myiter, mytime, mythid )
cph      call cost_sst   ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_ARGO_THETA_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_argo_theta'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_argo_theta ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_CTDT_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_ctdt'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_ctdt  ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_XBT_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_xbt'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_xbt   ( myiter, mytime, mythid )
#endif 

#ifdef ALLOW_TMI_SST_COST_CONTRIBUTION 
      write(msgbuf,'(a)') 'ph-cost call cost_tmi'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = nr
      nnzobs = 1
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      call cost_generic(
     &     nnzbar, tbarfile, tbar, xx_tbar_mean_dummy,
     &     nnzobs, sstdatfile, sstdat,
     &     nmonsrec, modelstartdate, localperiod,
     &     maskC, wsst, 
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_tmi, num_tmi,
     &     myiter, mytime, mythid )
cph      call cost_tmi( myiter, mytime, mythid )
#endif

#ifdef ALLOW_SSS_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_sss'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = nr
      nnzobs = 1
      spminloc = 20.
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      call cost_generic(
     &     nnzbar, sbarfile, sbar, xx_sbar_mean_dummy,
     &     nnzobs, sssdatfile, sssdat,
     &     nmonsrec, modelstartdate, localperiod,
     &     maskC, wsss, 
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_sss, num_sss,
     &     myiter, mytime, mythid )
cph      call cost_sss   ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_CTDS_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_ctds'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_ctds  ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_ARGO_SALT_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_argo_salt'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_argo_salt ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_THETA0_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_theta0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_theta0 ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_SALT0_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_salt0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_salt0  ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_THETA_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_theta'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_theta ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_SALT_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_salt'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_salt  ( myiter, mytime, mythid )
#endif

      end
