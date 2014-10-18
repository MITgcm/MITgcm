C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_hyd.F,v 1.35 2014/10/18 18:15:44 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

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
#ifdef ECCO_CTRL_DEPRECATED
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "ecco_cost.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_COST.h"
#endif
#endif

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#ifdef ECCO_CTRL_DEPRECATED
c     == local variables ==
      character*(max_len_mbuf) msgbuf

      integer nnzbar
      integer nnzobs
      integer nrecloc
      integer nmonsloc
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL localperiod

c     == end of interface ==


      IF (using_cost_sst) THEN
#if (defined (ALLOW_SST_COST_CONTRIBUTION))
      write(msgbuf,'(a)') 'ph-cost call cost_sst'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = nnztbar
      nnzobs = 1
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, tbarfile, tbar, xx_tbar_mean_dummy,
     &     nnzobs, sstdatfile, mult_sst,
     &     nmonsloc, nmonsloc, sststartdate, localperiod,
     &     'C', wsst, spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r, 
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, sstdatfile, objf_sst, num_sst,
     &     myiter, mytime, mythid )
c
#elif (defined (ALLOW_DAILYSST_COST_CONTRIBUTION))
c
      write(msgbuf,'(a)') 'ph-cost call cost_sst'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = 1
      nnzobs = 1
      nrecloc = ndaysrec
      spminloc = -2.
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 86400.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, sstbarfile, sstbar, xx_sstbar_mean_dummy,
     &     nnzobs, sstdatfile, mult_sst,
     &     nrecloc, nmonsloc, sststartdate, localperiod,
     &     'C', wsst, spminloc, spmaxloc, spzeroloc, 
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, sstdatfile, objf_sst, num_sst,
     &     myiter, mytime, mythid )
cph      call cost_sst   ( myiter, mytime, mythid )
#endif
      ENDIF

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

      IF (using_cost_sst) THEN
#ifdef ALLOW_TMI_SST_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_tmi'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = nnztbar
      nnzobs = 1
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, tbarfile, tbar, xx_tbar_mean_dummy,
     &     nnzobs, tmidatfile, mult_tmi,
     &     nmonsloc, nmonsloc, tmistartdate, localperiod,
     &     'C', wsst, spminloc, spmaxloc, spzeroloc, 
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, tmidatfile, objf_tmi, num_tmi,
     &     myiter, mytime, mythid )
cph      call cost_tmi( myiter, mytime, mythid )
#endif
      ENDIF

#ifdef ALLOW_SSS_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_sss'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = nnzsbar
      nnzobs = 1
      spminloc = 20.
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, sbarfile, sbar, xx_sbar_mean_dummy,
     &     nnzobs, sssdatfile, mult_sss,
     &     nmonsloc, nmonsloc, sssstartdate, localperiod,
     &     'C', wsss, spminloc, spmaxloc, spzeroloc, 
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, sssdatfile, objf_sss, num_sss,
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
      if (.NOT.ctrlUseGen) then
      write(msgbuf,'(a)') 'ph-cost call cost_theta0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_theta0 ( myiter, mytime, mythid )
      endif
#endif

#ifdef ALLOW_SALT0_COST_CONTRIBUTION
      if (.NOT.ctrlUseGen) then
      write(msgbuf,'(a)') 'ph-cost call cost_salt0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_salt0  ( myiter, mytime, mythid )
      endif
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

#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_sigmar'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_sigmar  ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_TRANSPORT_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_gen_transport'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_gen_transport  ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_ETAN0_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_etan0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_etan0 ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_UVEL0_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_uvel0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_uvel0 ( myiter, mytime, mythid )
#endif
#ifdef ALLOW_VVEL0_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_vvel0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      call cost_vvel0 ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_SEAICE_COST_SMR_AREA
# ifdef ALLOW_SEAICE_COST_AREASST
cph this block out of date;
cph similar call moved to seaice_cost_driver and
cph and extended using I. Fenty SST, SSS extensions

      write(msgbuf,'(a)') 'ph-cost call seaice_cost_areasst'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      nnzbar = 1
      nnzobs = 1
      nrecloc = ndaysrec
      spminloc = 0.
      spmaxloc = 1.
      spzeroloc = -9999.
      localperiod = 86400.
      call seaice_cost_areasst(
     &     nnzbar, smrareabarfile, smrareabar, xx_smrareabar_mean_dummy,
     &     nnzobs, smrareadatfile, smrareadat, mult_smrarea,
     &     nrecloc, smrareastartdate, localperiod,
     &     maskC, wsmrarea,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_smrarea, num_smrarea,
     &     myiter, mytime, mythid )

      write(msgbuf,'(a,1P2E18.10)')
     & 'ph-cost seaice_cost_areasst ',
     & objf_smrarea(1,1), num_smrarea(1,1)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

# endif /* ALLOW_SEAICE_COST_AREASST */
#endif

#endif /* ECCO_CTRL_DEPRECATED */

      end
