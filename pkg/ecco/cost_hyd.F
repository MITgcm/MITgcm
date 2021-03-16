#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

      subroutine cost_hyd( myIter, myTime, myThid )

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

      integer myIter
      _RL     myTime
      integer myThid

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

      logical dodimensionalcost2D, dodimensionalcost3D

c     == end of interface ==

      dodimensionalcost2D=.NOT.ctrlSmoothCorrel2D
      dodimensionalcost3D=.NOT.ctrlSmoothCorrel3D

      IF (using_cost_sst) THEN
#if (defined (ALLOW_SST_COST_CONTRIBUTION))
      write(msgbuf,'(a)') 'ph-cost call cost_sst'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = nnztbar
      nnzobs = 1
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, tbarfile, xx_tbar_mean_dummy,
     &     nnzobs, sstdatfile, ssterrfile, mult_sst,
     &     nmonsloc, nmonsloc, sststartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, sstdatfile, objf_sst, num_sst,
     &     myIter, myTime, myThid )
c
#elif (defined (ALLOW_DAILYSST_COST_CONTRIBUTION))
c
      write(msgbuf,'(a)') 'ph-cost call cost_sst'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = 1
      nnzobs = 1
      nrecloc = ndaysrec
      spminloc = -2.
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 86400.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, sstbarfile, xx_sstbar_mean_dummy,
     &     nnzobs, sstdatfile, ssterrfile, mult_sst,
     &     nrecloc, nmonsloc, sststartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, sstdatfile, objf_sst, num_sst,
     &     myIter, myTime, myThid )
cph      call cost_sst   ( myIter, myTime, myThid )
#endif
      ENDIF

#ifdef ALLOW_ARGO_THETA_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_argo_theta'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = Nr
      nnzobs = Nr
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, tbarfile, xx_tbar_mean_dummy,
     &     nnzobs, argotfile, temperrfile, mult_argot,
     &     nmonsloc, nmonsloc, argotstartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, argotfile, objf_argot, num_argot,
     &     myIter, myTime, myThid )
#endif

#ifdef ALLOW_CTDT_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_ctdt'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = Nr
      nnzobs = Nr
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, tbarfile, xx_tbar_mean_dummy,
     &     nnzobs, ctdtfile, temperrfile, mult_ctdt,
     &     nmonsloc, nmonsloc, ctdtstartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, ctdtfile, objf_ctdt, num_ctdt,
     &     myIter, myTime, myThid )
#endif

#ifdef ALLOW_XBT_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_xbt'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = Nr
      nnzobs = Nr
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, tbarfile, xx_tbar_mean_dummy,
     &     nnzobs, xbtfile, temperrfile, mult_xbt,
     &     nmonsloc, nmonsloc, xbtstartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, xbtfile, objf_xbt, num_xbt,
     &     myIter, myTime, myThid )
#endif

      IF (using_cost_sst) THEN
#ifdef ALLOW_TMI_SST_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_tmi'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = nnztbar
      nnzobs = 1
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, tbarfile, xx_tbar_mean_dummy,
     &     nnzobs, tmidatfile, ssterrfile, mult_tmi,
     &     nmonsloc, nmonsloc, tmistartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, tmidatfile, objf_tmi, num_tmi,
     &     myIter, myTime, myThid )
cph      call cost_tmi( myIter, myTime, myThid )
#endif
      ENDIF

#ifdef ALLOW_SSS_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_sss'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = nnzsbar
      nnzobs = 1
      spminloc = 20.
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, sbarfile, xx_sbar_mean_dummy,
     &     nnzobs, sssdatfile, ssterrfile, mult_sss,
     &     nmonsloc, nmonsloc, sssstartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, sssdatfile, objf_sss, num_sss,
     &     myIter, myTime, myThid )
cph      call cost_sss   ( myIter, myTime, myThid )
#endif

#ifdef ALLOW_CTDS_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_ctds'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = Nr
      nnzobs = Nr
      spminloc = 20.
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, sbarfile, xx_sbar_mean_dummy,
     &     nnzobs, ctdsfile, salterrfile, mult_ctds,
     &     nmonsloc, nmonsloc, ctdsstartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, ctdsfile, objf_ctds, num_ctds,
     &     myIter, myTime, myThid )
#endif

#ifdef ALLOW_ARGO_SALT_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_argo_salt'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = Nr
      nnzobs = Nr
      spminloc = 20.
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, sbarfile, xx_sbar_mean_dummy,
     &     nnzobs, argosfile, salterrfile, mult_argos,
     &     nmonsloc, nmonsloc, argosstartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     no_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, argosfile, objf_argos, num_argos,
     &     myIter, myTime, myThid )
#endif

#ifdef ALLOW_THETA0_COST_CONTRIBUTION
      if (.NOT.ctrlUseGen) then
      write(msgbuf,'(a)') 'ph-cost call cost_theta0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      call ctrl_cost_gen3d ( xx_theta_file,
     &     xx_theta_dummy, wthetaLev, dodimensionalcost3D,
     &     num_temp0, objf_temp0, maskC, myThid )
      endif
#endif

#ifdef ALLOW_SALT0_COST_CONTRIBUTION
      if (.NOT.ctrlUseGen) then
      write(msgbuf,'(a)') 'ph-cost call cost_salt0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      call ctrl_cost_gen3d ( xx_salt_file,
     &     xx_salt_dummy, wsaltLev, dodimensionalcost3D,
     &     num_salt0, objf_salt0, maskC, myThid )
      endif
#endif

#ifdef ALLOW_THETA_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_theta'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = Nr
      nnzobs = Nr
      spminloc = -1.8
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, tbarfile, xx_tbar_mean_dummy,
     &     nnzobs, tdatfile, temperrfile, mult_temp,
     &     nmonsloc, nmonsloc, modelstartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     clim_preproc, no_preproc_c, clim_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, tdatfile, objf_temp, num_temp,
     &     myIter, myTime, myThid )
#endif

#ifdef ALLOW_SALT_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_salt'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      nnzbar = Nr
      nnzobs = Nr
      spminloc = 25.
      spmaxloc = 40.
      spzeroloc = 0.
      localperiod = 0.
      nmonsloc=nmonsrec
      call cost_generic(
     &     nnzbar, sbarfile, xx_sbar_mean_dummy,
     &     nnzobs, sdatfile, salterrfile, mult_salt,
     &     nmonsloc, nmonsloc, modelstartdate, localperiod,
     &     'C', spminloc, spmaxloc, spzeroloc,
     &     clim_preproc, no_preproc_c, clim_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, sdatfile, objf_salt, num_salt,
     &     myIter, myTime, myThid )
#endif

c#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
c      write(msgbuf,'(a)') 'ph-cost call cost_sigmar'
c      call print_message( msgbuf, standardmessageunit,
c     &     SQUEEZE_RIGHT , myThid)
c      call cost_sigmar  ( myIter, myTime, myThid )
c#endif

c#ifdef ALLOW_TRANSPORT_COST_CONTRIBUTION
c      write(msgbuf,'(a)') 'ph-cost call cost_gen_transport'
c      call print_message( msgbuf, standardmessageunit,
c     &     SQUEEZE_RIGHT , myThid)
c      call cost_gen_transport  ( myIter, myTime, myThid )
c#endif

#ifdef ALLOW_ETAN0_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_etan0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      call ctrl_cost_gen2d (
     &     1,1,xx_etan_file,xx_etan_dummy,zeroRL,
     &     wetan, dodimensionalcost2D, num_etan0, objf_etan0,
#ifdef ECCO_CTRL_DEPRECATED
     &     zeroRL, num_zero_mean, objf_zero_mean,
     &     objf_zero_smoo, zeroRL, zeroRL,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, myThid )
#endif

#ifdef ALLOW_UVEL0_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_uvel0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      call ctrl_cost_gen3d (
     &     xx_uvel_file,xx_uvel_dummy, wuvel3d, dodimensionalcost3D,
     &     num_uvel0, objf_uvel0, maskW, myThid )
#endif
#ifdef ALLOW_VVEL0_COST_CONTRIBUTION
      write(msgbuf,'(a)') 'ph-cost call cost_vvel0'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
      call ctrl_cost_gen3d (
     &     xx_vvel_file,xx_vvel_dummy, wvvel3d, dodimensionalcost3D,
     &     num_vvel0, objf_vvel0, maskS, myThid )
#endif

#ifdef ALLOW_SEAICE_COST_SMR_AREA
# ifdef ALLOW_SEAICE_COST_AREASST
cph this block out of date;
cph similar call moved to seaice_cost_driver and
cph and extended using I. Fenty SST, SSS extensions

      write(msgbuf,'(a)') 'ph-cost call seaice_cost_areasst'
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)
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
     &     myIter, myTime, myThid )

      write(msgbuf,'(a,1P2E18.10)')
     & 'ph-cost seaice_cost_areasst ',
     & objf_smrarea(1,1), num_smrarea(1,1)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , myThid)

# endif /* ALLOW_SEAICE_COST_AREASST */
#endif

#endif /* ECCO_CTRL_DEPRECATED */

      RETURN
      END
