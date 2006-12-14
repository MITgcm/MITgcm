C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/the_main_loop.F,v 1.49 2006/12/14 22:31:18 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

      subroutine the_main_loop( myTime, myIter, mythid )

c     ==================================================================
c     SUBROUTINE the_main_loop
c     ==================================================================
c
c     o Run the ocean model and evaluate the specified cost function.
c
c     *the_main_loop* is the top-level routine for the Tangent Linear and
c     Adjoint Model Compiler (TAMC). For this purpose, the initialization
c     of the model was split into two parts. Those parameters that do
c     not depend on a specific model run are set in *initialise_fixed*,
c     whereas those that do depend on the specific realization are
c     initialized in *initialise_varia*. In order to do a so called
c     checkpointing during the adjoint calculation and to account for the
c     typical data involved in oceanographic applications a call tree
c     that is divided into yearly, monthly, daily, and step parts can
c     be used.
c
c     This routine is to be used in conjuction with the MITgcmuv release
c     checkpoint 24.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 14-Jul-1999
c
c              - The call to mapping was moved to initialise_varia,
c                since this routine has to be called before 
c                ini_predictor.
c
c              Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Patrick Heimbach heimbach@mit.edu 3-Jun-2000
c              - corrected computation of ikey_dynamics and
c                added computation of ikey_dynamics for the case
c                undef ALLOW_TAMC_CHECKPOINTING
c
c              Patrick Heimbach heimbach@mit.edu 6-Jun-2000
c              - corrected initialisation of comlev1 common blocks
c
c              Dimitris Menemenlis  menemenlis@jpl.nasa.gov 26-Feb-2003
c              - modifications for pkg/seaice
c
c     ==================================================================
c     SUBROUTINE the_main_loop
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE
#endif

#ifdef HAVE_SIGREG
#include "SIGREG.h"
#endif

#ifdef ALLOW_SHAP_FILT
# include "SHAP_FILT.h"
#endif
#ifdef ALLOW_ZONAL_FILT
# include "ZONAL_FILT.h"
#endif
#ifdef COMPONENT_MODULE
# include "CPL_PARAMS.h"
#endif

c**************************************
#ifdef ALLOW_AUTODIFF_TAMC

c These includes are needed for 
c AD-checkpointing. 
c They provide the fields to be stored.

# include "GRID.h"
# include "DYNVARS.h"
# include "FFIELDS.h"
# include "EOS.h"

# ifdef ALLOW_GENERIC_ADVDIFF
#  include "GAD.h"
# endif
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS.h"
# endif
# ifdef ALLOW_NONHYDROSTATIC
#  include "CG3D.h"
# endif
# if (defined (EXACT_CONSERV) || defined (NONLIN_FRSURF))
#  include "SURFACE.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS.h"
# endif
# ifdef ALLOW_EXF
#  include "exf_fields.h"
#  include "exf_clim_fields.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "exf_constants.h"
#  endif
# endif /* ALLOW_EXF */
# ifdef ALLOW_SEAICE
#  include "SEAICE.h"
#  include "SEAICE_COST.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
# ifdef ALLOW_RBCS
#  include "RBCS.h"
# endif
# ifdef ALLOW_PROFILES
#  include "profiles.h"
# endif
# ifdef ALLOW_DIVIDED_ADJOINT_MPI
#  include "mpif.h"
# endif

# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"
# include "ecco_cost.h"

#endif /* ALLOW_AUTODIFF_TAMC */
c**************************************

c     == routine arguments ==
c     note: under the multi-threaded model myiter and 
c           mytime are local variables passed around as routine 
c           arguments. Although this is fiddly it saves the need to 
c           impose additional synchronisation points when they are 
c           updated.
c     myiter - iteration counter for this thread
c     mytime - time counter for this thread
c     mythid - thread number for this instance of the routine.
      integer mythid 
      integer myiter
      _RL     mytime 

c     == local variables ==

      integer bi,bj
      integer iloop
      integer mydate(4)
#ifdef ALLOW_SNAPSHOTS
      character yprefix*3
#endif

#ifdef ALLOW_TAMC_CHECKPOINTING
      integer ilev_1
      integer ilev_2
      integer ilev_3
      integer ilev_4
      integer max_lev2
      integer max_lev3
      integer max_lev4
#endif

#if defined(TIME_PER_TIMESTEP) || defined(USE_PAPI_FLOPS) || defined(USE_PCL_FLOPS)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef TIME_PER_TIMESTEP
CCE107 common block for per timestep timing
C     !TIMING VARIABLES
C     == Timing variables ==
      REAL*8 utnew, utold, stnew, stold, wtnew, wtold
      DATA utnew, utold, stnew, stold, wtnew, wtold /6*0.0D0/
#endif
#ifdef USE_PAPI_FLOPS
CCE107 common block for PAPI summary performance
#include <fpapi.h>
      INTEGER*8 flpops, instr
      DATA flpops, instr /2*0/
      INTEGER check
      REAL*4 real_time, proc_time, mflops, ipc
      DATA real_time, proc_time, mflops, ipc /4*0.0E0/
#else
#ifdef USE_PCL_FLOPS
CCE107 common block for PCL summary performance
#include <pclh.f>
      INTEGER pcl_counter_list(5), flags, nevents, res, ipcl
      INTEGER*8 i_result(5), descr
      REAL*8 fp_result(5)
      COMMON /pclvars/ i_result, descr, fp_result, pcl_counter_list,
     $     flags, nevents 
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 61)
      CHARACTER*22 pcl_counter_name(0:nmaxevents-1)
      COMMON /pclnames/ pcl_counter_name
#endif
#endif
#endif
c--   == end of interface ==

#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_ENTER('THE_MAIN_LOOP',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
c--   Initialize storage for the initialisations.
CADJ INIT tapelev_ini_bibj_k   = USER
CADJ INIT tapelev_init = USER
c
#if (defined (AUTODIFF_2_LEVEL_CHECKPOINT))
CADJ  INIT tapelev2 = USER
#elif (defined (AUTODIFF_4_LEVEL_CHECKPOINT))
CADJ  INIT tapelev4 = USER
#else
CADJ  INIT tapelev3 = USER
#endif
c
# ifdef ALLOW_DIVIDED_ADJOINT
CADJ INIT onetape = user
cphCADJ INIT onetape = common, 1
cph We want to avoid common blocks except in the inner loop.
cph Reason: the active write and consecutive read may occur
cph         in separate model executions for which the info
cph         in common blocks are lost.
cph Thus, we can only store real values (no integers)
cph because we only have active file handling to real available.
# endif
# ifdef ALLOW_TAMC_CHECKPOINTING
       ikey_dynamics = 1
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

      CALL TIMER_START('ECCO SPIN-UP', mythid)

#ifdef ALLOW_CAL
c--   Get the current date.
      call CAL_TIMESTAMP( myiter, mytime, mydate, mythid )
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
CADJ STORE hFacC = tapelev_init, key = 1
# endif
#endif

C--   Set initial conditions (variable arrays)
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('INITIALISE_VARIA',myThid)
#endif
      CALL TIMER_START('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)
      CALL INITIALISE_VARIA( mythid )
      CALL TIMER_STOP ('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)

#ifdef ALLOW_MONITOR
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('MONITOR',myThid)
#endif
C--   Check status of solution (statistics, cfl, etc...)
      CALL TIMER_START('MONITOR             [THE_MAIN_LOOP]', mythid)
      CALL MONITOR( myIter, myTime, myThid )
      CALL TIMER_STOP ('MONITOR             [THE_MAIN_LOOP]', mythid)
#endif /* ALLOW_MONITOR */

C--   Do IO if needed (Dump for start state).
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('DO_THE_MODEL_IO',myThid)
#endif

#ifdef ALLOW_OFFLINE
      CALL TIMER_START('OFFLINE_MODEL_IO     [FORWARD_STEP]',myThid)
      CALL OFFLINE_MODEL_IO( myTime, myIter, myThid )
      CALL TIMER_STOP ('OFFLINE_MODEL_IO     [FORWARD_STEP]',myThid)
#else
      CALL TIMER_START('DO_THE_MODEL_IO     [THE_MAIN_LOOP]', mythid)
      CALL DO_THE_MODEL_IO( myTime, myIter, mythid )
      CALL TIMER_STOP ('DO_THE_MODEL_IO     [THE_MAIN_LOOP]', mythid)
#endif

      call timer_stop ('ECCO SPIN-UP', mythid)
      _BARRIER

#ifdef TIME_PER_TIMESTEP
CCE107 Initial call for timers
      _BEGIN_MASTER( myThid )
      CALL TIMER_GET_TIME( utold, stold, wtold )
      _END_MASTER( myThid )
#endif
#ifdef USE_PAPI_FLOPS
CCE107 Initial call for PAPI
      _BEGIN_MASTER( myThid )
#ifdef USE_FLIPS
      call PAPIF_flips(real_time, proc_time, flpops, mflops, check)
#else
      call PAPIF_flops(real_time, proc_time, flpops, mflops, check)
#endif
      WRITE(msgBuf,'(A34,F10.6,A,F10.6)')
     $     'Mflop/s before timestepping:', mflops, ' ', mflops*proc_time
     $     /(real_time + 1E-36)
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#ifdef PAPI_VERSION
      call PAPIF_ipc(real_time, proc_time, instr, ipc, check)
      WRITE(msgBuf,'(A34,F10.6,A,F10.6)')
     $     'IPC before timestepping:', ipc, ' ', ipc*proc_time
     $     /(real_time + 1E-36)
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#endif
      _END_MASTER( myThid )
#else
#ifdef USE_PCL_FLOPS
CCE107 Initial call for PCL
      _BEGIN_MASTER( myThid )
      res = PCLstop(descr, i_result, fp_result, nevents)
      do ipcl = 1, nevents
         WRITE(msgBuf,'(A22,A26,F10.6)'),
     $        pcl_counter_name(pcl_counter_list(ipcl)),
     $        'before timestepping:', fp_result(ipcl)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      enddo
      res = PCLstart(descr, pcl_counter_list, nevents, flags)
      _END_MASTER( myThid )
#endif
#endif

c--   Do the model integration.
      call timer_start('ECCO MAIN LOOP',mythid)

c     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STARTS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_TAMC_CHECKPOINTING

      max_lev4=nTimeSteps/(nchklev_1*nchklev_2*nchklev_3)+1
      max_lev3=nTimeSteps/(nchklev_1*nchklev_2)+1
      max_lev2=nTimeSteps/nchklev_1+1

c**************************************
#ifdef ALLOW_DIVIDED_ADJOINT
CADJ loop = divided
#endif
c**************************************

#ifdef AUTODIFF_4_LEVEL_CHECKPOINT
      do ilev_4 = 1,nchklev_4
         if(ilev_4.le.max_lev4) then
c**************************************
#include "checkpoint_lev4_directives.h"
c**************************************
c--     Initialise storage for the middle loop.
CADJ    INIT tapelev3 = USER
#endif /* AUTODIFF_4_LEVEL_CHECKPOINT */

#ifndef AUTODIFF_2_LEVEL_CHECKPOINT
      do ilev_3 = 1,nchklev_3
         if(ilev_3.le.max_lev3) then
c**************************************
#include "checkpoint_lev3_directives.h"
c**************************************
c--     Initialise storage for the middle loop.
CADJ    INIT tapelev2 = USER
#endif /* AUTODIFF_2_LEVEL_CHECKPOINT */

        do ilev_2 = 1,nchklev_2
         if(ilev_2.le.max_lev2) then
c**************************************
#include "checkpoint_lev2_directives.h"
c**************************************

c**************************************
#ifdef ALLOW_AUTODIFF_TAMC
c--       Initialize storage for the innermost loop.
c--       Always check common block sizes for the checkpointing!
c--
CADJ INIT comlev1        = COMMON,nchklev_1
CADJ INIT comlev1_bibj   = COMMON,nchklev_1*nsx*nsy*nthreads_chkpt
CADJ INIT comlev1_bibj_k = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt
c--
# ifdef ALLOW_KPP
CADJ INIT comlev1_kpp    = COMMON,nchklev_1*nsx*nsy
CADJ INIT comlev1_kpp_k  = COMMON,nchklev_1*nsx*nsy*nr
# endif /* ALLOW_KPP */
c--
# ifdef ALLOW_GMREDI
CADJ INIT comlev1_gmredi_k_gad
CADJ &    = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass
# endif /* ALLOW_GMREDI */
c--
# ifdef ALLOW_PTRACERS
CADJ INIT comlev1_bibj_ptracers = COMMON,
CADJ &    nchklev_1*nsx*nsy*nthreads_chkpt*PTRACERS_num
CADJ INIT comlev1_bibj_k_ptracers = COMMON,
CADJ &    nchklev_1*nsx*nsy*nthreads_chkpt*PTRACERS_num*nr
# endif /* ALLOW_PTRACERS */
c--
cph Now also needed by seaice
cph# ifndef DISABLE_MULTIDIM_ADVECTION
CADJ INIT comlev1_bibj_k_gad
CADJ &    = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass
CADJ INIT comlev1_bibj_k_gad_pass
CADJ &    = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass*maxcube
cph# endif /* DISABLE_MULTIDIM_ADVECTION */
c--
# if (defined (ALLOW_EXF) && defined (ALLOW_BULKFORMULAE))
CADJ INIT comlev1_exf_1
CADJ &     = COMMON,nchklev_1*snx*nsx*sny*nsy*nthreads_chkpt
CADJ INIT comlev1_exf_2
CADJ &     = COMMON,niter_bulk*nchklev_1*snx*nsx*sny*nsy*nthreads_chkpt
# endif
c--
# ifdef ALLOW_SEAICE
#  ifdef SEAICE_ALLOW_DYNAMICS
CADJ INIT comlev1_lsr = COMMON,nchklev_1*2
#  endif
#  ifdef SEAICE_MULTILEVEL
CADJ INIT comlev1_multdim 
CADJ &    = COMMON,nchklev_1*nsx*nsy*nthreads_chkpt*multdim
#  endif
# endif /* ALLOW_SEAICE */
c--
#endif /* ALLOW_AUTODIFF_TAMC */
c**************************************

          do ilev_1 = 1,nchklev_1

c--         The if-statement below introduces a some flexibility in the
c--         choice of the 3-tupel ( nchklev_1, nchklev_2, nchklev_3 ).
c--
c--         Requirement: nchklev_1*nchklev_2*nchklev_3 .ge. nTimeSteps .

            iloop = (ilev_2 - 1)*nchklev_1                     + ilev_1
#ifndef AUTODIFF_2_LEVEL_CHECKPOINT
     &            + (ilev_3 - 1)*nchklev_2*nchklev_1
#endif
#ifdef AUTODIFF_4_LEVEL_CHECKPOINT
     &            + (ilev_4 - 1)*nchklev_3*nchklev_2*nchklev_1
#endif

            if ( iloop .le. nTimeSteps ) then

#else /* ALLOW_TAMC_CHECKPOINTING  undefined */
c--   Initialise storage for the reference trajectory without TAMC check-
c--   pointing.
CADJ INIT history        = USER
CADJ INIT comlev1_bibj   = COMMON,nchklev_0*nsx*nsy*nthreads_chkpt
CADJ INIT comlev1_bibj_k = COMMON,nchklev_0*nsx*nsy*nr*nthreads_chkpt
CADJ INIT comlev1_kpp    = COMMON,nchklev_0*nsx*nsy

c--   Check the choice of the checkpointing parameters in relation
c--   to nTimeSteps: (nchklev_0 .ge. nTimeSteps)
      if (nchklev_0 .lt. nTimeSteps) then
        print*
        print*, ' the_main_loop: ',
     &       'TAMC checkpointing parameter nchklev_0 = ', 
     &       nchklev_0
        print*, '            is not consistent with nTimeSteps = ', 
     &       nTimeSteps
        stop    ' ... stopped in the_main_loop.'
      endif

      do iloop = 1, nTimeSteps

#endif /* ALLOW_TAMC_CHECKPOINTING */

#else /* ALLOW_AUTODIFF_TAMC  undefined */
c--   Start the main loop of ecco_Objfunc. Automatic differentiation is
c--   NOT enabled.
      do iloop = 1, nTimeSteps
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_TAMC_CHECKPOINTING
        nIter0 = NINT( (startTime-baseTime)/deltaTClock )
        ikey_dynamics = ilev_1
#endif

c--     Set the model iteration counter and the model time.
        myiter = nIter0 + (iloop-1)
        mytime = startTime + float(iloop-1)*deltaTclock

#ifdef ALLOW_AUTODIFF_TAMC
        CALL AUTODIFF_INADMODE_UNSET( myThid )
#endif

#ifdef ALLOW_DIAGNOSTICS
C--   State-variables diagnostics
      IF ( useDiagnostics ) THEN
C--   Switch on/off diagnostics for snap-shot output:
        CALL DIAGNOSTICS_SWITCH_ONOFF( myTime, myIter, myThid )
        CALL TIMER_START('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
        CALL DO_STATEVARS_DIAGS( myTime, 0, myIter, myThid )
        CALL TIMER_STOP ('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_PROFILES
c--     Accumulate in-situ time averages of temperature, salinity, and SSH.
        call timer_start('PROFILES_INLOOP       [ECCO MAIN]', mythid)
        call profiles_inloop( mytime, mythid )
        call timer_stop ('PROFILES_INLOOP       [ECCO MAIN]', mythid)
#endif

#ifdef ALLOW_COST

c--     Accumulate time averages of temperature, salinity
        call timer_start('COST_AVERAGESFIELDS   [ECCO MAIN]', mythid)
        call cost_averagesFields( mytime, mythid )
        call timer_stop ('COST_AVERAGESFIELDS   [ECCO MAIN]', mythid)


#ifdef ALLOW_COST_ATLANTIC
c--     Compute meridional heat transport 
        call timer_start('cost_atlantic         [ECCO MAIN]', mythid)
        call cost_atlantic(  mytime, myiter,mythid )
        call timer_stop ('cost_atlantic         [ECCO MAIN]', mythid)
#endif
#endif /* ALLOW_COST */

#ifdef ALLOW_AUTODIFF_TAMC
c**************************************
#include "checkpoint_lev1_directives.h"
#include "checkpoint_lev1_template.h"
c**************************************
#endif

C--   Call driver to load external forcing fields from file
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('LOAD_FIELDS_DRIVER',myThid)
#endif
      CALL TIMER_START('LOAD_FIELDS_DRIVER  [FORWARD_STEP]',myThid)
      CALL LOAD_FIELDS_DRIVER( myTime, myIter, myThid )
      CALL TIMER_STOP ('LOAD_FIELDS_DRIVER  [FORWARD_STEP]',myThid)


#ifdef ALLOW_AUTODIFF_TAMC
# if (defined (ALLOW_AUTODIFF_MONITOR))
        CALL DUMMY_IN_STEPPING( myTime, myIter, myThid )
# endif
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_PTRACERS
cph this replaces _bibj storing of ptracer within thermodynamics
CADJ STORE ptracer  = comlev1, key = ikey_dynamics
# endif
#endif

#ifdef ALLOW_EBM
        IF ( useEBM ) THEN
# ifdef ALLOW_DEBUG
         IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('EBM',myThid)
# endif
         CALL TIMER_START('EBM                [FORWARD_STEP]',mythid)
         CALL EBM_DRIVER ( myTime, myIter, myThid )
         CALL TIMER_STOP ('EBM                [FORWARD_STEP]',mythid)
        ENDIF
#endif

C--     Step forward fields and calculate time tendency terms.

#ifdef ALLOW_DEBUG
       IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('DO_ATMOSPHERIC_PHYS',myThid)
#endif
       CALL TIMER_START('DO_ATMOSPHERIC_PHYS [FORWARD_STEP]',mythid)
       CALL DO_ATMOSPHERIC_PHYS( myTime, myIter, myThid )
       CALL TIMER_STOP ('DO_ATMOSPHERIC_PHYS [FORWARD_STEP]',mythid)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE surfaceforcingtice    = comlev1, key = ikey_dynamics
# ifdef EXACT_CONSERV
cphCADJ STORE empmr              = comlev1, key = ikey_dynamics
cphCADJ STORE pmepr              = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_PTRACERS
CADJ STORE ptracer               = comlev1, key = ikey_dynamics
# endif
# ifdef NONLIN_FRSURF
CADJ STORE hFacC              = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifndef ALLOW_OFFLINE
#ifdef ALLOW_DEBUG
       IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('DO_OCEANIC_PHYS',myThid)
#endif
       CALL TIMER_START('DO_OCEANIC_PHYS     [FORWARD_STEP]',mythid)
       CALL DO_OCEANIC_PHYS( myTime, myIter, myThid )
       CALL TIMER_STOP ('DO_OCEANIC_PHYS     [FORWARD_STEP]',mythid)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE EmPmR              = comlev1, key = ikey_dynamics
# ifdef EXACT_CONSERV
CADJ STORE pmepr              = comlev1, key = ikey_dynamics
# endif
#endif
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
cph-test
CADJ STORE hFac_surfC         = comlev1, key = ikey_dynamics
CADJ STORE hfac_surfs         = comlev1, key = ikey_dynamics
CADJ STORE hfac_surfw         = comlev1, key = ikey_dynamics
CADJ STORE hFacC, hFacS, hFacW
CADJ &     = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC, recip_hFacS, recip_hFacW
CADJ &     = comlev1, key = ikey_dynamics
c
CADJ STORE surfaceforcingu = comlev1, key = ikey_dynamics
CADJ STORE surfaceforcingv = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_GCHEM
C     GCHEM package is an interface for any bio-geochemical or
C     ecosystem model you would like to include. 
C     If GCHEM_SEPARATE_FORCING is not defined, you are 
C     responsible for computing tendency terms for passive 
C     tracers and storing them on a 3DxNumPtracers-array called
C     gchemTendency in GCHEM_CALC_TENDENCY. This tendency is then added
C     to gPtr in ptracers_forcing later-on.
C     If GCHEM_SEPARATE_FORCING is defined, you are reponsible for
C     UPDATING ptracers directly in GCHEM_FORCING_SEP. This amounts
C     to a completely separate time step that you have to implement
C     yourself (Eulerian seems to be fine in most cases).
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CAVEAT: Up to now, when GCHEM is turned on the field ptracerForcingSurf,
C     which is needed for KPP is not set properly. ptracerForcingSurf must
C     be treated differently depending on whether GCHEM_SEPARATE_FORCING
C     is define or not. TBD.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       IF ( useGCHEM ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB ) 
     &       CALL DEBUG_CALL('GCHEM_CALC_TENDENCY',myThid)
#endif
        CALL TIMER_START('GCHEM_CALC_TENDENCY [FORWARD_STEP]',myThid)
        CALL GCHEM_CALC_TENDENCY( myTime, myIter, myThid )
        CALL TIMER_STOP ('GCHEM_CALC_TENDENCY [FORWARD_STEP]',myThid)
       ENDIF
#endif /* ALLOW_GCHEM */

#ifdef ALLOW_AUTODIFF_TAMC
cph needed to be moved here from do_oceanic_physics
cph to be visible down the road
c
CADJ STORE surfaceForcingS    = comlev1, key = ikey_dynamics
CADJ STORE surfaceForcingT    = comlev1, key = ikey_dynamics
CADJ STORE surfaceForcingTice = comlev1, key = ikey_dynamics
ctest(
CADJ STORE IVDConvCount       = comlev1, key = ikey_dynamics
ctest)
# ifdef ALLOW_PTRACERS
CADJ STORE surfaceForcingPtr  = comlev1, key = ikey_dynamics
# endif
c
# ifdef ALLOW_GMREDI
CADJ STORE Kwx                = comlev1, key = ikey_dynamics
CADJ STORE Kwy                = comlev1, key = ikey_dynamics
CADJ STORE Kwz                = comlev1, key = ikey_dynamics
#  ifdef GM_BOLUS_ADVEC
CADJ STORE GM_PsiX            = comlev1, key = ikey_dynamics
CADJ STORE GM_PsiY            = comlev1, key = ikey_dynamics
#  endif
# endif
c
# ifdef ALLOW_KPP
CADJ STORE KPPghat            = comlev1, key = ikey_dynamics
CADJ STORE KPPfrac            = comlev1, key = ikey_dynamics
CADJ STORE KPPdiffKzS         = comlev1, key = ikey_dynamics
CADJ STORE KPPdiffKzT         = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
CADJ STORE etaH               = comlev1, key = ikey_dynamics
#  ifdef ALLOW_CD_CODE
CADJ STORE etanm1             = comlev1, key = ikey_dynamics
#  endif
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

      IF ( .NOT.staggerTimeStep ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('THERMODYNAMICS      [FORWARD_STEP]',mythid)
        CALL THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('THERMODYNAMICS      [FORWARD_STEP]',mythid)
C--    if not staggerTimeStep: end
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
CADJ STORE hFacC          = comlev1, key = ikey_dynamics
CADJ STORE hFacS          = comlev1, key = ikey_dynamics
CADJ STORE hFacW          = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacS    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacW    = comlev1, key = ikey_dynamics
CADJ STORE etaN           = comlev1, key = ikey_dynamics
# endif
#endif

C--   Step forward fields and calculate time tendency terms.
#ifndef ALLOW_OFFLINE
#ifndef ALLOW_AUTODIFF_TAMC
      IF ( momStepping ) THEN
#endif
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('DYNAMICS',myThid)
#endif
        CALL TIMER_START('DYNAMICS            [FORWARD_STEP]',mythid)
        CALL DYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('DYNAMICS            [FORWARD_STEP]',mythid)
#ifndef ALLOW_AUTODIFF_TAMC
      ENDIF
#endif
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
cph-test
CADJ STORE gU, gV  = comlev1, key = ikey_dynamics
# endif
#endif

C--   Update time-counter
      myIter = nIter0 + iLoop
      myTime = startTime + deltaTClock * float(iLoop)

#ifdef ALLOW_MNC
C     Update the default next iter for MNC
      IF ( useMNC ) THEN
         CALL MNC_CW_CITER_SETG( 1, 1, -1, myIter , myThid )

C        TODO: Logic should be added here so that users can specify, on
C        a per-citer-group basis, when it is time to update the
C        "current" (and not just the "next") iteration

C        TODO: the following is just a temporary band-aid (mostly, for
C        Baylor) until someone writes a routine that better handles time
C        boundaries such as weeks, months, years, etc.
         IF ( mnc_filefreq .GT. 0 ) THEN
           IF (DIFFERENT_MULTIPLE(mnc_filefreq,myTime,deltaTClock)) 
     &          THEN
             CALL MNC_CW_CITER_SETG( 1, 1, myIter, -1 , myThid )
           ENDIF
         ENDIF
       ENDIF
#endif

C--   Update geometric factors:
#ifdef NONLIN_FRSURF
C-    update hfacC,W,S and recip_hFac according to etaH(n+1) :
      IF ( nonlinFreeSurf.GT.0) THEN
       IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
# ifdef ALLOW_AUTODIFF_TAMC
cph-test
CADJ STORE hFacC    = comlev1, key = ikey_dynamics
CADJ STORE hFacS    = comlev1, key = ikey_dynamics
CADJ STORE hFacW    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacS    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacW    = comlev1, key = ikey_dynamics
# endif
        CALL TIMER_START('UPDATE_R_STAR      [FORWARD_STEP]',myThid)
        CALL UPDATE_R_STAR( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_R_STAR      [FORWARD_STEP]',myThid)
# ifdef ALLOW_AUTODIFF_TAMC
cph-test
CADJ STORE hFacC    = comlev1, key = ikey_dynamics
CADJ STORE hFacS    = comlev1, key = ikey_dynamics
CADJ STORE hFacW    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacS    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacW    = comlev1, key = ikey_dynamics
# endif
# endif /* DISABLE_RSTAR_CODE */
       ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hFac_surfC, hFac_surfS, hFac_surfW 
CADJ &     = comlev1, key = ikey_dynamics
#endif
        CALL TIMER_START('UPDATE_SURF_DR     [FORWARD_STEP]',myThid)
        CALL UPDATE_SURF_DR( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_SURF_DR     [FORWARD_STEP]',myThid)
       ENDIF
      ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
cph-test
CADJ STORE hFacC    = comlev1, key = ikey_dynamics
CADJ STORE hFacS    = comlev1, key = ikey_dynamics
CADJ STORE hFacW    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacS    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacW    = comlev1, key = ikey_dynamics
# endif
C-    update also CG2D matrix (and preconditioner)
      IF ( momStepping .AND. nonlinFreeSurf.GT.2 ) THEN
        CALL TIMER_START('UPDATE_CG2D        [FORWARD_STEP]',myThid)
        CALL UPDATE_CG2D( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_CG2D        [FORWARD_STEP]',myThid)
      ENDIF
#endif /* NONLIN_FRSURF */

C--   Apply Filters to u*,v* before SOLVE_FOR_PRESSURE
#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT .AND. shap_filt_uvStar) THEN
        CALL TIMER_START('SHAP_FILT           [FORWARD_STEP]',myThid)
        IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
          CALL SHAP_FILT_APPLY_UV( uVel,vVel,
     &                             myTime, myIter, myThid )
        ENDIF
        CALL SHAP_FILT_APPLY_UV( gU,gV,myTime,myIter,myThid)
        CALL TIMER_STOP ('SHAP_FILT           [FORWARD_STEP]',myThid)
      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT .AND. zonal_filt_uvStar) THEN
        CALL TIMER_START('ZONAL_FILT_APPLY    [FORWARD_STEP]',myThid)
        IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
          CALL ZONAL_FILT_APPLY_UV( uVel, vVel, myThid )
        ENDIF
        CALL ZONAL_FILT_APPLY_UV( gU, gV, myThid )
        CALL TIMER_STOP ('ZONAL_FILT_APPLY    [FORWARD_STEP]',myThid)
      ENDIF
#endif  

C--   Solve elliptic equation(s).
C     Two-dimensional only for conventional hydrostatic or 
C     three-dimensional for non-hydrostatic and/or IGW scheme.
#ifndef ALLOW_OFFLINE
      IF ( momStepping ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
CADJ STORE uvel, vvel
CADJ &     = comlev1, key = ikey_dynamics
CADJ STORE empmr,hfacs,hfacw
CADJ &     = comlev1, key = ikey_dynamics
# endif
#endif
        CALL TIMER_START('SOLVE_FOR_PRESSURE  [FORWARD_STEP]',myThid)
        CALL SOLVE_FOR_PRESSURE(myTime, myIter, myThid)
        CALL TIMER_STOP ('SOLVE_FOR_PRESSURE  [FORWARD_STEP]',myThid)
      ENDIF
#endif

C--   Correct divergence in flow field and cycle time-stepping momentum
c     IF ( momStepping ) THEN
#ifndef ALLOW_OFFLINE
        CALL TIMER_START('UV_CORRECTION_STEP  [FORWARD_STEP]',myThid)
        CALL MOMENTUM_CORRECTION_STEP(myTime, myIter, myThid)
        CALL TIMER_STOP ('UV_CORRECTION_STEP  [FORWARD_STEP]',myThid)
#endif
c     ENDIF

#ifdef EXACT_CONSERV
      IF (exactConserv) THEN
#ifdef ALLOW_AUTODIFF_TAMC
cph-test
cphCADJ STORE etaH          = comlev1, key = ikey_dynamics
#endif
C--   Update etaH(n+1) :
        CALL TIMER_START('UPDATE_ETAH        [FORWARD_STEP]',mythid)
        CALL UPDATE_ETAH( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_ETAH        [FORWARD_STEP]',mythid)
      ENDIF
#endif /* EXACT_CONSERV */ 

#ifdef NONLIN_FRSURF
      IF ( select_rStar.NE.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
C--   r* : compute the future level thickness according to etaH(n+1) 
        CALL TIMER_START('CALC_R_STAR       [FORWARD_STEP]',mythid)
        CALL CALC_R_STAR(etaH, myTime, myIter, myThid )
        CALL TIMER_STOP ('CALC_R_STAR       [FORWARD_STEP]',mythid)
# endif /* DISABLE_RSTAR_CODE */
      ELSEIF ( nonlinFreeSurf.GT.0) THEN
C--   compute the future surface level thickness according to etaH(n+1) 
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE etaH          = comlev1, key = ikey_dynamics
# endif
        CALL TIMER_START('CALC_SURF_DR      [FORWARD_STEP]',mythid)
        CALL CALC_SURF_DR(etaH, myTime, myIter, myThid )
        CALL TIMER_STOP ('CALC_SURF_DR      [FORWARD_STEP]',mythid)
      ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
cph-test
CADJ STORE hFac_surfC    = comlev1, key = ikey_dynamics
# endif 
#endif /* NONLIN_FRSURF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( staggerTimeStep ) THEN
C--   do exchanges of U,V (needed for multiDim) when using stagger time-step :
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('DO_STAGGER_FIELDS_EXCH.',myThid)
#endif
        CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
        CALL DO_STAGGER_FIELDS_EXCHANGES( myTime, myIter, myThid )
        CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

#ifdef ALLOW_DIAGNOSTICS
C--   State-variables diagnostics
       IF ( usediagnostics ) THEN
        CALL TIMER_START('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
        CALL DO_STATEVARS_DIAGS( myTime, 1, myIter, myThid )
        CALL TIMER_STOP ('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
       ENDIF
#endif

#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('THERMODYNAMICS      [FORWARD_STEP]',mythid)
        CALL THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('THERMODYNAMICS      [FORWARD_STEP]',mythid)

C--    if staggerTimeStep: end
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
cph This is needed because convective_adjustment calls
cph find_rho which may use pressure()
CADJ STORE totphihyd  = comlev1, key = ikey_dynamics
#endif
C--   Cycle time-stepping Tracers arrays (T,S,+pTracers)
        CALL TIMER_START('TS_CORRECTION_STEP  [FORWARD_STEP]',myThid)
        CALL TRACERS_CORRECTION_STEP(myTime, myIter, myThid)
        CALL TIMER_STOP ('TS_CORRECTION_STEP  [FORWARD_STEP]',myThid)

#ifdef ALLOW_GCHEM
C     Add separate timestepping of chemical/biological/forcing
C     of ptracers here in GCHEM_FORCING_SEP
        IF ( useGCHEM ) THEN 
#ifdef ALLOW_DEBUG
         IF ( debugLevel .GE. debLevB ) 
     &        CALL DEBUG_CALL('GCHEM_FORCING_SEP',myThid)
#endif /* ALLOW_DEBUG */
         CALL TIMER_START('GCHEM_FORCING_SEP  [FORWARD_STEP]',myThid)
         CALL GCHEM_FORCING_SEP( myTime,myIter,myThid )
         CALL TIMER_STOP ('GCHEM_FORCING_SEP  [FORWARD_STEP]',myThid)
        ENDIF	
#endif /* ALLOW_GCHEM */

C--   Do "blocking" sends and receives for tendency "overlap" terms
c     CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
c     CALL DO_GTERM_BLOCKING_EXCHANGES( myThid )
c     CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

C--   Do "blocking" sends and receives for field "overlap" terms
      CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
      CALL DO_FIELDS_BLOCKING_EXCHANGES( myThid )
      CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL TIMER_START('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
       CALL DO_STATEVARS_DIAGS( myTime, 2, myIter, myThid )
       CALL TIMER_STOP ('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_FLT
C--   Calculate float trajectories
      IF (useFLT) THEN
        CALL TIMER_START('FLOATS            [FORWARD_STEP]',myThid)
        CALL FLT_MAIN(myIter,myTime, myThid)
        CALL TIMER_STOP ('FLOATS            [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
        CALL AUTODIFF_INADMODE_SET( myThid )
#endif

#ifdef ALLOW_TIMEAVE
C--   State-variables time-averaging
      CALL TIMER_START('DO_STATEVARS_TAVE   [FORWARD_STEP]',myThid)
      CALL DO_STATEVARS_TAVE( myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_STATEVARS_TAVE   [FORWARD_STEP]',myThid)
#endif

#ifndef ALLOW_OFFLINE
#ifdef ALLOW_MONITOR
C--   Check status of solution (statistics, cfl, etc...)
      CALL TIMER_START('MONITOR             [FORWARD_STEP]',myThid)
      CALL MONITOR( myIter, myTime, myThid )
      CALL TIMER_STOP ('MONITOR             [FORWARD_STEP]',myThid)
#endif /* ALLOW_MONITOR */
#endif

C--   Do IO if needed.
#ifdef ALLOW_OFFLINE
      CALL TIMER_START('OFFLINE_MODEL_IO     [FORWARD_STEP]',myThid)
      CALL OFFLINE_MODEL_IO( myTime, myIter, myThid )
      CALL TIMER_STOP ('OFFLINE_MODEL_IO     [FORWARD_STEP]',myThid)
#else
      CALL TIMER_START('DO_THE_MODEL_IO     [FORWARD_STEP]',myThid)
      CALL DO_THE_MODEL_IO( myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_THE_MODEL_IO     [FORWARD_STEP]',myThid)
#endif

#ifndef ALLOW_DIVIDED_ADJOINT
# ifdef HAVE_SIGREG
      IF ( useSIGREG ) THEN
        IF ( i_got_signal .GT. 0 ) THEN
          CALL PACKAGES_WRITE_PICKUP(
     I         .TRUE., myTime, myIter, myThid )
          CALL WRITE_PICKUP(
     I         .TRUE., myTime, myIter, myThid )
          STOP 'Checkpoint completed -- killed by signal handler'
        ENDIF
      ENDIF
# endif /* HAVE_SIGREG */
C--   Save state for restarts
      CALL TIMER_START('DO_WRITE_PICKUP     [FORWARD_STEP]',myThid)
      CALL DO_WRITE_PICKUP(
     I               .FALSE., myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_WRITE_PICKUP     [FORWARD_STEP]',myThid)
#endif /* ALLOW_DIVIDED_ADJOINT */

#ifdef TIME_PER_TIMESTEP
CCE107 Time per timestep information
      _BEGIN_MASTER( myThid )
      CALL TIMER_GET_TIME( utnew, stnew, wtnew )
      WRITE(msgBuf,'(A34,3F10.6,I8)')
     $        'User, system and wallclock time:', utnew - utold,
     $        stnew - stold, wtnew - wtold, iloop
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      utold = utnew
      stold = stnew
      wtold = wtnew
      _END_MASTER( myThid )
#endif
#ifdef USE_PAPI_FLOPS
CCE107 PAPI summary performance
      _BEGIN_MASTER( myThid )
#ifdef USE_FLIPS
      call PAPIF_flips(real_time, proc_time, flpops, mflops, check)
#else
      call PAPIF_flops(real_time, proc_time, flpops, mflops, check)
#endif
      WRITE(msgBuf,'(F10.6,A,F10.6,A34,I8)')
     $     mflops, ' ', mflops*proc_time/(real_time + 1E-36),
     $     'Mflop/s during timestep ', iloop 
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#ifdef PAPI_VERSION
      call PAPIF_ipc(real_time, proc_time, instr, ipc, check)
      WRITE(msgBuf,'(F10.6,A,F10.6,A34,I8)')
     $     ipc, ' ', ipc*proc_time/(real_time + 1E-36),
     $     'IPC during timestep ', iloop 
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#endif
      _END_MASTER( myThid )
#else
#ifdef USE_PCL_FLOPS
CCE107 PCL summary performance
      _BEGIN_MASTER( myThid )
      res = PCLstop(descr, i_result, fp_result, nevents)
      do ipcl = 1, nevents
         WRITE(msgBuf,'(F10.6,A2,A22,A17,I8)'), fp_result(ipcl),
     $        ' ', pcl_counter_name(pcl_counter_list(ipcl)),
     $        'during timestep ', iloop 
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      enddo
       res = PCLstart(descr, pcl_counter_list, nevents, flags)
      _END_MASTER( myThid )
#endif
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_TAMC_CHECKPOINTING
             endif
            enddo
           endif
          enddo
#ifndef AUTODIFF_2_LEVEL_CHECKPOINT
         endif
        enddo
#endif
#ifdef AUTODIFF_4_LEVEL_CHECKPOINT
       endif
      enddo
#endif
c
#else /* ndef ALLOW_TAMC_CHECKPOINTING */
      enddo
#endif /* ALLOW_TAMC_CHECKPOINTING */

#else /* ndef ALLOW_AUTODIFF_TAMC */
      enddo
#endif /* ALLOW_AUTODIFF_TAMC */

      _BARRIER
      call timer_stop ('ECCO MAIN LOOP', mythid)

      call timer_start('ECCO SPIN-DOWN', mythid)

#ifdef ALLOW_PROFILES
#ifndef ALLOW_DIVIDED_ADJOINT
c--     Accumulate in-situ time averages of temperature, salinity, and SSH.
      call timer_start('PROFILES_INLOOP     [ECCO SPIN-DOWN]', mythid)
      call profiles_inloop( mytime, mythid )
      call timer_stop ('PROFILES_INLOOP     [ECCO SPIN-DOWN]', mythid)
#endif
#endif

#ifdef ALLOW_COST

#ifdef ALLOW_DIVIDED_ADJOINT
CADJ STORE mytime = onetape
#endif
c--   Accumulate time averages of temperature, salinity, and SSH.
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_averagesfields',myThid)
#endif
      call timer_start('cost_averagesfields [ECCO SPIN-DOWN]', mythid)
      call cost_averagesfields( mytime, mythid )
      call timer_stop ('cost_averagesfields [ECCO SPIN-DOWN]', mythid)
#ifdef ALLOW_DIVIDED_ADJOINT
c**************************************
#include "cost_averages_bar_directives.h"
c**************************************
#endif

#ifdef ALLOW_COST_ATLANTIC
c--   Compute meridional heat transport 
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_atlantic',myThid)
#endif
      call timer_start('cost_atlantic      [ECCO SPIN-DOWN]', mythid)
      call cost_atlantic(  mytime, myiter,mythid )
      call timer_stop ('cost_atlantic      [ECCO SPIN-DOWN]', mythid)
#endif

c--   Compute the cost function contribution of the boundary forcing,
c--   i.e. heat flux, salt flux, zonal and meridional wind stress.
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_forcing',myThid)
#endif
      call timer_start('cost_forcing       [ECCO SPIN-DOWN]', mythid)
      call cost_forcing( myiter, mytime, mythid )
      call timer_stop ('cost_forcing       [ECCO SPIN-DOWN]', mythid)
cph(
c--   Compute cost function contribution of wind stress observations.  
#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
      call cost_mean_heatflux( myiter, mytime, mythid )
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE objf_hfluxmm = tapelev_init, key = 1
# endif
#endif

c--   Compute cost function contribution of wind stress observations.
#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
      call cost_mean_saltflux( myiter, mytime, mythid )
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE objf_sfluxmm = tapelev_init, key = 1
# endif
#endif
cph)

c--   Compute cost function contribution of SSH.
#ifdef ALLOW_SSH_COST_CONTRIBUTION
# ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('cost_ssh',myThid)
# endif
      call timer_start('cost_ssh           [ECCO SPIN-DOWN]', mythid)
      call cost_ssh( myiter, mytime, mythid )
      call timer_stop ('cost_ssh           [ECCO SPIN-DOWN]', mythid)
# ifdef ALLOW_AUTODIFF_TAMC
#  ifdef ALLOW_PROFILES
CADJ STORE prof_etan_mean = tapelev_init, key = 1
#  endif
# endif
#endif

c--   Compute cost function contribution of Temperature and Salinity.
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_hyd',myThid)
#endif
      call timer_start('cost_hyd           [ECCO SPIN-DOWN]', mythid)
      call cost_hyd( myiter, mytime, mythid )
      call timer_stop ('cost_hyd           [ECCO SPIN-DOWN]', mythid)

#ifdef ALLOW_SEAICE
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('seaice_cost_driver',myThid)
#endif
      call timer_start('seaice_cost_driver [ECCO SPIN-DOWN]', mythid)
      call seaice_cost_driver( myiter, mytime, mythid )
      call timer_stop ('seaice_cost_driver [ECCO SPIN-DOWN]', mythid)
#endif

#ifdef ALLOW_OBCS_COST_CONTRIBUTION
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_obcs',myThid)
#endif
      call timer_start('cost_obcs          [ECCO SPIN-DOWN]', mythid)
      call cost_obcs( myiter, mytime, mythid )
      call timer_stop ('cost_obcs          [ECCO SPIN-DOWN]', mythid)
#endif

#ifdef ALLOW_CURMTR_COST_CONTRIBUTION
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_curmtr',myThid)
#endif
      call timer_start('cost_curmtr        [ECCO SPIN-DOWN]', mythid)
      call cost_curmtr( myiter, mytime, mythid )
      call timer_stop ('cost_curmtr        [ECCO SPIN-DOWN]', mythid)
#endif

c--   Compute cost function contribution of drifter's velocities.
#ifdef ALLOW_DRIFTER_COST_CONTRIBUTION
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_drifter',myThid)
#endif
      call timer_start('cost_drifter         [ECCO SPIN-DOWN]', mythid)
      call cost_drifter( myiter, mytime, mythid )
      call timer_stop ('cost_drifter         [ECCO SPIN-DOWN]', mythid)
#endif

c--   Compute cost function contribution of wind stress observations.
#ifdef ALLOW_SCAT_COST_CONTRIBUTION
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_scat',myThid)
#endif
      call timer_start('cost_scat            [ECCO SPIN-DOWN]', mythid) 
      call cost_scat( myiter, mytime, mythid )
      call timer_stop ('cost_scat            [ECCO SPIN-DOWN]', mythid)
#endif

c--   Compute cost function contribution of drift between the first 
c     and the last year.
#ifdef ALLOW_DRIFT_COST_CONTRIBUTION
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_drift',myThid)
#endif
      call timer_start('cost_drift         [ECCO SPIN-DOWN]', mythid)
      call cost_drift( myiter, mytime, mythid )
      call timer_stop ('cost_drift         [ECCO SPIN-DOWN]', mythid)
#endif
#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_driftw',myThid)
#endif
      call timer_start('cost_driftw        [ECCO SPIN-DOWN]', mythid)
      call cost_driftw( myiter, mytime, mythid )
      call timer_stop ('cost_driftw        [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

c--   Compute initial vs. final T/S deviation
#ifdef ALLOW_COST_INI_FIN
      call timer_start('cost_ini_fin       [ECCO SPIN-DOWN]', mythid)
      call cost_theta_ini_fin( myiter, mytime, mythid )
      call cost_salt_ini_fin( myiter, mytime, mythid )
      call timer_stop ('cost_ini_fin       [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

c--   Eddy stress penalty term
#ifdef ALLOW_COST_TAU_EDDY
      call timer_start('cost_tau_eddy      [ECCO SPIN-DOWN]', mythid)
      call cost_tau_eddy( mythid )
      call timer_stop ('cost_tau_eddy      [ECCO SPIN-DOWN]', mythid)
#endif

c--   Sum all cost function contributions.
#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('cost_final',myThid)
#endif
      call timer_start('COST_FINAL         [ECCO SPIN-DOWN]', mythid)
      call cost_final( mythid )
      call timer_stop ('COST_FINAL         [ECCO SPIN-DOWN]', mythid)

#endif /* ALLOW_COST */

      call timer_stop ('ECCO SPIN-DOWN', mythid)

#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_LEAVE('THE_MAIN_LOOP',myThid)
#endif

      return
      end

