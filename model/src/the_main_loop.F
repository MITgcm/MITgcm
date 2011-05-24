C $Header: /u/gcmpack/MITgcm/model/src/the_main_loop.F,v 1.118 2011/05/24 20:25:33 jmc Exp $
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

CBOP
C     !ROUTINE: THE_MAIN_LOOP
C     !INTERFACE:
      SUBROUTINE THE_MAIN_LOOP( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE the_main_loop
C     | o Run the ocean model and evaluate the specified cost function.
C     *================================================================*
C     |
C     | THE_MAIN_LOOP is the toplevel routine for the Tangent Linear and
C     | Adjoint Model Compiler (TAMC). 
C     | For this purpose the initialization
C     | of the model was split into two parts. Those parameters that do
C     | not depend on a specific model run are set in INITIALISE_FIXED,
C     | whereas those that do depend on the specific realization are
C     | initialized in INITIALISE_VARIA. 
C     | This routine is to be used in conjuction with the MITgcmuv 
C     | checkpoint 37.
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

c**************************************
#ifdef ALLOW_AUTODIFF
# ifndef ALLOW_AUTODIFF_OPENAD

c These includes are needed for 
c AD-checkpointing. 
c They provide the fields to be stored.

#  include "AUTODIFF_MYFIELDS.h"
#  include "GRID.h"
#  include "DYNVARS.h"
#  include "SURFACE.h"
#  include "FFIELDS.h"
#  include "EOS.h"
#  include "AUTODIFF.h"

#  ifdef ALLOW_GENERIC_ADVDIFF
#   include "GAD.h"
#  endif
#  ifdef ALLOW_MOM_FLUXFORM
#   include "MOM_FLUXFORM.h"
#  endif
#  ifdef ALLOW_CD_CODE
#   include "CD_CODE_VARS.h"
#  endif
#  ifdef ALLOW_PTRACERS
#   include "PTRACERS_SIZE.h"
#   include "PTRACERS_FIELDS.h"
#  endif
# ifdef ALLOW_GCHEM
#  include "GCHEM_FIELDS.h"
# endif
# ifdef ALLOW_CFC
#  include "CFC.h"
# endif
# ifdef ALLOW_DIC
#  include "DIC_VARS.h"
#  include "DIC_LOAD.h"
#  include "DIC_ATMOS.h"
#  include "DIC_CTRL.h"
#  include "DIC_COST.h"
# endif
#  ifdef ALLOW_OBCS
#   include "OBCS_FIELDS.h"
#   include "OBCS_SEAICE.h"
#   ifdef ALLOW_PTRACERS
#    include "OBCS_PTRACERS.h"
#   endif
#  endif
#  ifdef ALLOW_EXF
#   include "EXF_FIELDS.h"
#   ifdef ALLOW_BULKFORMULAE
#    include "EXF_CONSTANTS.h"
#   endif
#  endif /* ALLOW_EXF */
#  ifdef ALLOW_SEAICE
#   include "SEAICE_SIZE.h"
#   include "SEAICE.h"
#   include "SEAICE_PARAMS.h"
#   include "SEAICE_COST.h"
#  endif
#  ifdef ALLOW_SALT_PLUME
#   include "SALT_PLUME.h"
#  endif
#  ifdef ALLOW_THSICE
#   include "THSICE_SIZE.h"
#   include "THSICE_VARS.h"
#  endif
#  ifdef ALLOW_SHELFICE
#   include "SHELFICE.h"
#   include "SHELFICE_COST.h"
#  endif
#  ifdef ALLOW_EBM
#   include "EBM.h"
#  endif
#  ifdef ALLOW_RBCS
#   include "RBCS_SIZE.h"
#   include "RBCS_FIELDS.h"
#  endif
#  ifdef ALLOW_OFFLINE
#   include "OFFLINE.h"
#  endif
#  ifdef ALLOW_DIVIDED_ADJOINT_MPI
#   include "mpif.h"
#  endif

#  include "tamc.h"
# endif /* undef ALLOW_AUTODIFF_OPENAD */

# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"
# ifdef ALLOW_ECCO
#  include "ecco_cost.h"
# endif

#endif /* ALLOW_AUTODIFF */
c**************************************

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     note: under the multi-threaded model myiter and 
C           mytime are local variables passed around as routine 
C           arguments. Although this is fiddly it saves the need to 
C           impose additional synchronisation points when they are 
C           updated.
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid 

C     !FUNCTIONS:
C     == Functions ==

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer iloop
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_AUTODIFF_OPENAD
      integer  uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4
      integer ilev_4
      integer theCurrentStep
# endif
#ifdef STORE_LOADEDREC_TEST
      integer bi,bj
#endif /* STORE_LOADEDREC_TEST */
#endif

CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_MAIN_LOOP',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
c--   Initialize storage for the cost function evaluation.
CADJ  INIT dummytape = common, 1
c--   Initialize storage for the outermost loop.
CADJ  INIT tapelev_init   = USER
#ifndef ALLOW_AUTODIFF_WHTAPEIO
CADJ  INIT tapelev_ini_bibj_k   = USER
#endif
c
# if (defined (AUTODIFF_2_LEVEL_CHECKPOINT))
CADJ  INIT tapelev2 = USER
# elif (defined (AUTODIFF_4_LEVEL_CHECKPOINT))
CADJ  INIT tapelev4 = USER
# else
CADJ  INIT tapelev3 = USER
# endif
#endif

#ifdef ALLOW_AUTODIFF
      nIter0 = NINT( (startTime-baseTime)/deltaTClock )
      ikey_dynamics = 1
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
CADJ STORE hFacC = tapelev_init, key = 1
# endif
#endif

#ifdef ALLOW_AUTODIFF_OPENAD
# ifdef ALLOW_THETA0_CONTROL
c$openad INDEPENDENT(xx_theta)
# endif
# ifdef ALLOW_SALT0_CONTROL
c$openad INDEPENDENT(xx_salt)
# endif
# ifdef ALLOW_HFLUX0_CONTROL
c$openad INDEPENDENT(xx_hflux0)    
# endif
# ifdef ALLOW_SFLUX0_CONTROL
c$openad INDEPENDENT(xx_sflux0) 
# endif
# ifdef ALLOW_TAUU0_CONTROL 
c$openad INDEPENDENT(xx_tauu0)    
# endif
# ifdef ALLOW_TAUV0_CONTROL 
c$openad INDEPENDENT(xx_tauv0) 
# endif
# ifdef ALLOW_DIFFKR_CONTROL
c$openad INDEPENDENT(xx_diffkr) 
# endif
# ifdef ALLOW_ KAPGM_CONTROL
c$openad INDEPENDENT(xx_kapgm) 
# endif
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INITIALISE_VARIA',myThid)
#endif
C--   Set initial conditions (variable arrays)
      CALL TIMER_START('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)
      CALL INITIALISE_VARIA( mythid )
      CALL TIMER_STOP ('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)

#ifdef ALLOW_SHOWFLOPS
      CALL TIMER_START('SHOWFLOPS_INIT      [THE_MAIN_LOOP]', mythid)
      CALL SHOWFLOPS_INIT( myThid )
      CALL TIMER_STOP('SHOWFLOPS_INIT       [THE_MAIN_LOOP]', mythid)
#endif

c--   Do the model integration.
      CALL TIMER_START('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

c     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STARTS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#ifndef ALLOW_AUTODIFF_OPENAD
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ifdef ALLOW_AUTODIFF
#  ifdef ALLOW_TAMC_CHECKPOINTING

      max_lev4=nTimeSteps/(nchklev_1*nchklev_2*nchklev_3)+1
      max_lev3=nTimeSteps/(nchklev_1*nchklev_2)+1
      max_lev2=nTimeSteps/nchklev_1+1

c**************************************
#   ifdef ALLOW_DIVIDED_ADJOINT
CADJ loop = divided
#   endif
c**************************************

#   ifdef AUTODIFF_4_LEVEL_CHECKPOINT
      do ilev_4 = 1,nchklev_4
         if(ilev_4.le.max_lev4) then
c**************************************
#ifdef ALLOW_AUTODIFF_WHTAPEIO
            CALL AUTODIFF_WHTAPEIO_SYNC( 4 , 0, mythid )
#endif
            CALL AUTODIFF_STORE( myThid )
#include "checkpoint_lev4_directives.h"
            CALL AUTODIFF_RESTORE( myThid )
#ifdef ALLOW_AUTODIFF_WHTAPEIO     
            CALL AUTODIFF_WHTAPEIO_SYNC( 4 , 1, mythid )
#endif      
c**************************************
c--     Initialise storage for the middle loop.
CADJ    INIT tapelev3 = USER
#   endif /* AUTODIFF_4_LEVEL_CHECKPOINT */

#   ifndef AUTODIFF_2_LEVEL_CHECKPOINT
      do ilev_3 = 1,nchklev_3
         if(ilev_3.le.max_lev3) then
c**************************************
#ifdef ALLOW_AUTODIFF_WHTAPEIO     
            CALL AUTODIFF_WHTAPEIO_SYNC( 3 , 0, mythid )
#endif
            CALL AUTODIFF_STORE( myThid )
#include "checkpoint_lev3_directives.h"
            CALL AUTODIFF_RESTORE( myThid )
#ifdef ALLOW_AUTODIFF_WHTAPEIO     
            CALL AUTODIFF_WHTAPEIO_SYNC( 3 , 1, mythid )
#endif
c**************************************
c--     Initialise storage for the middle loop.
CADJ    INIT tapelev2 = USER
#   endif /* AUTODIFF_2_LEVEL_CHECKPOINT */

        do ilev_2 = 1,nchklev_2
         if(ilev_2.le.max_lev2) then
c**************************************
#ifdef ALLOW_AUTODIFF_WHTAPEIO     
            CALL AUTODIFF_WHTAPEIO_SYNC( 2 , 0, mythid )
#endif
            CALL AUTODIFF_STORE( myThid )
#include "checkpoint_lev2_directives.h"
            CALL AUTODIFF_RESTORE( myThid )
#ifdef ALLOW_AUTODIFF_WHTAPEIO     
            CALL AUTODIFF_WHTAPEIO_SYNC( 2 , 1, mythid )
#endif
c**************************************

c**************************************
c--
c--       Initialize storage for the innermost loop.
c--       Always check common block sizes for the checkpointing!
c--
CADJ INIT comlev1        = COMMON,nchklev_1
CADJ INIT comlev1_bibj   = COMMON,nchklev_1*nsx*nsy*nthreads_chkpt
CADJ INIT comlev1_bibj_k = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt
c--
#   ifdef ALLOW_KPP
CADJ INIT comlev1_kpp    = COMMON,nchklev_1*nsx*nsy
CADJ INIT comlev1_kpp_k  = COMMON,nchklev_1*nsx*nsy*nr
#   endif /* ALLOW_KPP */
c--
#   ifdef ALLOW_GMREDI
CADJ INIT comlev1_gmredi_k_gad
CADJ &    = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass
#   endif /* ALLOW_GMREDI */
c--
#   ifdef ALLOW_PTRACERS
CADJ INIT comlev1_bibj_ptracers = COMMON,
CADJ &    nchklev_1*nsx*nsy*nthreads_chkpt*PTRACERS_num
CADJ INIT comlev1_bibj_k_ptracers = COMMON,
CADJ &    nchklev_1*nsx*nsy*nthreads_chkpt*PTRACERS_num*nr
#   endif /* ALLOW_PTRACERS */
c--
#   ifndef DISABLE_MULTIDIM_ADVECTION
CADJ INIT comlev1_bibj_k_gad = COMMON,
CADJ &    nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass
CADJ INIT comlev1_bibj_k_gad_pass = COMMON,
CADJ &    nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass*maxpass
#   endif /* DISABLE_MULTIDIM_ADVECTION */
c--
#   ifdef ALLOW_MOM_COMMON
#   ifndef AUTODIFF_DISABLE_LEITH
CADJ INIT comlev1_mom_ijk_loop
CADJ &     = COMMON,nchklev_1*
CADJ &       (snx+2*olx)*nsx*(sny+2*oly)*nsy*nr*nthreads_chkpt
#   endif /* AUTODIFF_DISABLE_LEITH */
#   endif /* ALLOW_MOM_COMMON */
c--
#   if (defined (ALLOW_EXF) && defined (ALLOW_BULKFORMULAE))
CADJ INIT comlev1_exf_1
CADJ &     = COMMON,nchklev_1*snx*nsx*sny*nsy*nthreads_chkpt
CADJ INIT comlev1_exf_2
CADJ &     = COMMON,niter_bulk*nchklev_1*snx*nsx*sny*nsy*nthreads_chkpt
#   endif /* ALLOW_BULKFORMULAE */
c--
#   ifdef ALLOW_SEAICE
#    ifdef SEAICE_ALLOW_DYNAMICS
cphCADJ INIT comlev1_lsr = COMMON,nchklev_1*2
CADJ INIT comlev1_dynsol = COMMON,nchklev_1*MPSEUDOTIMESTEPS
#   endif
#   ifdef SEAICE_ALLOW_EVP
CADJ INIT comlev1_evp = COMMON,nEVPstepMax*nchklev_1
#   endif
#   ifdef SEAICE_MULTICATEGORY
CADJ INIT comlev1_multdim 
CADJ &    = COMMON,nchklev_1*nsx*nsy*nthreads_chkpt*multdim
#   endif
#    ifndef DISABLE_MULTIDIM_ADVECTION
CADJ INIT comlev1_bibj_k_gadice = COMMON,
CADJ &    nchklev_1*nsx*nsy*nthreads_chkpt*maxpass
CADJ INIT comlev1_bibj_k_gadice_pass = COMMON,
CADJ &    nchklev_1*nsx*nsy*nthreads_chkpt*maxpass*maxpass
#    endif /* DISABLE_MULTIDIM_ADVECTION */
# endif /* ALLOW_SEAICE */
c--
#   ifdef ALLOW_THSICE
CADJ INIT comlev1_thsice_1
CADJ &     = COMMON,nchklev_1*snx*nsx*sny*nsy*nthreads_chkpt
CADJ INIT comlev1_thsice_2
CADJ &     = COMMON,nchklev_1*snx*nsx*sny*nsy*nlyr*nthreads_chkpt
CADJ INIT comlev1_thsice_3
CADJ &     = COMMON,nchklev_1*snx*nsx*sny*nsy*MaxTsf*nthreads_chkpt
CADJ INIT comlev1_thsice_4
CADJ &     = COMMON,nchklev_1*nsx*nsy*maxpass*nthreads_chkpt
#   endif /* ALLOW_THSICE */
c--
#   ifdef ALLOW_DEPTH_CONTROL
CADJ INIT comlev1_cg2d
CADJ &     = COMMON,nchklev_1*nthreads_chkpt
CADJ INIT comlev1_cg2d_iter
CADJ &     = COMMON,nchklev_1*nthreads_chkpt*numItersMax
#   endif
c--
c**************************************

#ifdef STORE_LOADEDREC_TEST
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            loadedRec(bi,bj) = 0
           ENDDO
          ENDDO
#endif /* STORE_LOADEDREC_TEST */

          do ilev_1 = 1,nchklev_1

c--         The if-statement below introduces a some flexibility in the
c--         choice of the 3-tupel ( nchklev_1, nchklev_2, nchklev_3 ).

            iloop = (ilev_2 - 1)*nchklev_1                     + ilev_1
#    ifndef AUTODIFF_2_LEVEL_CHECKPOINT
     &            + (ilev_3 - 1)*nchklev_2*nchklev_1
#    endif
#    ifdef AUTODIFF_4_LEVEL_CHECKPOINT
     &            + (ilev_4 - 1)*nchklev_3*nchklev_2*nchklev_1
#    endif

            if ( iloop .le. nTimeSteps ) then

#  else /* ALLOW_TAMC_CHECKPOINTING  undefined */
c--   Initialise storage for reference trajectory without TAMC check-
c--   pointing.
CADJ INIT history        = USER
CADJ INIT comlev1_bibj   = COMMON,nchklev_0*nsx*nsy*nthreads_chkpt
CADJ INIT comlev1_bibj_k = COMMON,nchklev_0*nsx*nsy*nr*nthreads_chkpt
CADJ INIT comlev1_kpp    = COMMON,nchklev_0*nsx*nsy

c--   Check the choice of the checkpointing parameters in relation
c--   to nTimeSteps: (nchklev_0 .ge. nTimeSteps)
      if (nchklev_0 .lt. nTimeSteps) then
        print*
        print*, ' the_main_loop: TAMC checkpointing parameter ',
     &       'nchklev_0 = ',       nchklev_0
        print*, '                 not consistent with nTimeSteps = ', 
     &       nTimeSteps
        stop    ' ... stopped in the_main_loop.'
      endif

      DO iloop = 1, nTimeSteps

#  endif /* ALLOW_TAMC_CHECKPOINTING */
# endif /* ALLOW_AUTODIFF */

#endif /* undef ALLOW_AUTODIFF_OPENAD */

#ifdef ALLOW_AUTODIFF_OPENAD
      call openad_checkpointinit(uCheckLev1, 
     +        uCheckLev2, 
     +        uCheckLev3,
     +        uCheckLev4 )
      
      theCurrentStep=0

      if (uCheckLev4 .gt. 0 ) then 
       do ilev_4 = 1, uCheckLev4

#endif

#ifndef ALLOW_AUTODIFF 

c--   Start the main loop of adjoint_Objfunc. Automatic differentiation
c--   NOT enabled.
      DO iloop = 1, nTimeSteps

#endif /* ALLOW_AUTODIFF */

c--     >>> Loop body start <<<

#ifdef ALLOW_AUTODIFF_TAMC
        nIter0 = NINT( (startTime-baseTime)/deltaTClock )
        ikey_dynamics = ilev_1
#endif

#ifdef ALLOW_ECCO_EVOLUTION
#ifdef ALLOW_ECCO
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_averagesfields',myThid)
#endif
c--   Accumulate time averages of temperature, salinity
#ifdef ALLOW_AUTODIFF
C--   Reset the model iteration counter and the model time.
      myIter = nIter0 + (iloop-1)
      myTime = startTime + float(iloop-1)*deltaTclock
#endif
      CALL TIMER_START('COST_AVERAGESFIELDS [THE_MAIN_LOOP]',mythid)
      CALL COST_AVERAGESFIELDS( mytime, mythid )
      CALL TIMER_STOP ('COST_AVERAGESFIELDS [THE_MAIN_LOOP]',mythid)
#endif /* ALLOW_ECCO */
#endif /* ALLOW_ECCO_EVOLUTION */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('FORWARD_STEP',myThid)
#endif

#ifdef ALLOW_ATM2D
        CALL TIMER_START('FORWARD_STEP_ATM2D  [THE_MAIN_LOOP]',mythid)
        CALL FORWARD_STEP_ATM2D( iloop, mytime, myiter, mythid )
        CALL TIMER_STOP ('FORWARD_STEP_ATM2D  [THE_MAIN_LOOP]',mythid)
#else
        CALL TIMER_START('FORWARD_STEP        [THE_MAIN_LOOP]',mythid)
# ifdef ALLOW_AUTODIFF_OPENAD
            CALL THE_FOURTH_LEVEL_LOOP( ilev_4, 
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +theCurrentStep,
     +myTime, myIter, myThid )
# else 
        CALL FORWARD_STEP( iloop, mytime, myiter, mythid )
# endif
        CALL TIMER_STOP ('FORWARD_STEP        [THE_MAIN_LOOP]',mythid)
#endif

c--     >>> Loop body end <<<
#ifdef ALLOW_AUTODIFF
# ifndef ALLOW_AUTODIFF_OPENAD

#   ifdef ALLOW_TAMC_CHECKPOINTING 
            endif
          enddo
          endif
        enddo
#    ifndef AUTODIFF_2_LEVEL_CHECKPOINT
        endif
      enddo
#    endif
#    ifdef AUTODIFF_4_LEVEL_CHECKPOINT
       endif
      enddo
#    endif
#   else /* ndef ALLOW_TAMC_CHECKPOINTING */
      enddo
#   endif /* ALLOW_TAMC_CHECKPOINTING */
# else /* ALLOW_AUTODIFF_OPENAD */
         end do
      else
         CALL THE_FOURTH_LEVEL_PLAIN( 
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +theCurrentStep,
     +myTime, myIter, myThid )
      end if
# endif /* ALLOW_AUTODIFF_OPENAD */
#else /* ALLOW_AUTODIFF */
      enddo
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_ECCO
c--     Accumulate time averages of temperature, salinity
      CALL TIMER_START('COST_AVERAGESFIELDS [THE_MAIN_LOOP]',mythid)
      CALL COST_AVERAGESFIELDS( mytime, mythid )
      CALL TIMER_STOP ('COST_AVERAGESFIELDS [THE_MAIN_LOOP]',mythid)
#endif /* ALLOW_ECCO */

#ifdef ALLOW_PROFILES
c--   Accumulate in-situ time averages of temperature, salinity, SSH.
      CALL TIMER_START('PROFILES_INLOOP    [THE_MAIN_LOOP]', mythid)
      CALL PROFILES_INLOOP( mytime, mythid )
      CALL TIMER_STOP ('PROFILES_INLOOP    [THE_MAIN_LOOP]', mythid)
#endif

#ifdef ALLOW_ECCO
c--   Compute all ecco cost function contributions.
      CALL TIMER_START('ECCO_COST_DRIVER   [THE_MAIN_LOOP]', mythid)
      CALL ECCO_COST_DRIVER ( myTime, myIter, mythid )
      CALL TIMER_STOP ('ECCO_COST_DRIVER   [THE_MAIN_LOOP]', mythid)
#endif /* ALLOW_ECCO */

#ifdef ALLOW_COST
c--   Sum all cost function contributions.
      CALL TIMER_START('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)
      CALL COST_FINAL ( mythid )
      CALL TIMER_STOP ('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)

# ifdef ALLOW_AUTODIFF_OPENAD
c$openad DEPENDENT(fc)
# endif /* ALLOW_AUTODIFF_OPENAD */

#endif /* ALLOW_COST */

      _BARRIER
      CALL TIMER_STOP ('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_MAIN_LOOP',myThid)
#endif

      RETURN
      END
