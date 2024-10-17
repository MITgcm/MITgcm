#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD_OPTIONS.h"
#endif
#ifdef ALLOW_GGL90
# include "GGL90_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_GCHEM
# include "GCHEM_OPTIONS.h"
#endif
#ifdef ALLOW_DIC
# include "DIC_OPTIONS.h"
#endif
#ifdef ALLOW_BLING
# include "BLING_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_PROFILES
# include "PROFILES_OPTIONS.h"
#endif
#ifdef ALLOW_OBSFIT
# include "OBSFIT_OPTIONS.h"
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
# ifndef ALLOW_OPENAD

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
#   include "GAD_SOM_VARS.h"
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
#   include "PTRACERS_START.h"
#  endif
#  ifdef ALLOW_GCHEM
#   include "GCHEM_SIZE.h"
#   include "GCHEM_FIELDS.h"
#  endif
#  ifdef ALLOW_CFC
#   include "CFC.h"
#  endif
#  ifdef ALLOW_DIC
#   include "DIC_VARS.h"
#   include "DIC_LOAD.h"
#   include "DIC_ATMOS.h"
#   include "DIC_CTRL.h"
#   include "DIC_COST.h"
#  endif
#  ifdef ALLOW_OBCS
#   include "OBCS_PARAMS.h"
#   include "OBCS_FIELDS.h"
#   include "OBCS_SEAICE.h"
#   ifdef ALLOW_PTRACERS
#    include "OBCS_PTRACERS.h"
#   endif
#  endif
#  ifdef ALLOW_EXF
#   ifdef ALLOW_BULKFORMULAE
#    include "EXF_CONSTANTS.h"
#   endif
#   include "EXF_INTERP_SIZE.h"
#   include "EXF_FIELDS.h"
#  endif /* ALLOW_EXF */
#  ifdef ALLOW_BLING
#   include "BLING_VARS.h"
#   include "BLING_LOAD.h"
#  endif
#  ifdef ALLOW_SEAICE
#   include "SEAICE_SIZE.h"
#   include "SEAICE.h"
#   include "SEAICE_PARAMS.h"
#   include "SEAICE_COST.h"
#   include "SEAICE_TRACER.h"
#  endif
#  ifdef ALLOW_SALT_PLUME
#   include "SALT_PLUME.h"
#  endif
#  ifdef ALLOW_THSICE
#   include "THSICE_SIZE.h"
#   include "THSICE_VARS.h"
#   include "THSICE_COST.h"
#  endif
#  ifdef ALLOW_SHELFICE
#   include "SHELFICE.h"
#   include "SHELFICE_COST.h"
#  endif
#  ifdef ALLOW_STREAMICE
#   include "STREAMICE.h"
#   include "STREAMICE_ADV.h"
#   include "STREAMICE_BDRY.h"
#   include "STREAMICE_CG.h"
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
# if (defined ALLOW_CG2D_NSA || defined NONLIN_FRSURF || \
      defined ALLOW_DEPTH_CONTROL)
#   include "CG2D.h"
#  endif
#  ifdef ALLOW_DIVIDED_ADJOINT
#   ifdef ALLOW_USE_MPI
#    include "mpif.h"
#   endif
#  endif

C-- keep tamc.h here without ALLOW_AUTODIFF_TAMC for ALLOW_TAMC_CHECKPOINTING
#  include "tamc.h"

#  ifdef ALLOW_GGL90
#   include "GGL90.h"
#  endif
#  ifdef ALLOW_PROFILES
#   include "PROFILES_SIZE.h"
#   include "profiles.h"
#  endif
#  ifdef ALLOW_OBSFIT
#   include "OBSFIT_SIZE.h"
#   include "OBSFIT.h"
#  endif
#  ifdef ALLOW_ECCO
#   include "ecco_cost.h"
#  endif
C     it is important that this file is included after SEAICE_SIZE.h
#  include "AUTODIFF_STORE.h"
# endif /* undef ALLOW_OPENAD */

# ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "CTRL_DUMMY.h"
# include "CTRL_GENARR.h"
# include "CTRL_OBCS.h"
# endif
# ifdef ALLOW_COST
#  include "cost.h"
# endif

#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_TAPENADE
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
#  include "GMREDI_TAVE.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP.h"
c#  include "KPP_PARAMS.h"
# endif
# ifdef ALLOW_TIMEAVE
#  include "TIMEAVE_STATV.h"
# endif
# ifdef ALLOW_EXF
#  include "EXF_PARAM.h"
#  include "EXF_INTERP_PARAM.h"
# endif
# ifdef ALLOW_DOWN_SLOPE
#  include "DWNSLP_SIZE.h"
#  include "DWNSLP_VARS.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_PARAMS.h"
# endif
#endif /* ALLOW_TAPENADE */

c**************************************

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     note: under the multi-threaded model myIter and
C           myTime are local variables passed around as routine
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
      INTEGER iloop
#ifdef STORE_LOADEDREC_TEST
      INTEGER bi,bj
#endif /* STORE_LOADEDREC_TEST */
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_TAMC_CHECKPOINTING
      INTEGER ilev_1
      INTEGER ilev_2, max_lev2
#  ifndef AUTODIFF_2_LEVEL_CHECKPOINT
      INTEGER ilev_3, max_lev3
#  endif
#  ifdef AUTODIFF_4_LEVEL_CHECKPOINT
      INTEGER ilev_4, max_lev4
#  endif
# endif /* ALLOW_TAMC_CHECKPOINTING */
#endif /* ALLOW_AUTODIFF */
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_MAIN_LOOP',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
c--   Initialize storage for the cost function evaluation.
c---------------------------------------------------------
c--   Initialize storage for the initialisation phase.
# if ( defined ALLOW_AUTODIFF_WHTAPEIO && !defined ALLOW_DIVIDED_ADJOINT )
CADJ INIT tapelev_init       = COMMON, 1
# else
CADJ INIT tapelev_init       = USER
# endif
c--   this tape is never used
cCADJ INIT tapelev_ini_bibj   = USER
# ifdef INCLUDE_CONVECT_INI_CALL
c--   currently only used in convective_adjustement_ini.F
#  if ( defined ALLOW_AUTODIFF_WHTAPEIO && !defined ALLOW_DIVIDED_ADJOINT )
CADJ INIT tapelev_ini_bibj_k = COMMON, nSx*nSy*Nr
#  else
CADJ INIT tapelev_ini_bibj_k = USER
#  endif
# endif
# ifdef ALLOW_GENTIM2D_CONTROL
c--   exclusive tape for variables with suffix "_dummy"
CADJ INIT dummytape          = COMMON, 1
# endif

c--   Initialize storage for the outermost loop.
# ifdef ALLOW_TAMC_CHECKPOINTING
#  if (defined (AUTODIFF_2_LEVEL_CHECKPOINT))
CADJ  INIT tapelev2 = USER
CADJ  INIT tapelvi2 = USER,'adi'
#  elif (defined (AUTODIFF_4_LEVEL_CHECKPOINT))
CADJ  INIT tapelev4 = USER
CADJ  INIT tapelvi4 = USER,'adi'
#  else
CADJ  INIT tapelev3 = USER
CADJ  INIT tapelvi3 = USER,'adi'
#  endif
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF
      nIter0 = NINT( (startTime-baseTime)/deltaTClock )
#endif

#ifdef ALLOW_AUTODIFF_TAMC
      ikey_dynamics = 1
# if ( defined ALLOW_AUTODIFF_WHTAPEIO && defined ALLOW_DIVIDED_ADJOINT )
C     this needs to happen before any store directive to taplev_init
      CALL AUTODIFF_WHTAPEIO_SYNC( 1 , 0, myThid )
# endif
# ifdef ALLOW_GENTIM2D_CONTROL
CADJ STORE xx_gentim2d_dummy = dummytape, key = 1 , kind = isbyte
# endif
# ifdef ALLOW_SHELFICE
CADJ STORE shelficeLoadAnomaly = tapelev_init, key=1, kind=isbyte
# endif
# ifdef ALLOW_STREAMICE
CADJ STORE H_streamice              = tapelev_init, key=1
CADJ STORE B_glen                   = tapelev_init, key=1
CADJ STORE bdot_streamice           = tapelev_init, key=1
CADJ STORE c_basal_friction         = tapelev_init, key=1
CADJ STORE streamice_bdot_maxmelt_v = tapelev_init, key=1
#  ifdef USE_ALT_RLOW
CADJ STORE r_low_si                 = tapelev_init, key=1
#  endif
# endif
# ifdef ALLOW_DIC
CADJ STORE co2atmos = tapelev_init, key = 1
CADJ STORE alpha    = tapelev_init, key = 1
# endif
#endif

#ifdef ALLOW_OPENAD
# ifdef ALLOW_GENARR2D_CONTROL
cphc$openad INDEPENDENT(xx_genarr2d)
c$openad INDEPENDENT(xx_genarr2d_dummy)
# endif
# ifdef ALLOW_GENARR3D_CONTROL
cphc$openad INDEPENDENT(xx_genarr3d)
c$openad INDEPENDENT(xx_genarr3d_dummy)
# endif
# ifdef ALLOW_GENTIM2D_CONTROL
cphc$openad INDEPENDENT(xx_gentim2d)
c$openad INDEPENDENT(xx_gentim2d_dummy)
# endif
#endif /* ALLOW_OPENAD */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INITIALISE_VARIA',myThid)
#endif
C--   Set initial conditions (variable arrays)
      CALL TIMER_START('INITIALISE_VARIA    [THE_MAIN_LOOP]', myThid)
      CALL INITIALISE_VARIA( myThid )
      CALL TIMER_STOP ('INITIALISE_VARIA    [THE_MAIN_LOOP]', myThid)

#ifdef ALLOW_SHOWFLOPS
      CALL TIMER_START('SHOWFLOPS_INIT      [THE_MAIN_LOOP]', myThid)
      CALL SHOWFLOPS_INIT( myThid )
      CALL TIMER_STOP ('SHOWFLOPS_INIT      [THE_MAIN_LOOP]', myThid)
#endif

c--   Do the model integration.
      CALL TIMER_START('MAIN LOOP           [THE_MAIN_LOOP]', myThid)

c     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STARTS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<

#ifdef USE_PDAF
c     Initialize PDAF
      CALL TIMER_START('INIT_PDAF           [THE_MAIN_LOOP]', myThid)
      CALL INIT_PDAF( nIter0, myTime, myIter, myThid )
      CALL TIMER_STOP ('INIT_PDAF           [THE_MAIN_LOOP]', myThid)
#endif

#if ( defined ALLOW_AUTODIFF_WHTAPEIO && defined ALLOW_DIVIDED_ADJOINT )
C     end of initialisation and use of tapelev_init: write buffer to disk
      CALL AUTODIFF_WHTAPEIO_SYNC( 1 , 1, myThid )
#endif

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#ifndef ALLOW_OPENAD
# ifdef ALLOW_AUTODIFF
#  ifdef ALLOW_TAMC_CHECKPOINTING
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#   ifdef AUTODIFF_4_LEVEL_CHECKPOINT
      max_lev4=nTimeSteps/(nchklev_1*nchklev_2*nchklev_3)+1
#   endif
#   ifndef AUTODIFF_2_LEVEL_CHECKPOINT
      max_lev3=nTimeSteps/(nchklev_1*nchklev_2)+1
#   endif
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
#    ifdef ALLOW_AUTODIFF_WHTAPEIO
            CALL AUTODIFF_WHTAPEIO_SYNC( 4 , 0, myThid )
#    endif
            CALL AUTODIFF_STORE( myThid )
#include "checkpoint_lev4_directives.h"
            CALL AUTODIFF_RESTORE( myThid )
#    ifdef ALLOW_AUTODIFF_WHTAPEIO
            CALL AUTODIFF_WHTAPEIO_SYNC( 4 , 1, myThid )
#    endif
c**************************************
c--     Initialise storage for the middle loop.
CADJ    INIT tapelev3 = USER
CADJ    INIT tapelvi3 = USER,'adi'
#   endif /* AUTODIFF_4_LEVEL_CHECKPOINT */

#   ifndef AUTODIFF_2_LEVEL_CHECKPOINT
      do ilev_3 = 1,nchklev_3
         if(ilev_3.le.max_lev3) then
c**************************************
#    ifdef ALLOW_AUTODIFF_WHTAPEIO
            CALL AUTODIFF_WHTAPEIO_SYNC( 3 , 0, myThid )
#    endif
            CALL AUTODIFF_STORE( myThid )
#include "checkpoint_lev3_directives.h"
            CALL AUTODIFF_RESTORE( myThid )
#    ifdef ALLOW_AUTODIFF_WHTAPEIO
            CALL AUTODIFF_WHTAPEIO_SYNC( 3 , 1, myThid )
#    endif
c**************************************
c--     Initialise storage for the middle loop.
CADJ    INIT tapelev2 = USER
CADJ    INIT tapelvi2 = USER,'adi'
#   endif /* AUTODIFF_2_LEVEL_CHECKPOINT */

        do ilev_2 = 1,nchklev_2
         if(ilev_2.le.max_lev2) then
c**************************************
#   ifdef ALLOW_AUTODIFF_WHTAPEIO
            CALL AUTODIFF_WHTAPEIO_SYNC( 2 , 0, myThid )
#   endif
            CALL AUTODIFF_STORE( myThid )
#include "checkpoint_lev2_directives.h"
            CALL AUTODIFF_RESTORE( myThid )
#   ifdef ALLOW_AUTODIFF_WHTAPEIO
            CALL AUTODIFF_WHTAPEIO_SYNC( 2 , 1, myThid )
#   endif
c**************************************

#  endif /* ALLOW_TAMC_CHECKPOINTING */

#  ifdef ALLOW_AUTODIFF_TAMC
c**************************************
c--       Initialize storage for the innermost loop.
c--       Always check common block sizes for the checkpointing!
c--
CADJ INIT comlev1        = COMMON,nchklev_1
CADJ INIT comlev1_bibj   = COMMON,nchklev_1*nSx*nSy
CADJ INIT comlev1_bibj_k = COMMON,nchklev_1*nSx*nSy*Nr
#   ifdef ALLOW_BLING
CADJ INIT comlev1_bibj_ijk = COMMON,
CADJ &    nchklev_1*nSx*nSy*Nr*(2*OLy+sNy)*(2*OLx+sNx)
#   endif
c--
#   ifdef ALLOW_KPP
CADJ INIT comlev1_kpp    = COMMON,nchklev_1*nSx*nSy
CADJ INIT comlev1_kpp_k  = COMMON,nchklev_1*nSx*nSy*Nr
#   endif /* ALLOW_KPP */
c--
#   ifdef ALLOW_GMREDI
CADJ INIT comlev1_gmredi_k_gad = COMMON,nchklev_1*nSx*nSy*Nr*maxpass
cCADJ INIT comlev1_gmredi_slope = COMMON,nchklev_1*nSx*nSy*Nr*3
#   endif /* ALLOW_GMREDI */
c--
#   ifdef ALLOW_PTRACERS
CADJ INIT comlev1_bibj_ptracers = COMMON,
CADJ &    nchklev_1*nSx*nSy*PTRACERS_num
CADJ INIT comlev1_bibj_k_ptracers = COMMON,
CADJ &    nchklev_1*nSx*nSy*PTRACERS_num*Nr
#   endif /* ALLOW_PTRACERS */
c--
#   ifdef ALLOW_MOM_COMMON
#   ifndef AUTODIFF_DISABLE_LEITH
CADJ INIT comlev1_mom_ijk_loop
CADJ &     = COMMON,nchklev_1*(sNx+2*OLx)*nSx*(sNy+2*OLy)*nSy*Nr
#   endif /* AUTODIFF_DISABLE_LEITH */
#   endif /* ALLOW_MOM_COMMON */
c--
#   if (defined ALLOW_EXF && defined ALLOW_BULKFORMULAE)
CADJ INIT comlev1_exf_1 = COMMON,nchklev_1*sNx*nSx*sNy*nSy
CADJ INIT comlev1_exf_2 = COMMON,niter_bulk*nchklev_1*sNx*nSx*sNy*nSy
#   endif /* ALLOW_BULKFORMULAE */
c--
#   ifdef ALLOW_SEAICE
#    ifdef SEAICE_CGRID
CADJ INIT comlev1_dynsol = COMMON,nchklev_1*MPSEUDOTIMESTEPS
#     ifdef SEAICE_LSR_ADJOINT_ITER
CADJ INIT comlev1_lsr = COMMON,nchklev_1*MPSEUDOTIMESTEPS*SOLV_MAX_FIXED
CADJ INIT comlev1_bibj_lsr = COMMON,
CADJ &  nchklev_1*nSx*nSy*MPSEUDOTIMESTEPS*SOLV_MAX_FIXED
#     else /* make the common blocks smaller to reduce memory footprint */
CADJ INIT comlev1_lsr      = COMMON,        nchklev_1*MPSEUDOTIMESTEPS
CADJ INIT comlev1_bibj_lsr = COMMON,nSx*nSy*nchklev_1*MPSEUDOTIMESTEPS
#     endif
#     ifdef SEAICE_ALLOW_EVP
CADJ INIT comlev1_evp      = COMMON,        nEVPstepMax*nchklev_1
CADJ INIT comlev1_bibj_evp = COMMON,nSx*nSy*nEVPstepMax*nchklev_1
#     endif
#    endif
CML#   ifdef SEAICE_MULTICATEGORY
CMLCADJ INIT comlev1_multdim = COMMON,nchklev_1*nSx*nSy*nitd
CML#   endif
#    ifndef DISABLE_MULTIDIM_ADVECTION
CADJ INIT comlev1_bibj_k_gadice = COMMON,
CADJ &    nchklev_1*nSx*nSy*maxpass
CADJ INIT comlev1_bibj_k_gadice_pass = COMMON,
CADJ &    nchklev_1*nSx*nSy*maxpass*maxcube
#    endif /* DISABLE_MULTIDIM_ADVECTION */
#   endif /* ALLOW_SEAICE */
c--
#   ifdef ALLOW_THSICE
C     Tape for thsice_advection, which is called for thSIce_nAdv (=5)
C     different variables (thSIce_nAdv is set in THSICE_SIZE.h)
CADJ INIT comlev1_thsice_adv  = COMMON,
CADJ &    nchklev_1*nSx*nSy*maxcube*thSIce_nAdv
CADJ INIT comlev1_thsice_nlyr = COMMON,nchklev_1*nSx*nSy*nlyr
CADJ INIT comlev1_thsice_s4t  = COMMON,nchklev_1*nSx*nSy*MaxTsf
#    if (defined ALLOW_EXF && defined ALLOW_BULKFORMULAE)
C     These tapes are only used within thsice_get_exf.F
CADJ INIT comlev1_thsice_exf       = COMMON,
CADJ &    nchklev_1*nSx*nSy*(MaxTsf+1)
CADJ INIT comlev1_thsice_exf_niter = COMMON,
CADJ &    nchklev_1*nSx*nSy*(MaxTsf+1)*niter_bulk
CADJ INIT comlev1_thsice_exf_ij    = COMMON,
CADJ &    nchklev_1*nSx*nSy*(MaxTsf+1)*niter_bulk*sNx*sNy
#    endif
#   endif /* ALLOW_THSICE */
c--
#   ifdef ALLOW_STEEP_ICECAVITY
CADJ INIT comlev1_stic_bibj_ijk = COMMON,
CADJ &    nchklev_1*nSx*nSy*Nr*(2*OLy+sNy)*(2*OLx+sNx)
#   endif
c--
#   ifdef ALLOW_STREAMICE
CADJ INIT comlev1_stream_nl    = COMMON,nchklev_1*streamice_max_nl
CADJ INIT comlev1_stream_front = COMMON,nchklev_1*4
CADJ INIT comlev1_stream_ij
CADJ &     = COMMON,nchklev_1*4*(sNx+2)*nSx*(sNy+2)*nSy
CADJ INIT comlev1_stream_hybrid = COMMON,nchklev_1*sNx*nSx*sNy*nSy*Nr
#   endif
c--
#   ifdef ALLOW_CG2D_NSA
CADJ INIT comlev1_cg2d      = COMMON,nchklev_1
CADJ INIT comlev1_cg2d_iter = COMMON,nchklev_1*numItersMax
#   endif
#  endif /* ALLOW_AUTODIFF_TAMC */
c--
#  ifdef ALLOW_GENCOST_CONTRIBUTION
CADJ INIT comlev1_ngencost = COMMON,nchklev_1*NGENCOST
#  endif
c**************************************

#  ifdef STORE_LOADEDREC_TEST
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            loadedRec(bi,bj) = 0
           ENDDO
          ENDDO
#  endif /* STORE_LOADEDREC_TEST */

#  if ( defined ALLOW_AUTODIFF_TAMC && defined ALLOW_OFFLINE )
C     The following statement forces TAF to store wVel correctly in some
C     outer checkpoint levels in some cases when it does not do it
C     automatically (similar to TICES in seaice_reg_ridge.F). This is a
C     TAF bug and this statement can probably be removed again, once
C     this bug is fixed (but it does not hurt, either).
#   ifndef AUTODIFF_USE_STORE_RESTORE
CADJ INCOMPLETE wVel
#   endif
#  endif
#  ifdef ALLOW_TAMC_CHECKPOINTING

          do ilev_1 = 1,nchklev_1

c--         The if-statement below introduces a some flexibility in the
c--         choice of the 3-tupel ( nchklev_1, nchklev_2, nchklev_3 ).

            iloop = (ilev_2 - 1)*nchklev_1 + ilev_1
#   ifndef AUTODIFF_2_LEVEL_CHECKPOINT
     &            + (ilev_3 - 1)*nchklev_2*nchklev_1
#   endif
#   ifdef AUTODIFF_4_LEVEL_CHECKPOINT
     &            + (ilev_4 - 1)*nchklev_3*nchklev_2*nchklev_1
#   endif

            if ( iloop .le. nTimeSteps ) then

#  else /* ALLOW_TAMC_CHECKPOINTING  undefined */

      DO iloop = 1, nTimeSteps

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  endif /* ALLOW_TAMC_CHECKPOINTING */
# endif /* ALLOW_AUTODIFF */
#endif /* undef ALLOW_OPENAD */
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#ifndef ALLOW_AUTODIFF
      DO iloop = 1, nTimeSteps
#endif /* ALLOW_AUTODIFF */

c--     >>> Loop body start <<<

#ifdef ALLOW_AUTODIFF
      nIter0 = NINT( (startTime-baseTime)/deltaTClock )
C--   Reset the model iteration counter and the model time.
      myIter = nIter0 + (iloop-1)
      myTime = startTime + deltaTClock*(iloop-1)
# ifdef ALLOW_AUTODIFF_TAMC
#  ifdef ALLOW_TAMC_CHECKPOINTING
      ikey_dynamics = ilev_1
#  else
      ikey_dynamics = iloop
#  endif
# endif
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_ECCO
      IF ( useECCO ) then
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('cost_averagesfields',myThid)
# endif
c--   Accumulate time averages of temperature, salinity
        CALL TIMER_START('COST_AVERAGESFIELDS [MAIN_DO_LOOP]',myThid)
        CALL COST_AVERAGESFIELDS( myTime, myThid )
        CALL TIMER_STOP ('COST_AVERAGESFIELDS [MAIN_DO_LOOP]',myThid)
      ENDIF
#endif /* ALLOW_ECCO */

#ifdef ALLOW_PROFILES
      IF (usePROFILES) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('profiles_inloop',myThid)
#endif
c--     Accumulate in-situ time averages of theta, salt, and SSH.
#ifdef ALLOW_AUTODIFF
C--   Reset the model iteration counter and the model time.
        myIter = nIter0 + (iloop-1)
        myTime = startTime + deltaTClock*(iloop-1)
#endif
        CALL TIMER_START('PROFILES_INLOOP    [MAIN_DO_LOOP]', myThid)
        CALL PROFILES_INLOOP( myTime, myThid )
        CALL TIMER_STOP ('PROFILES_INLOOP    [MAIN_DO_LOOP]', myThid)
      ENDIF
#endif

#ifdef ALLOW_OBSFIT
      IF (useOBSFIT) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('obsfit_inloop',myThid)
#endif
c--     Accumulate in-situ time averages of theta, salt, and SSH.
#ifdef ALLOW_AUTODIFF
C--   Reset the model iteration counter and the model time.
        myIter = nIter0 + (iloop-1)
        myTime = startTime + deltaTClock*(iloop-1)
#endif
        CALL TIMER_START('OBSFIT_INLOOP      [MAIN_DO_LOOP]', myThid)
        CALL OBSFIT_INLOOP( myTime, myThid )
        CALL TIMER_STOP ('OBSFIT_INLOOP      [MAIN_DO_LOOP]', myThid)
      ENDIF
#endif

      CALL TIMER_START('MAIN_DO_LOOP        [THE_MAIN_LOOP]', myThid)
      CALL MAIN_DO_LOOP( iloop, myTime, myIter, myThid )
      CALL TIMER_STOP ('MAIN_DO_LOOP        [THE_MAIN_LOOP]', myThid)

#ifdef USE_PDAF
C     PDAF analysis step
      CALL TIMER_START('ASSIMILATE_PDAF     [THE_MAIN_LOOP]', myThid)
      CALL ASSIMILATE_PDAF( myTime, myIter, myThid )
      CALL TIMER_STOP ('ASSIMILATE_PDAF     [THE_MAIN_LOOP]', myThid)
#endif

c--     >>> Loop body end <<<
#ifndef ALLOW_OPENAD
# ifdef ALLOW_AUTODIFF
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
      ENDDO
#   endif /* ALLOW_TAMC_CHECKPOINTING */
# else /* ALLOW_AUTODIFF */
      ENDDO
# endif /* ALLOW_AUTODIFF */
#endif /* undef ALLOW_OPENAD */

c     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STOPS   <<<<<<<<<<<<<<<<<<<<<<<<<<<<

#ifdef ALLOW_ECCO
c--     Accumulate time averages of temperature, salinity
      CALL TIMER_START('COST_AVERAGESFIELDS [THE_MAIN_LOOP]',myThid)
      CALL COST_AVERAGESFIELDS( endtime, myThid )
      CALL TIMER_STOP ('COST_AVERAGESFIELDS [THE_MAIN_LOOP]',myThid)
#endif /* ALLOW_ECCO */

#ifdef ALLOW_PROFILES
      IF (usePROFILES) THEN
c--   Accumulate in-situ time averages of temperature, salinity, SSH.
        CALL TIMER_START('PROFILES_INLOOP    [THE_MAIN_LOOP]', myThid)
        CALL PROFILES_INLOOP( endtime, myThid )
        CALL TIMER_STOP ('PROFILES_INLOOP    [THE_MAIN_LOOP]', myThid)
c--   Compute all profiles cost function contributions.
        CALL COST_PROFILES( myIter, myTime, myThid )
      ENDIF
#endif

#ifdef ALLOW_OBSFIT
      IF (useOBSFIT) THEN
c--   Accumulate in-situ time averages of temperature, salinity, SSH.
        CALL TIMER_START('OBSFIT_INLOOP      [THE_MAIN_LOOP]', myThid)
        CALL OBSFIT_INLOOP( endtime, myThid )
        CALL TIMER_STOP ('OBSFIT_INLOOP      [THE_MAIN_LOOP]', myThid)
c--   Compute all obsfit cost function contributions.
        CALL COST_OBSFIT( myIter, myTime, myThid )
      ENDIF
#endif

#ifdef ALLOW_ECCO
      IF ( useECCO ) then
c--   Compute all ecco cost function contributions.
        CALL TIMER_START('ECCO_COST_DRIVER   [THE_MAIN_LOOP]', myThid)
        CALL ECCO_COST_DRIVER ( endTime, myIter, myThid )
        CALL TIMER_STOP ('ECCO_COST_DRIVER   [THE_MAIN_LOOP]', myThid)
      ENDIF
#endif /* ALLOW_ECCO */

#ifdef ALLOW_COST
c--   Sum all cost function contributions.
      CALL TIMER_START('COST_FINAL         [ADJOINT SPIN-DOWN]', myThid)
      CALL COST_FINAL ( myThid )
      CALL TIMER_STOP ('COST_FINAL         [ADJOINT SPIN-DOWN]', myThid)

# ifdef ALLOW_OPENAD
c$openad DEPENDENT(fc)
# endif /* ALLOW_OPENAD */

#endif /* ALLOW_COST */

#ifdef USE_PDAF
C     PDAF finalization
      CALL FINALIZE_PDAF( )
#endif

      _BARRIER
      CALL TIMER_STOP ('MAIN LOOP           [THE_MAIN_LOOP]', myThid)

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_MAIN_LOOP',myThid)
#endif

      RETURN
      END
