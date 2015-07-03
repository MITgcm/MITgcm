#include "PACKAGES_CONFIG.h"
#include "OPENAD_OPTIONS.h"

      subroutine template()
#ifdef ALLOW_OPENAD_DIVA
      use OAD_regular_cp
      use OAD_tape
      use OAD_rev

c we may need these for the checkpointing
      use SIZE_mod
      use EEPARAMS_mod
      use PARAMS_mod
      use BAR2_mod
      use BARRIER_mod
#ifdef ALLOW_CD_CODE
      use CD_CODE_VARS_mod
#endif
      use CG2D_mod
      use CG3D_mod
      use DYNVARS_mod
      use EESUPPORT_mod
      use EOS_mod
      use EXCH_mod
      use FC_NAMEMANGLE_mod
      use FFIELDS_mod
#ifdef ALLOW_GENERIC_ADVDIFF
      use GAD_mod
#endif
      use GLOBAL_MAX_mod
      use GLOBAL_SUM_mod
#ifdef ALLOW_GMREDI
      use GMREDI_mod
      use GMREDI_TAVE_mod
#endif
      use GRID_mod
#ifdef ALLOW_MOM_COMMON
      use MOM_VISC_mod
#endif
      use MPI_INFO_mod
#ifdef ALLOW_SHAP_FILT
      use SHAP_FILT_mod
#endif
#ifdef ALLOW_STREAMICE
      use STREAMICE_mod
      use STREAMICE_ADV_mod
      use STREAMICE_BDRY_mod
      use STREAMICE_CG_mod
#endif
      use SOLVE_FOR_PRESSURE3D_mod
      use SOLVE_FOR_PRESSURE_mod
      use SURFACE_mod
      use tamc_mod
      use tamc_keys_mod
      use cost_mod
      use g_cost_mod
      use ctrl_mod
      use ctrl_dummy_mod
      use ctrl_weights_mod
      use optim_mod
      use grdchk_mod
      
!$TEMPLATE_PRAGMA_DECLARATIONS
      integer, save :: currcp, curradjointcp
      integer :: cp_loop_variable_1,cp_loop_variable_2,
     +     cp_loop_variable_3,cp_loop_variable_4,cp_loop_variable_5

      double precision, dimension(:), allocatable, save :: theArgFStack
      integer, save :: theArgFStackoffset=0, theArgFStackSize=0
      integer, dimension(:), allocatable, save :: theArgIStack
      integer, save :: theArgIStackoffset=0, theArgIStackSize=0
      type(modeType) :: our_orig_mode
      integer nt

      integer iaddr
      external iaddr
      if (our_rev_mode%arg_store) then 
        call cp_write_open()
!$PLACEHOLDER_PRAGMA$ id=8
        call cp_close()
      end if 
      if (our_rev_mode%arg_restore) then
        call cp_read_open()
!$PLACEHOLDER_PRAGMA$ id=9
        call cp_close()
      end if
      if (our_rev_mode%plain) then
        do nt=1,nTimeSteps_l2
          CALL OpenAD_inner_do_loop(nt, MYTIME, MYITER, MYTHID)
        enddo
      end if
      if (our_rev_mode%tape) then
        print *, 'DIVA Running Regular Plain nTimeSteps_l2 ', nTimeSteps
     +_l2
        currcp = 0
        do nt=1,nTimeSteps_l2
          print *, 'DIVA Checkpointing currcp  and running plain', currc
     +p
            call cp_write_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=8
            call cp_close
            currcp=currcp+1
          call OAD_revPlain
          CALL OpenAD_inner_do_loop(nt, MYTIME, MYITER, MYTHID)
          call OAD_revTape
        end do
        currcp = currcp -1
      end if
      if (our_rev_mode%adjoint) then
        curradjointcp = currcp
        print *, 'DIVA Reading plain checkpoint currcp ', currcp
        call cp_read_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=9
        call cp_close
        call OAD_revTape
        print *, 'DIVA Running First Tape'
        CALL OpenAD_inner_do_loop(nTimeSteps_l2, MYTIME, MYITER, MYTHID)
        print *, 'DIVA Running First Adjoint '
        call OAD_revAdjoint
        CALL OpenAD_inner_do_loop(nTimeSteps_l2, MYTIME, MYITER, MYTHID)
        print *, 'DIVA Writing adjoint checkpoint curradjointcp ', curra
     +djointcp
        call cp_write_open(curradjointcp)
!$PLACEHOLDER_PRAGMA$ id=12
        call cp_close
        do nt=nTimeSteps_l2-1,1,-1
          currcp = currcp -1
          print *, 'DIVA Running TA pairs'
          print *, 'DIVA Reading plain checkpoint currcp ', currcp
          call cp_read_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=9
          call cp_close
          call OAD_revTape
          print *, 'DIVA Running Tape'
          CALL OpenAD_inner_do_loop(nt, MYTIME, MYITER, MYTHID)
          print *, 'DIVA Reading adjoint checkpoint curradjointcp ', cur
     +radjointcp
          call cp_read_open(curradjointcp)
!$PLACEHOLDER_PRAGMA$ id=13
          call cp_close
          print *, 'DIVA Running adjoint '
          call OAD_revAdjoint
          CALL OpenAD_inner_do_loop(nt, MYTIME, MYITER, MYTHID)
          print *, 'DIVA Writing adjoint checkpoint curradjointcp ', cur
     +radjointcp-1
         call cp_write_open(curradjointcp-1)
!$PLACEHOLDER_PRAGMA$ id=12
          curradjointcp = curradjointcp-1
          call cp_close
        end do
      end if
#endif
      end subroutine template
