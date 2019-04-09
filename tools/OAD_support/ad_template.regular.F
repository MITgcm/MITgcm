#include "PACKAGES_CONFIG.h"
#include "OPENAD_OPTIONS.h"

      subroutine template()
      use OAD_regular_cp
      use OAD_tape
      use OAD_rev
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
      use GAD_mod
      use GLOBAL_MAX_mod
      use GLOBAL_SUM_mod
#ifdef ALLOW_GMREDI
      use GMREDI_mod
      use GMREDI_TAVE_mod
#endif
      use GRID_mod
      use MOM_VISC_mod
      use MPI_INFO_mod
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
#ifdef ALLOW_DIC
      use dic_cost_mod
      use dic_vars_mod
      use dic_load_mod
      use dic_atmos_mod
      use dic_ctrl_mod
#endif
#ifdef ALLOW_PTRACERS
      use ptracers_size_mod
      use ptracers_fields_mod
      use ptracers_start_mod
#endif

C we may need these for the checkpointing
      use OAD_active
      use w2f__types
      use oad_intrinsics
      use size_mod
      use eeparams_mod
      use params_mod
      use ctrl_size_mod
      use ctrl_genarr_mod
      use ctrl_obcs_mod
#ifdef ALLOW_GCHEM
      use gchem_size_mod
      use gchem_fields_mod
#endif
#endif      
!$TEMPLATE_PRAGMA_DECLARATIONS
      integer, save :: currcp, curradjointcp, maxfwditer, maxadjiter
      integer myfwditer, myadjiter
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
        call cp_read_state(currcp, curradjointcp, maxfwditer,maxadjiter,
     +myIter)
        myfwditer = 0
        if (curradjointcp .eq. -1 .and. currcp.ne. nTimeSteps_l2) then 
          if(currcp.eq.0) then
            call cp_write_tape_state(4)
          end if
          do nt=currcp+1,nTimeSteps_l2
            print *, 'DIVA Checkpointing currcp  and running plain', cur
     +rcp
            if(nt.eq.1) then 
              print *, 'DIVA Writing currcp becuase nt.eq.1'
              call cp_write_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=8
              call cp_close
            else if(myfwditer.eq.0) then
              print *, 'DIVA Reading currcp becuase myfwditer.eq.0'
              call cp_read_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=9
              call cp_close
            end if 
            if(myfwditer.eq.maxfwditer) then
              !stop 'DIVA End of forward run after checkpointing'
              print *, 'DIVA End of forward run after checkpointing'
              call OAD_revTape
              return    
            end if
            call OAD_revPlain
            CALL OpenAD_inner_do_loop(nt, MYTIME, MYITER, MYTHID)
            currcp=currcp+1
            call cp_write_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=8
            call cp_close
            call cp_write_state(currcp, curradjointcp, maxfwditer, maxad
     +jiter, myIter)
            myfwditer = myfwditer+1
          end do
          currcp = currcp-1
          !stop 'DIVA End of forward run after nt.eq.nTimeSteps_l2'
          print *, 'DIVA End of forward run after nt.eq.nTimeSteps_l2'
          call OAD_revTape
          return
        else if (currcp .eq. nTimeSteps_l2) then
          print *, 'DIVA Reading currcp and not running plain because curr
     +cp .eq. nTimeSteps_l2', currcp-1
          call cp_read_open(currcp-1)
!$PLACEHOLDER_PRAGMA$ id=9
          call cp_close
          call OAD_revPlain
          CALL OpenAD_inner_do_loop(nTimeSteps_l2, MYTIME, MYITER, MYTHI
     +D)
        end if
        call OAD_revTape
      end if
      if (our_rev_mode%adjoint) then
        call cp_read_state(currcp, curradjointcp, maxfwditer, maxadjiter
     +, myIter)
        if(currcp.ne.0) then
          myadjiter = 0
          do nt=currcp,1,-1
            curradjointcp = currcp
            currcp = currcp -1
            print *, 'DIVA Running TA pairs ', nt
            print *, 'DIVA Reading plain checkpoint currcp ', currcp
            call cp_read_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=9
            call cp_close
            call OAD_revTape
            print *, 'DIVA Running Tape'
            CALL OpenAD_inner_do_loop(nt, MYTIME, MYITER, MYTHID)          
            if(nt.eq.1) then
            print *, 'DIVA nteq1 Reading plain checkpoint currcp',currcp
            call cp_read_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=9
            call cp_close
            end if 
            if (nt .ne. nTimeSteps_l2) then
              print *, 'DIVA Reading adjoint checkpoint curradjointcp ',
     +curradjointcp
              call cp_read_open(curradjointcp)
!$PLACEHOLDER_PRAGMA$ id=13
              call cp_close
            end if
            print *, 'DIVA Running adjoint '
            call OAD_revAdjoint
            CALL OpenAD_inner_do_loop(nt, MYTIME, MYITER, MYTHID)
                if(curradjointcp-1.eq.0) then
            call cp_read_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=9
            call cp_close
            !      myiter = myiter-1
                end if
            print *, 'DIVA Writing adjoint checkpoint curradjointcp ', c
     +urradjointcp-1
            call cp_write_open(curradjointcp-1)
!$PLACEHOLDER_PRAGMA$ id=12
            curradjointcp = curradjointcp-1
            call cp_close
            call cp_write_state(currcp, curradjointcp, maxfwditer, maxad
     +jiter, myIter)
            myadjiter= myadjiter+1
            if(myadjiter.eq.maxadjiter .and. nt.ne.1) then
              print *, 'DIVA End of adjoint run after checkpointing nt 
     +', nt
              !stop 0
              call OAD_revAdjoint
              return
            end if
          end do
          call cp_read_tape_state(4)
        end if      
      end if
      end subroutine template
