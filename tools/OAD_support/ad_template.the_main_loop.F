#include "PACKAGES_CONFIG.h"
#include "OPENAD_OPTIONS.h"

      subroutine template()
      use OAD_auxillary_cp
      use OAD_tape
      use OAD_rev

c we may need these for the checkpointing
      use SIZE_mod
      use EEPARAMS_mod
      use PARAMS_mod
      use BAR2_mod
      use BARRIER_mod
#ifdef ALLOW_PTRACERS
      use ptracers_fields_mod
      use ptracers_start_mod
#endif
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
#ifdef ALLOW_SHELFICE

      use shelfice_mod
      use shelfice_cost_mod
      use obcs_fields_mod
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
#ifdef ALLOW_DIC
      use dic_cost_mod
      use dic_vars_mod
      use dic_load_mod
      use dic_atmos_mod
#endif

!$TEMPLATE_PRAGMA_DECLARATIONS

      integer :: currcp, curradjointcp, maxfwditer, maxadjiter
      integer :: cp_loop_variable_1,cp_loop_variable_2,
     +     cp_loop_variable_3,cp_loop_variable_4
      logical :: exst
      integer iaddr
      external iaddr
      
      ! Temporaries to hold the stack pointers
      integer :: temp_double_tape_pointer
      integer :: temp_integer_tape_pointer
      integer :: temp_logical_tape_pointer
      integer :: temp_string_tape_pointer
      logical :: isfirst
#ifdef OAD_DEBUG_SPLIT1
      character*(80):: indentation='                                        
     +                                         '
      our_indent=our_indent+1

      write(standardmessageunit, '(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'enter __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 
#endif

      if (our_rev_mode%plain) then
#ifdef OAD_DEBUG_SPLIT1
        write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering plain'
#endif
!$PLACEHOLDER_PRAGMA$ id=1
      end if
      if (our_rev_mode%tape .and..not.our_rev_mode%strictAnonymous) then
#ifdef OAD_DEBUG_SPLIT1
        write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering tape'
#endif
!$PLACEHOLDER_PRAGMA$ id=2
      end if 
      if (our_rev_mode%adjoint 
     +.and. .not.our_rev_mode%strictAnonymous) then
#ifdef OAD_DEBUG_SPLIT1
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering adjoint'
#endif
!$PLACEHOLDER_PRAGMA$ id=3
      end if 
      if (our_rev_mode%tape .and. our_rev_mode%strictAnonymous) then
#ifdef OAD_DEBUG_SPLIT1
        write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering sa tape'
#endif
      call cp_read_state(currcp, curradjointcp, maxfwditer, maxadjiter,
     +myIter)
      !Store the stack pointers
      isfirst = .false.
      if (currcp .eq. 0) then
        isfirst = .true.
      end if
      if(currcp.ne.0.) then
        call cp_read_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=9
        call cp_close
      end if
!$PLACEHOLDER_PRAGMA$ id=10
      call cp_read_state(currcp, curradjointcp, maxfwditer, maxadjiter,
     +myIter)
      call cp_write_open(currcp)
!$PLACEHOLDER_PRAGMA$ id=8
      call cp_close
      if (isfirst.eqv..true.) then 
        call cp_write_tape_state(1)
      else if (currcp.eq.nTimeSteps_l2) then
        call cp_write_tape_state(2)
      else
        call cp_write_tape_state(3)
      end if
      end if 
      if (our_rev_mode%adjoint 
     +.and. our_rev_mode%strictAnonymous) then
#ifdef OAD_DEBUG_SPLIT1
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering sa adjoint'
#endif
      call cp_read_state(currcp, curradjointcp, maxfwditer, maxadjiter,
     +myIter)
      !Retore the stack pointers
      isfirst = .false.
      if (curradjointcp.eq.-1) then
        isfirst = .true.
      end if 
      if (isfirst.eqv..true.) then                         
        call cp_read_tape_state(2)
      else if (curradjointcp-maxadjiter.le.0) then
        call cp_read_tape_state(1)
      else
        call cp_read_tape_state(3)
      end if
      call cp_read_open(currcp)
      print *, 'DIVA main_loop read currcp', currcp
!$PLACEHOLDER_PRAGMA$ id=9
      call cp_close
      call cp_read_state(currcp, curradjointcp, maxfwditer, maxadjiter,
     +myIter)
      if(curradjointcp.ne.-1) then
        print *, 'DIVA main_loop read curradjointcp', curradjointcp
        call cp_read_open(curradjointcp+nTimeSteps_l2+1)
!$PLACEHOLDER_PRAGMA$ id=13
        call cp_close
      end if
!$PLACEHOLDER_PRAGMA$ id=11
      call cp_read_state(currcp, curradjointcp, maxfwditer, maxadjiter,
     +myIter)
      print *, 'DIVA main_loop write curradjointcp', curradjointcp
      call cp_write_open(curradjointcp+nTimeSteps_l2+1)
!$PLACEHOLDER_PRAGMA$ id=12
      call cp_close
      end if 

#ifdef OAD_DEBUG_SPLIT1
      write(standardmessageunit,'(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'leave __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 

      our_indent=our_indent-1
#endif
      end subroutine template
