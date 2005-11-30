      subroutine template()
      use OpenAD_checkpoints
      use OpenAD_tape
      use OpenAD_rev

c we may need these for the checkpointing
      use SIZE_mod
      use EEPARAMS_mod
      use PARAMS_mod
      use BAR2_mod
      use BARRIER_mod
      use CD_CODE_VARS_mod
      use CG2D_mod
      use CG3D_mod
      use DFILE_mod
      use DYNVARS_mod
      use EEIO_mod
      use EESUPPORT_mod
      use EOS_mod
      use EXCH_JAM_mod
      use EXCH_mod
      use FC_NAMEMANGLE_mod
      use FFIELDS_mod
      use GAD_mod
      use GLOBAL_MAX_mod
      use GLOBAL_SUM_mod
      use GRID_mod
      use GW_mod
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
      
!$TEMPLATE_PRAGMA_DECLARATIONS

      integer :: cp_loop_variable_1,cp_loop_variable_2,
     +     cp_loop_variable_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                        
     +                                         '

      our_indent=our_indent+1

      write(*,'(A,A,L,L,L,L,L)') 
     +indentation(1:our_indent), "enter __SRNAME__:",
     +our_rev_mode%arg_store,
     +our_rev_mode%arg_restore,
     +our_rev_mode%plain,
     +our_rev_mode%tape,
     +our_rev_mode%adjoint

      write(*,'(A,A,A,I6,A,I6,A,I6,A,I6,A,I8,A,I8)')
     +indentation(1:our_indent), "enter __SRNAME__:",
     +" CD:",cp_double_pointer, 
     +" CI:",cp_integer_pointer, 
     +" CB:",cp_boolean_pointer, 
     +" CS:",cp_string_pointer, 
     +" TD:",double_tape_pointer, 
     +" TI:",integer_tape_pointer

      if (our_rev_mode%arg_store) then 
         write(*,'(A,A)') 
     +indentation(1:our_indent), " __SRNAME__: entering arg store"
!$PLACEHOLDER_PRAGMA$ id=4
      end if 
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') 
     +indentation(1:our_indent), " __SRNAME__: entering arg restore"
!$PLACEHOLDER_PRAGMA$ id=6
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') 
     +indentation(1:our_indent), " __SRNAME__: entering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
!$PLACEHOLDER_PRAGMA$ id=1
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') 
     +indentation(1:our_indent), " __SRNAME__: entering tape"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
!$PLACEHOLDER_PRAGMA$ id=2
         write(*,'(A,A)') 
     +indentation(1:our_indent), " __SRNAME__: following with adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.TRUE.
!$PLACEHOLDER_PRAGMA$ id=3
         our_rev_mode=our_orig_mode
      end if 

          write(*,'(A,A,A,I6,A,I6,A,I6,A,I6,A,I8,A,I8)')
     +indentation(1:our_indent), "leave __SRNAME__:",
     +" CD:",cp_double_pointer, 
     +" CI:",cp_integer_pointer, 
     +" CB:",cp_boolean_pointer, 
     +" CS:",cp_string_pointer, 
     +" TD:",double_tape_pointer, 
     +" TI:",integer_tape_pointer

      write(*,'(A,A,L,L,L,L,L)') 
     +indentation(1:our_indent), "leave __SRNAME__:",
     +our_rev_mode%arg_store,
     +our_rev_mode%arg_restore,
     +our_rev_mode%plain,
     +our_rev_mode%tape,
     +our_rev_mode%adjoint

      our_indent=our_indent-1

      end subroutine template
