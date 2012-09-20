      subroutine template()
      use OAD_cp
      use OAD_tape
      use OAD_rev

c we may need these for the checkpointing
      use SIZE_mod
      use EEPARAMS_mod
      use PARAMS_mod
      use BAR2_mod
      use BARRIER_mod
      use CD_CODE_VARS_mod
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
      use GMREDI_mod
      use GMREDI_TAVE_mod
      use GRID_mod
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
     +     cp_loop_variable_3,cp_loop_variable_4,cp_loop_variable_5

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

#ifdef OAD_DEBUG_JOINT
      character*(80):: indentation='                                        
     +                                         '
      our_indent=our_indent+1

      write(standardmessageunit, '(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'enter __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 
#endif

      if (our_rev_mode%switchedToCheckpoint) then

      if (our_rev_mode%arg_store) then 
	call cp_write_open()
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering arg store'
#endif
!$PLACEHOLDER_PRAGMA$ id=8
       call cp_close()
      end if 
      if (our_rev_mode%arg_restore) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering arg restore'
#endif
	call cp_read_open()
!$PLACEHOLDER_PRAGMA$ id=9
	call cp_close()
      end if
      if (our_rev_mode%plain) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: run plain, down plain'
#endif
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
!$PLACEHOLDER_PRAGMA$ id=1
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: run tape, down arg store/plain'
#endif
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
!$PLACEHOLDER_PRAGMA$ id=2
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: following with adjoint, down arg restor/tape'
#endif
!$PLACEHOLDER_PRAGMA$ id=3
         our_rev_mode=our_orig_mode
      end if 

      else

      our_rev_mode%switchedToCheckpoint = .TRUE.

      if (our_rev_mode%arg_store) then 
         write(errorMessageUnit,'(A)') 
     +'OAD ERROR __SRNAME__: s2c arg store is the wrong logic'
        stop
      end if 
      if (our_rev_mode%arg_restore) then
         write(errorMessageUnit,'(A)') 
     +'OAD ERROR __SRNAME__: s2c arg restore is the wrong logic'
        stop
      end if
      if (our_rev_mode%plain) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: s2c entering plain'
#endif
         our_orig_mode=our_rev_mode
!$PLACEHOLDER_PRAGMA$ id=1
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: s2c entering as tape'
#endif
c we have to switch from split to checkpointing mode
c i.e. outside taping means top level checkpointing here
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: s2c store argument cp'
#endif
	call cp_write_open()
!$PLACEHOLDER_PRAGMA$ id=8
	call cp_close()
c now do the forward run down a plain
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), ' __SRNAME__: s2c run plain'
#endif
!$PLACEHOLDER_PRAGMA$ id=1
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%adjoint) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: s2c entering as adjoint'
#endif
c we have to switch from split to checkpointing mode
c i.e. outside adjoint means argument restore, taping and then  adjoint here
c argument restore
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: s2c restore argument cp'
#endif
	call cp_read_open()
!$PLACEHOLDER_PRAGMA$ id=9
	call cp_close()
c setup checkpointing downwards
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
c taping this level
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: s2c run tape, down arg store/plain'
#endif
!$PLACEHOLDER_PRAGMA$ id=2
c setting up for taping downwards
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.TRUE.
c adjoining this level
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: s2c run adjoint, down arg restore/tape'
#endif
!$PLACEHOLDER_PRAGMA$ id=3
         our_rev_mode=our_orig_mode
      end if 

      our_rev_mode%switchedToCheckpoint=.FALSE.

      end if

#ifdef OAD_DEBUG_JOINT
      write(standardmessageunit,'(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'leave __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 

      our_indent=our_indent-1
#endif

      end subroutine template
