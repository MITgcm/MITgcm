      subroutine template()
      use OAD_cp
      use OAD_tape
      use OAD_rev
      use revolve

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
      LOGICAL :: initialized=.FALSE.
      TYPE(rvAction),save :: theAction
      CHARACTER , DIMENSION(80) :: errorMsg
      integer, save :: jointCPCount
      integer, save :: currIter

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

      nIter0 = NINT( (startTime-baseTime)/deltaTClock )
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
      DO iloop = 1, nTimeSteps
        CALL OpenAD_forward_step( iloop, mytime, myiter, mythid )
      enddo
      end if
      if (our_rev_mode%tape) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: run tape, down revolve until first U turn'
#endif
         currIter=0
         jointCPcount=cp_fNumber()
         initialized=rvInit(nTimeSteps,120,
     +                      errorMsg,theAction)
         IF (.NOT.initialized) WRITE(*,'(A,A)') 'Error: ', errorMsg
         do while (theAction%actionFlag/=rvDone)
           theAction=rvNextAction()
           select case (theAction%actionFlag) 
             case (rvStore)
                call cp_write_open(theAction%cpNum+jointCPCount)
!$PLACEHOLDER_PRAGMA$ id=8
                call cp_close
             case (rvForward)
                call OAD_revPlain
                do currIter=currIter,theAction%iteration-1
                   CALL OpenAD_forward_step( currIter+1, mytime, 
     +myiter, mythid )
                end do
                call OAD_revTape
             case (rvFirstUTurn)
                CALL OpenAD_forward_step( currIter+1, mytime, myiter, 
     +mythid )
! get out now ...
                exit                
             end select   
          end do
      end if 
      if (our_rev_mode%adjoint) then
        IF (.NOT.initialized) WRITE(*,'(A)') 'Error: not initialized'
        do while (theAction%actionFlag/=rvDone)
           select case (theAction%actionFlag) 
             case (rvFirstUTurn)
!we taped already ... see above
                CALL OpenAD_forward_step( currIter+1, mytime, myiter, 
     +mythid )
             case (rvStore)
                call cp_write_open(theAction%cpNum+jointCPCount)
!$PLACEHOLDER_PRAGMA$ id=8
                call cp_close
             case (rvRestore)
                call cp_read_open(theAction%cpNum+jointCPCount)
!$PLACEHOLDER_PRAGMA$ id=9
                currIter=theAction%iteration
                call cp_close
             case (rvForward)
                call OAD_revPlain
                do currIter=currIter,theAction%iteration-1
                   CALL OpenAD_forward_step( currIter+1, mytime, myiter,
     + mythid )
                end do
                call OAD_revAdjoint
             case (rvUTurn)
                call OAD_revTape
                CALL OpenAD_forward_step( currIter+1, mytime, myiter, 
     +mythid )
                call OAD_revAdjoint
                CALL OpenAD_forward_step( currIter+1, mytime, myiter, 
     +mythid )
           end select   
           theAction=rvNextAction()
        end do
      end if 

#ifdef OAD_DEBUG_JOINT
      write(standardmessageunit,'(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'leave __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 

      our_indent=our_indent-1
#endif

      end subroutine template
