#include "PACKAGES_CONFIG.h"
#include "OPENAD_OPTIONS.h"

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
#ifdef ALLOW_GGL90
      use GGL90_mod
      use GGL90_TAVE_mod
#endif
#ifdef ALLOW_GMREDI
      use GMREDI_mod
      use GMREDI_TAVE_mod
#endif
      use GRID_mod
#ifdef ALLOW_KPP
      use KPP_mod
      use KPP_PARAMS_mod
      use KPP_TAVE_mod
#endif
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
      LOGICAL :: initialized=.FALSE.
      TYPE(rvAction),save :: theAction
      CHARACTER(80) :: errorMsg
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
#ifdef ALLOW_OPENAD_DIVA
      DO iloop1 = 1, nTimeSteps
        PROD = (ILOOP1 + NTIMESTEPS_L2 *(ILOOP +(-1)))
        print *, 'DIVA Revolve Plain PROD = ', PROD
        CALL OpenAD_forward_step( PROD, mytime, myiter, mythid )
      enddo
#else 
      DO iloop = 1, nTimeSteps
        CALL OpenAD_forward_step( iloop, mytime, myiter, mythid )
      enddo
#endif
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
#ifdef ALLOW_OPENAD_DIVA 
                   PROD = (currIter+1 + NTIMESTEPS_L2 *(ILOOP +(-1)))
                   print *, 'DIVA Revolve Tape rvForward PROD = ', PROD
                   CALL OpenAD_forward_step( PROD, mytime, 
     +myiter, mythid )
#else
                   CALL OpenAD_forward_step( currIter+1, mytime,
     +myiter, mythid ) 
#endif
                end do
                call OAD_revTape
             case (rvFirstUTurn)
#ifdef ALLOW_OPENAD_DIVA 
                PROD = (currIter+1 + NTIMESTEPS_L2 *(ILOOP +(-1)))
                print *, 'DIVA Revolve Tape rvFirstUTurn PROD = ', PROD
                CALL OpenAD_forward_step( PROD, mytime, myiter, 
     +mythid )
#else
                CALL OpenAD_forward_step( currIter+1, mytime, myiter,
     +mythid )
#endif
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
#ifdef ALLOW_OPENAD_DIVA 
                PROD = (currIter+1 + NTIMESTEPS_L2 *(ILOOP +(-1))) 
                print *, 'DIVA Revolve Adjoint rvFirstUTurn PROD = ', PROD
                CALL OpenAD_forward_step( PROD, mytime, myiter, 
     +mythid )
#else
                CALL OpenAD_forward_step( currIter+1, mytime, myiter, 
     +mythid )
#endif
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
#ifdef ALLOW_OPENAD_DIVA
                   PROD = (currIter+1 + NTIMESTEPS_L2 *(ILOOP +(-1)))
                   print *, 'DIVA Revolve Adjoint rvForward PROD = ', PROD 
                   CALL OpenAD_forward_step( PROD, mytime, myiter,
     + mythid )
#else
                   CALL OpenAD_forward_step( currIter+1, mytime, myiter,
     + mythid )
#endif
                end do
                call OAD_revAdjoint
             case (rvUTurn)
#ifdef ALLOW_OPENAD_DIVA
                PROD = (currIter+1 + NTIMESTEPS_L2 *(ILOOP +(-1))) 
#endif
                call OAD_revTape
#ifdef ALLOW_OPENAD_DIVA
                print *, 'DIVA Revolve Adjoint rvUTurn tp PROD = ', PROD 
                CALL OpenAD_forward_step( PROD, mytime, myiter, 
     +mythid )
#else
                CALL OpenAD_forward_step( currIter+1, mytime, myiter,
     +mythid )
#endif
                call OAD_revAdjoint
#ifdef ALLOW_OPENAD_DIVA
                print *, 'DIVA Revolve Adjoint rvUTurn ad PROD = ', PROD 
                CALL OpenAD_forward_step( PROD, mytime, myiter, 
     +mythid )
#else
                CALL OpenAD_forward_step( currIter+1, mytime, myiter,
     +mythid )
#endif
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
