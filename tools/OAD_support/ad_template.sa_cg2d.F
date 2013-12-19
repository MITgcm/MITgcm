      subroutine template()
      use OAD_cp
      use OAD_tape
      use OAD_rev

!$TEMPLATE_PRAGMA_DECLARATIONS

      integer :: cp_loop_variable_1,cp_loop_variable_2,
     +     cp_loop_variable_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      INTEGER numItersHelper, myThidHelper, nIterMinHelper

      Real*8  cg2d_b_p(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8  cg2d_x_p(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef OAD_DEBUG_JOINT
      character*(80):: indentation='                                        
     +                                         '
      our_indent=our_indent+1

      write(standardmessageunit, '(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'enter __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 
#endif
      if (our_rev_mode%plain) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering plain'
#endif
c set up for plain execution
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: runninng plain / down plain'
#endif
         cg2d_b_p=cg2d_b%v
         cg2d_x_p=cg2d_x%v
	call cg2d (
     I                cg2d_b_p,
     U                cg2d_x_p,
     O                firstResidual,
     O                minResidualSq,
     O                lastResidual,
     U                numIters,
     O                nIterMin,
     I                myThid) 
        cg2d_x%v=cg2d_x_p        
c reset the mode
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then

         numItersHelper=numiters
         mythidHelper=mythid
         nIterMinHelper=nIterMin
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering tape'
#endif
c set up for plain execution
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: runninng plain / down plain'
#endif
         cg2d_b_p=cg2d_b%v
         cg2d_x_p=cg2d_x%v
	call cg2d (
     I                cg2d_b_p,
     U                cg2d_x_p,
     O                firstResidual,
     O                minResidualSq,
     O                lastResidual,
     U                numIters,
     O                nIterMin,
     I                myThid) 
        cg2d_x%v=cg2d_x_p        
c reset the mode
         our_rev_mode=our_orig_mode
c manually push integers to the tape:
         call oad_tape_push(numItersHelper)
         call oad_tape_push(nIterMinHelper)
         call oad_tape_push(mythidHelper)
      end if
      if (our_rev_mode%adjoint) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering adjoint'
#endif
c manually pop two integers from the tape:        
         call oad_tape_pop(mythid)
         call oad_tape_pop(nIterMin)
         call oad_tape_pop(numiters)
c selfadjoint:
c the original is called with 
c  cg2d(b,x,...)
c in the adjoint context if we 
c use the same code base 
c we call with 
c  cg2d(x_bar,bh,...
c where afterwards
c b_bar+=bh  and x_bar=0
c the adjoint second formal argument cg2d_x should be
c the values of the first argument:  
      cg2d_b_p=cg2d_x%d
c the first formal argument cg2d_b should hold 
c the increment, i.e. we nullify the second formal 
c argument (cg2d_x) value:
      cg2d_x_p=0.0
c set up for plain execution
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: runninng self adjoint / down plain'
#endif
	call cg2d (
     I                cg2d_b_p,
     U                cg2d_x_p,
     O                firstResidual,
     O                minResidualSq,
     O                lastResidual,
     U                numIters,
     O                nIterMin,
     I                myThid) 
c reset the mode
         our_rev_mode=our_orig_mode
c now the adjoint result is the increment 
c contained in the second formal argument
         cg2d_b%d= cg2d_b%d+cg2d_x_p
         cg2d_x%d=0.0
      end if 
#ifdef OAD_DEBUG_JOINT
      write(standardmessageunit,'(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'leave __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 

      our_indent=our_indent-1
#endif

      end subroutine template
