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

      Real*8 cg2d_x_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cg2d_x_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cg2d_b_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cg2d_b_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      Real*8 aw2d_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 aw2d_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 ac2d_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 ac2d_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 as2d_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 as2d_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

!      Real*8 cg_Bu_p  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      Real*8 cg_Bv_p  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!       SUBROUTINE OpenAD_cg2d_no_nsa(CG2D_B_IN, CG2D_X_IN, FIRSTRESIDUAL
!     +, MINRESIDUALSQ, LASTRESIDUAL, NUMITERS, NITERMIN, MYTHID)

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
         cg2d_b_v = cg2d_b_in%v
         cg2d_x_v = cg2d_x_in%v
         ac2d_v = ac2d_in%v
         aw2d_v = aw2d_in%v
         as2d_v = as2d_in%v
         call CG2D_NO_NSA( 
     U        cg2d_b_v,     
     U        cg2d_x_v,     
     U        ac2d_v, as2d_v, aw2d_v,
     O        firstResidual,
     O        minResidualSq,
     O        lastResidual,
     U        numIters,
     O        nIterMin,
     I        myThid) 
         cg2d_x_in%v = cg2d_x_v
c reset the mode
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then

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

         cg2d_x_v = cg2d_x_in%v
         cg2d_b_v = cg2d_b_in%v
         ac2d_v = ac2d_in%v
         aw2d_v = aw2d_in%v
         as2d_v = as2d_in%v
         call oad_tape_push(cg2d_b_v)      
         call oad_tape_push(ac2d_v)      
         call oad_tape_push(as2d_v)      
         call oad_tape_push(aw2d_v)      
         call oad_tape_push(firstResidual)      
         call oad_tape_push(minResidualSq)      
         call oad_tape_push(lastResidual)      
         call oad_tape_push(numIters)      
         call oad_tape_push(nIterMin)      
         call oad_tape_push(myThid)      
         call CG2D_NO_NSA(
     U        cg2d_b_v,
     U        cg2d_x_v,
     U        ac2d_v, as2d_v, aw2d_v,
     O        firstResidual,
     O        minResidualSq,
     O        lastResidual,
     U        numIters,
     O        nIterMin,
     I        myThid)
         cg2d_x_in%v = cg2d_x_v
         call oad_tape_push(cg2d_x_v)     
c reset the mode
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%adjoint) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering adjoint'
#endif
         call oad_tape_pop(cg2d_x_v)
         call oad_tape_pop(myThid)
         call oad_tape_pop(nIterMin)
         call oad_tape_pop(numIters)
         call oad_tape_pop(lastResidual)
         call oad_tape_pop(minResidualSq)
         call oad_tape_pop(firstResidual)
         call oad_tape_pop(aw2d_v)
         call oad_tape_pop(as2d_v)
         call oad_tape_pop(ac2d_v)
         call oad_tape_pop(cg2d_b_v)   
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
         cg2d_x_d = cg2d_x_in%d
         cg2d_b_d = cg2d_b_in%d
         ac2d_d = ac2d_in%d
         aw2d_d = aw2d_in%d
         as2d_d = as2d_in%d
      call ADCG2D_NO_NSA(
     I                cg2d_b_v, ! Right hand side, forward
     U                cg2d_b_d, ! Right hand side, adjoint (output)
     I                cg2d_x_v, ! scaled eta,      forward 
     U                cg2d_x_d, ! scaled eta,      adjoint (input)
     U                ac2d_v,
     U                ac2d_d,
     U                as2d_v,
     U                as2d_d,
     U                aw2d_v,
     U                aw2d_d,
     O                firstResidual,
     O                minResidualSq,
     O                lastResidual,
     U                numIters,
     U                nIterMin,
     I                myThid )
c reset the mode
         cg2d_x_in%d = cg2d_x_d
         cg2d_b_in%d = cg2d_b_d
         ac2d_in%d = ac2d_d
         as2d_in%d = as2d_d
         aw2d_in%d = aw2d_d
         our_rev_mode=our_orig_mode
      end if 
#ifdef OAD_DEBUG_JOINT
      write(standardmessageunit,'(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'leave __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 

      our_indent=our_indent-1
#endif

      end subroutine template
