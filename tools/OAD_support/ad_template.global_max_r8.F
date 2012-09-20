      subroutine template()
      use OAD_rev

!$TEMPLATE_PRAGMA_DECLARATIONS

      type(modeType) :: our_orig_mode

c lovcal vars:

      real*8 maxPhip

#ifdef OAD_DEBUG_SPLIT2

      character*(80):: indentation='                                        
     +                                         '
      our_indent=our_indent+1

      write(standardmessageunit, '(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'enter __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 
#endif

      if (our_rev_mode%plain .OR. our_rev_mode%tape) then
#ifdef OAD_DEBUG_SPLIT2
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering plain or tape'
#endif
         maxPhip = maxPhi%v
c set up for plain execution
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
         call global_max_r8( maxPhip,myThid )
c reset the mode
         our_rev_mode=our_orig_mode
c copy back
         maxPhi%v = maxPhip
      end if
      if (our_rev_mode%adjoint) then
#ifdef OAD_DEBUG_SPLIT2
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering adjoint'
#endif
         maxPhip = maxPhi%d
c set up for plain execution
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
         call global_admax_r8( maxPhip,myThid )
c reset the mode
         our_rev_mode=our_orig_mode
c copy back
         maxPhi%d = maxPhip
      end if

#ifdef OAD_DEBUG_SPLIT2
      write(standardmessageunit,'(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'leave __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 

      our_indent=our_indent-1
#endif

      end subroutine template
