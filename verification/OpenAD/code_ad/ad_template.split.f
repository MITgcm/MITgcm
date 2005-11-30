      subroutine template()
      use OpenAD_tape
      use OpenAD_rev

!$TEMPLATE_PRAGMA_DECLARATIONS

      integer :: cp_loop_variable_1,cp_loop_variable_2,
     +     cp_loop_variable_3,cp_loop_variable_4

      integer iaddr
      external iaddr

c$$$      character*(80):: indentation='                                        
c$$$     +                                         '
c$$$      our_indent=our_indent+1
c$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
c$$$     +indentation(1:our_indent), "enter __SRNAME__:",
c$$$     +our_rev_mode%arg_store,
c$$$     +our_rev_mode%arg_restore,
c$$$     +our_rev_mode%res_store,
c$$$     +our_rev_mode%res_restore,
c$$$     +our_rev_mode%plain,
c$$$     +our_rev_mode%tape,
c$$$     +our_rev_mode%adjoint
c$$$
c$$$      write(*,'(A,A,A,I8,A,I8)')
c$$$     +     indentation(1:our_indent), "enter __SRNAME__:",
c$$$     +     " DT:",double_tape_pointer, 
c$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
c$$$         write(*,'(A,A)') 
c$$$     +indentation(1:our_indent), " __SRNAME__: entering plain"
!$PLACEHOLDER_PRAGMA$ id=1
      end if
      if (our_rev_mode%tape) then
c$$$         write(*,'(A,A)') 
c$$$     +indentation(1:our_indent), " __SRNAME__: entering tape"
!$PLACEHOLDER_PRAGMA$ id=2
      end if 
      if (our_rev_mode%adjoint) then
c$$$         write(*,'(A,A)') 
c$$$     +indentation(1:our_indent), " __SRNAME__: entering adjoint"
!$PLACEHOLDER_PRAGMA$ id=3
      end if 

c$$$      write(*,'(A,A,A,I8,A,I8)')
c$$$     +     indentation(1:our_indent), "leave __SRNAME__:",
c$$$     +     " DT:",double_tape_pointer, 
c$$$     +     " IT:",integer_tape_pointer
c$$$
c$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
c$$$     +indentation(1:our_indent), "leave __SRNAME__:",
c$$$     +our_rev_mode%arg_store,
c$$$     +our_rev_mode%arg_restore,
c$$$     +our_rev_mode%res_store,
c$$$     +our_rev_mode%res_restore,
c$$$     +our_rev_mode%plain,
c$$$     +our_rev_mode%tape,
c$$$     +our_rev_mode%adjoint
c$$$      our_indent=our_indent-1
      end subroutine template
