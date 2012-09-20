#include "CPP_EEOPTIONS.h"
      subroutine template()
      use OAD_tape
      use OAD_rev

!$TEMPLATE_PRAGMA_DECLARATIONS

      type(modeType) :: our_orig_mode

c lovcal vars:
      INTEGER , PARAMETER :: nArgsHelper=9 
      INTEGER argsHelper(nArgsHelper)
      _RS     array_p( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )

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
c copy the values
         array_p = array%v
c keep the mode
         our_orig_mode=our_rev_mode
c set up for plain execution
         call OAD_revPlain()
c do it
         call EXCH1_RS( 
     U                 array_p,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
c reset the mode
         our_rev_mode=our_orig_mode
c copy back
         array%v = array_p
      end if
      if (our_rev_mode%tape) then
#ifdef OAD_DEBUG_SPLIT1
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering tape'
#endif
c copy the values
         array_p = array%v
c copy the args in case they are overwritte
         argsHelper=(/myOLw, myOLe, myOLs, myOLn, myNz,exchWidthX, 
     +exchWidthY,cornerMode, myThid/)
c keep the mode
         our_orig_mode=our_rev_mode
c set up for plain execution
         call OAD_revPlain()
c do it
         call EXCH1_RS(
     U                 array_p,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
c reset the mode
         our_rev_mode=our_orig_mode
c copy back
         array%v = array_p
c store the args:
         if(oad_it_sz.lt. oad_it_ptr+nArgsHelper) call oad_it_grow()
         oad_it(oad_it_ptr:oad_it_ptr+nArgsHelper-1)=argsHelper 
         oad_it_ptr=oad_it_ptr+nArgsHelper
      end if
      if (our_rev_mode%adjoint) then
c restore the args:
      oad_it_ptr=oad_it_ptr-nArgsHelper
      argsHelper=oad_it(oad_it_ptr:oad_it_ptr+nArgsHelper-1)
#ifdef OAD_DEBUG_SPLIT1
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering adjoint'
#endif
c copy the adjoints
         array_p = array%d
c keep the mode
         our_orig_mode=our_rev_mode
c set up for plain execution
         call OAD_revPlain()
c call the manual adjoint
         call EXCH1_RS_AD(
     U                 array_p,
     I                 argsHelper(1),argsHelper(2),argsHelper(3),
     I                 argsHelper(4),argsHelper(5),argsHelper(6),
     I                 argsHelper(7),argsHelper(8),argsHelper(9) )
c reset the mode
         our_rev_mode=our_orig_mode
c copy back
         array%d = array_p
      end if

#ifdef OAD_DEBUG_SPLIT1
      write(standardmessageunit,'(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'leave __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 

      our_indent=our_indent-1
#endif

      end subroutine template
