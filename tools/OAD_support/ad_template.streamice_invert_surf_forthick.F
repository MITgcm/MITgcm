#include "STREAMICE_OPTIONS.h"

      SUBROUTINE template()
      use OAD_cp
      use OAD_tape
      use OAD_rev

!$TEMPLATE_PRAGMA_DECLARATIONS

      integer :: cp_loop_variable_1,cp_loop_variable_2,
     +     cp_loop_variable_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr


      Real*8 H_p (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 surf_p (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 R_p (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 H_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 surf_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 R_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      
C
C     **** Statements ****
C


      if (our_rev_mode%plain) then
#ifdef STREAMICE_ALLOW_DEPTH_CONTROL
#ifdef STREAMICE_SMOOTH_FLOATATION

       H_p = H_streamice%v
       surf_p = surf_el_streamice%v
       R_p = R_low_si%v      

       CALL STREAMICE_INVERT_SURF_FORTHICK(
     O            H_p,
     I            surf_p,
     I            R_p,
     I            delta,
     I            myThid)

       H_streamice%v = H_p
#endif
#endif
      end if
      if (our_rev_mode%tape) then
#ifdef STREAMICE_SMOOTH_FLOATATION
#ifdef STREAMICE_ALLOW_DEPTH_CONTROL
c set up for plain execution
       our_orig_mode=our_rev_mode
       our_rev_mode%arg_store=.FALSE.
       our_rev_mode%arg_restore=.FALSE.
       our_rev_mode%plain=.TRUE.
       our_rev_mode%tape=.FALSE.
       our_rev_mode%adjoint=.FALSE.

       H_p = H_streamice%v
       surf_p = surf_el_streamice%v
       R_p = R_low_si%v

       call oad_tape_push(surf_p)
       call oad_tape_push(R_p)
       call oad_tape_push(delta)
       call oad_tape_push(myThid)

       CALL STREAMICE_INVERT_SURF_FORTHICK(
     O            H_p,
     I            surf_p,
     I            R_p,
     I            delta,
     I            myThid)

       call oad_tape_push(H_p)
       H_streamice%v = H_p
       our_rev_mode=our_orig_mode
#endif
#endif     
      end if
      if (our_rev_mode%adjoint) then
c set up for plain execution
#ifdef STREAMICE_ALLOW_DEPTH_CONTROL
#ifdef STREAMICE_SMOOTH_FLOATATION
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.

        call oad_tape_pop(H_p)
        call oad_tape_pop(myThid)
        call oad_tape_pop(delta)
        call oad_tape_pop(R_p)
        call oad_tape_pop(surf_p)

        H_d = H_streamice%d
        surf_d = surf_el_streamice%d
        R_d = R_low_si%d      

        CALL ADSTRMICE_H_INV (
     &            H_p,
     &            H_d,
     &            Surf_p,
     &            R_p,
     &            R_d,
     &            delta,
     &            mythid)

        R_low_si%d = R_d

       our_rev_mode=our_orig_mode
#endif      
#endif
      end if
      end subroutine template
