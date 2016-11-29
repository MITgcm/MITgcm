#include "STREAMICE_OPTIONS.h"

      SUBROUTINE template()
      use OAD_cp
      use OAD_tape
      use OAD_rev


!      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
!     +_3,cp_loop_variable_4


      if (our_rev_mode%plain) then
      CALL OpenAD_exch_xy_rl (X, mythid)
      end if
      if (our_rev_mode%tape .and..not.our_rev_mode%strictAnonymous) then
      CALL OpenAD_exch_xy_rl (X, mythid)
      end if
      if (our_rev_mode%tape .and. our_rev_mode%strictAnonymous) then
      CALL OpenAD_exch_xy_rl (X, mythid)
      end if
      if (our_rev_mode%adjoint) then

      CALL OpenAD_exch_xy_rl (X, mythid)

      

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=0,sNy+1
         DO i=1,sNx
          q_int1(i,j,bi,bj) = .25 * X(i-1,j,bi,bj)%d + 
     &                        .50 * X(i,j,bi,bj)%d +
     &                        .25 * X(i+1,j,bi,bj)%d
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          q_int2(i,j,bi,bj) = .25 * q_int1(i,j+1,bi,bj) +  
     &                        .50 * q_int1(i,j,bi,bj) +
     &                        .25 * q_int1(i,j+1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XY_RL (q_int2, mythid)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          X(i,j,bi,bj)%d = .25 * q_int2(i,j+1,bi,bj) +  
     &                        .50 * q_int2(i,j,bi,bj) +
     &                        .25 * q_int2(i,j+1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      
      end if
      end subroutine template
