#include "STREAMICE_OPTIONS.h"

      SUBROUTINE template()
      use OAD_cp
      use OAD_tape
      use OAD_rev

     
C
C     **** Statements ****
C

      !logical isTape 
      !type(modeType) :: our_orig_mode

!      if (our_rev_mode%plain .or. our_rev_mode%tape) then
      if (our_rev_mode%plain) then
 
!       if (our_rev_mode%tape) then
!        isTape=.true.
!        our_rev_mode%plain=.true.
!        our_rev_mode%tape=.false.
!        our_rev_mode%adjoint=.false.
!      endif
            

      err_max = 0. _d 0

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            Au_SI (i,j,bi,bj) = 0. _d 0
            Av_SI (i,j,bi,bj) = 0. _d 0
            ubd_SI (i,j,bi,bj)%v = 0. _d 0
            vbd_SI (i,j,bi,bj)%v = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL openad_STREAMICE_CG_BOUND_VALS( myThid,
     O    ubd_SI,
     O    vbd_SI)

        CALL openad_STREAMICE_CG_ACTION( myThid,
     O    Au_SI,
     O    Av_SI,
     I    U_streamice,
     I    V_streamice,
     I    0, sNx+1, 0, sNy+1 )


        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            err_tempu = 0. _d 0
            err_tempv = 0. _d 0
            IF (STREAMICE_umask(i,j,bi,bj).eq.1) THEN
             err_tempu =
     &        ABS (Au_SI(i,j,bi,bj)+0*ubd_SI(i,j,bi,bj)%v -
     &            taudx_SI(i,j,bi,bj)%v)
            ENDIF
            IF (STREAMICE_vmask(i,j,bi,bj).eq.1) THEN
             err_tempv = MAX( err_tempu,
     &        ABS (Av_SI(i,j,bi,bj)+0*vbd_SI(i,j,bi,bj)%v -
     &            taudy_SI(i,j,bi,bj)%v))
            ENDIF
            IF (err_tempv .ge. err_max) THEN
             err_max = err_tempv
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL GLOBAL_MAX_R8 (err_max, myThid)

!       if (isTape) then
!        our_rev_mode%plain=.false.
!        our_rev_mode%tape=.true.
!        our_rev_mode%adjoint=.false.
!       endif

      end if



      end subroutine template
