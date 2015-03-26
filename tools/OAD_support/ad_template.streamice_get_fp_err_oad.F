#include "STREAMICE_OPTIONS.h"

      SUBROUTINE template()
      use OAD_cp
      use OAD_tape
      use OAD_rev

C
C     **** Global Variables & Derived Type Definitions ****
C

C
C     **** Parameters and Result ****
C
#if (defined (ALLOW_STREAMICE_OAD_FP)) 


      if (our_rev_mode%plain) then
       err_max = 0. _d 0

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           err_tempu = 0. _d 0
           err_tempv = 0. _d 0
           IF (STREAMICE_umask(i,j,bi,bj).eq.1) THEN
            err_tempu =
     &       ABS (U_streamice(i,j,bi,bj)%v-u_new_SI(i,j,bi,bj)%v)
           ENDIF
           IF (STREAMICE_vmask(i,j,bi,bj).eq.1) THEN
            err_tempv = MAX( err_tempu,
     &       ABS (V_streamice(i,j,bi,bj)%v-v_new_SI(i,j,bi,bj)%v))
           ENDIF
           IF (err_tempv .ge. err_max) err_max = err_tempv
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_MAX_R8 (err_max, myThid)
      end if
      

      if (our_rev_mode%tape) then
      
       err_max = 0. _d 0

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           err_tempu = 0. _d 0
           err_tempv = 0. _d 0
           IF (STREAMICE_umask(i,j,bi,bj).eq.1) THEN
            err_tempu =
     &       ABS (U_streamice(i,j,bi,bj)%v-u_new_SI(i,j,bi,bj)%v)
           ENDIF
           IF (STREAMICE_vmask(i,j,bi,bj).eq.1) THEN
            err_tempv = MAX( err_tempu,
     &       ABS (V_streamice(i,j,bi,bj)%v-v_new_SI(i,j,bi,bj)%v))
           ENDIF
           IF (err_tempv .ge. err_max) err_max = err_tempv
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_MAX_R8 (err_max, myThid)
     
      end if
      

      if (our_rev_mode%adjoint) then

       err_max = 0. _d 0

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           err_tempu = 0. _d 0
           err_tempv = 0. _d 0
           IF (STREAMICE_umask(i,j,bi,bj).eq.1) THEN
            err_tempu =
     &       ABS (U_streamice(i,j,bi,bj)%d-u_new_SI(i,j,bi,bj)%d)
           ENDIF
           IF (STREAMICE_vmask(i,j,bi,bj).eq.1) THEN
            err_tempv = MAX( err_tempu,
     &       ABS (V_streamice(i,j,bi,bj)%d-v_new_SI(i,j,bi,bj)%d))
           ENDIF
           IF (err_tempv .ge. err_max) err_max = err_tempv
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_MAX_R8 (err_max, myThid)

      end if
#endif
      end subroutine template
