#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_GET_VEL_RESID_ERR_OAD ( err_max, myThid )
C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"

C     !INPUT/OUTPUT ARGUMENTS
      _RL err_max
      INTEGER myThid

#ifdef ALLOW_STREAMICE
#if (defined (ALLOW_STREAMICE_OAD_FP))

      INTEGER conv_flag, i, j, bi, bj, myIter
      Real*8 u_dummy (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 v_dummy (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL err_sum
      _RL err_sum_tile (nSx,nSy)

      INTEGER ikey_nl
      _RL err_tempu, err_tempv
      _RL max_vel, tempu, tempv, err_lastchange, cgtol
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL isTape

       err_max = 0. _d 0
       err_sum = 0. _d 0

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         err_sum_tile(bi,bj) = 0. _d 0
        ENDDO
       ENDDO

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            Au_SI (i,j,bi,bj) = 0. _d 0
            Av_SI (i,j,bi,bj) = 0. _d 0
            u_dummy (i,j,bi,bj) = u_streamice(i,j,bi,bj)
            v_dummy (i,j,bi,bj) = v_streamice(i,j,bi,bj)
            ubd_SI (i,j,bi,bj) = 0. _d 0
            vbd_SI (i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL STREAMICE_CG_BOUND_VALS( myThid,
     O    ubd_SI,
     O    vbd_SI)

        CALL STREAMICE_CG_ACTION( myThid,
     O    Au_SI,
     O    Av_SI,
     I    U_dummy,
     I    V_dummy,
     I    0, sNx+1, 0, sNy+1 )

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            err_tempu = 0. _d 0
            err_tempv = 0. _d 0
            IF (STREAMICE_umask(i,j,bi,bj).eq.1) THEN
             err_tempu =
     &        ABS (Au_SI(i,j,bi,bj)+0*ubd_SI(i,j,bi,bj) -
     &            taudx_SI(i,j,bi,bj))
            ENDIF
            IF (STREAMICE_vmask(i,j,bi,bj).eq.1) THEN
             err_tempv = MAX( err_tempu,
     &        ABS (Av_SI(i,j,bi,bj)+0*vbd_SI(i,j,bi,bj) -
     &            taudy_SI(i,j,bi,bj)))
            ENDIF
            IF (err_tempv .ge. err_max) THEN
             err_max = err_tempv
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL GLOBAL_MAX_R8 (err_max, myThid)

#endif
#endif
      RETURN
      END
