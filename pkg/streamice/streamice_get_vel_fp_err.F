C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_get_vel_fp_err.F,v 1.4 2015/02/16 16:46:45 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_GET_VEL_FP_ERR ( err_max, myThid )
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

      INTEGER conv_flag, i, j, bi, bj

      INTEGER ikey_nl
      _RL err_tempu, err_tempv
      _RL max_vel, tempu, tempv, err_lastchange, cgtol
      CHARACTER*(MAX_LEN_MBUF) msgBuf

       err_max = 0. _d 0

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           err_tempu = 0. _d 0
           err_tempv = 0. _d 0
           IF (STREAMICE_umask(i,j,bi,bj).eq.1) THEN
            err_tempu =
     &       ABS (U_streamice(i,j,bi,bj)-u_old_SI(i,j,bi,bj))
           ENDIF
           IF (STREAMICE_vmask(i,j,bi,bj).eq.1) THEN
            err_tempv = MAX( err_tempu,
     &       ABS (V_streamice(i,j,bi,bj)-v_old_SI(i,j,bi,bj)))
           ENDIF
           IF (err_tempv .ge. err_max) err_max = err_tempv
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_MAX_R8 (err_max, myThid)

#endif
      RETURN
      END
