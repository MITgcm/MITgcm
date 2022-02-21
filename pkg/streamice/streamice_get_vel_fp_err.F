#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_GET_VEL_FP_ERR ( err_max,
#ifdef ALLOW_AUTODIFF_TAMC
     I     ikey_nl,
#endif
     I     myThid )
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
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER ikey_nl
#endif
      INTEGER myThid

#ifdef ALLOW_STREAMICE

      INTEGER i, j, bi, bj

      _RL err_tempu, err_tempv

      _RL err_sum
      _RL err_sum_tile (nSx,nSy)

       err_max = 0. _d 0
       err_sum = 0. _d 0

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         err_sum_tile(bi,bj) = 0. _d 0
        ENDDO
       ENDDO

       if (streamice_err_norm .lt. 1.0) then

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
#ifdef ALLOW_AUTODIFF_TAMC
!$TAF STORE err_max = comlev1_stream_nl, key=ikey_nl
#endif
           err_tempu = 0. _d 0
           err_tempv = 0. _d 0
           IF (STREAMICE_umask(i,j,bi,bj).eq.1) THEN
            err_tempu =
#ifdef ALLOW_STREAMICE_OAD_FP
     &       ABS (U_streamice(i,j,bi,bj)-u_new_SI(i,j,bi,bj))
#else
     &       ABS (U_streamice(i,j,bi,bj)-u_old_SI(i,j,bi,bj))
#endif
           ENDIF
           IF (STREAMICE_vmask(i,j,bi,bj).eq.1) THEN
            err_tempv = MAX( err_tempu,
#ifdef ALLOW_STREAMICE_OAD_FP
     &       ABS (V_streamice(i,j,bi,bj)-v_new_SI(i,j,bi,bj)))
#else
     &       ABS (V_streamice(i,j,bi,bj)-v_old_SI(i,j,bi,bj)))
#endif
           ENDIF
           IF (err_tempv .ge. err_max) err_max = err_tempv
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_MAX_R8 (err_max, myThid)

       ELSE

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           IF (STREAMICE_umask(i,j,bi,bj).eq.1) THEN
            err_sum_tile(bi,bj) = err_sum_tile(bi,bj) +
#ifdef ALLOW_STREAMICE_OAD_FP
     &       (ABS(U_streamice(i,j,bi,bj)-
     &              u_new_SI(i,j,bi,bj)))**streamice_err_norm
#else
     &       (ABS(U_streamice(i,j,bi,bj)-
     &               u_old_SI(i,j,bi,bj)))**streamice_err_norm
#endif
           ENDIF
           IF (STREAMICE_vmask(i,j,bi,bj).eq.1) THEN
            err_sum_tile(bi,bj) = err_sum_tile(bi,bj) +
#ifdef ALLOW_STREAMICE_OAD_FP
     &       (ABS(v_streamice(i,j,bi,bj)-
     &              v_new_SI(i,j,bi,bj)))**streamice_err_norm
#else
     &       (ABS(v_streamice(i,j,bi,bj)-
     &               v_old_SI(i,j,bi,bj)))**streamice_err_norm
#endif
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( err_sum_tile, err_sum, myThid )

       err_max = err_sum ** (1./streamice_err_norm)

       ENDIF

#endif
      RETURN
      END
