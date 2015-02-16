C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_get_vel_resid_err.F,v 1.3 2015/02/16 16:46:45 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_GET_VEL_RESID_ERR ( err_max, myThid )
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
      INTEGER myThid, myiter

#ifdef ALLOW_STREAMICE

      INTEGER conv_flag, i, j, bi, bj

      INTEGER ikey_nl
      _RL err_tempu, err_tempv
      _RL max_vel, tempu, tempv, err_lastchange, cgtol
      CHARACTER*(MAX_LEN_MBUF) msgBuf

!#ifdef ALLOW_AUTODIFF_TAMC
!!$TAF STORE U_streamice = comlev1_stream_nl, key=ikey_nl
!!$TAF STORE V_streamice = comlev1_stream_nl, key=ikey_nl
!#endif

        err_max = 0. _d 0

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            Au_SI (i,j,bi,bj) = 0. _d 0
            Av_SI (i,j,bi,bj) = 0. _d 0
            ubd_SI (i,j,bi,bj) = 0. _d 0
            vbd_SI (i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL STREAMICE_CG_BOUND_VALS( myThid,
     O    ubd_SI,
     O    vbd_SI)

!#ifdef ALLOW_AUTODIFF_TAMC
!!$TAF STORE U_streamice = comlev1_stream_nl, key=ikey_nl
!!$TAF STORE V_streamice = comlev1_stream_nl, key=ikey_nl
!#endif

        CALL STREAMICE_CG_ACTION( myThid,
     O    Au_SI,
     O    Av_SI,
     I    U_streamice,
     I    V_streamice,
     I    0, sNx+1, 0, sNy+1 )

!#ifdef ALLOW_AUTODIFF_TAMC
!!$TAF STORE U_streamice = comlev1_stream_nl, key=ikey_nl
!!$TAF STORE V_streamice = comlev1_stream_nl, key=ikey_nl
!#endif

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
!#ifdef ALLOW_AUTODIFF_TAMC
!!$TAF STORE err_max = comlev1_stream_nl, key=ikey_dynamics
!#endif

#endif
      RETURN
      END
