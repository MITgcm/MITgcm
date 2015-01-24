C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_apply_flux_ctrl.F,v 1.2 2015/01/24 15:34:32 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_APPLY_FLUX_CTRL ( myThid )

C     /============================================================\
C     | SUBROUTINE                                                 |   
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
!#ifdef ALLOW_STREAMICE_FLUX_CONTROL
!#include "STREAMICE_CTRL_FLUX.h"
!#endif

      INTEGER myThid

#ifdef ALLOW_STREAMICE
#ifdef ALLOW_STREAMICE_FLUX_CONTROL
#ifdef STREAMICE_FALSE
      INTEGER bi, bj, i, j, k
      _RS maskval
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL streamice_flux_scale_current (n_fluxes_max)


!!!!!!!  IN PLACE OF THIS, THERE WOULD BE CALL TO SOMETHING
!!!!!!   LIKE CTRL_GENTIME_2D IN ORDER TO PROPERLY INTERPOLATE
!!!!!!   THE SCALINGS TO THE CURRENT TIME STEP

      DO i=1,n_fluxes
       streamice_flux_scale_current (i) =
     &  streamice_ctrl_flux_scale (i,1)
      ENDDO

!!!!!!!!
!!!!!!!
!!!!!!!!

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF (streamice_umask(i,j,bi,bj).eq.3.0 .OR.
     &        streamice_vmask(i,j,bi,bj).eq.3.0) THEN

           maskval = INT(streamice_ctrl_flux_mask(i,j,bi,bj))

           DO k=1,n_fluxes_max
            IF (maskval.eq.streamice_ctrl_flux_id(k)) THEN
             IF (streamice_umask(i,j,bi,bj).eq.3.0
              u_bdry_values_SI (i,j,bi,bj) = 
     &         u_bdry_values_SI_base(i,j,bi,bj) *
     &         streamice_flux_scale_current (k)
             ENDIF
             IF (streamice_vmask(i,j,bi,bj).eq.3.0
              v_bdry_values_SI (i,j,bi,bj) = 
     &         v_bdry_values_SI_base(i,j,bi,bj) *
     &         streamice_flux_scale_current (k)
             ENDIF
             EXIT
            ENDIF
           ENDDO 
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif
#endif
#endif
      RETURN
      END
