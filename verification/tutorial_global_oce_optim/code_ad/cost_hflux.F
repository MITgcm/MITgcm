#include "COST_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: COST_HFLUX
C     !INTERFACE:
      SUBROUTINE COST_HFLUX( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE COST_HFLUX
C     | o the subroutine computes the cost function relative to
C     |   mean surface hflux optimization as a simple example.
C     *==========================================================*
C     \ev
C     !USES:
       IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "cost.h"
#ifdef ALLOW_CTRL
# include "ctrl_weights.h"
# include "CTRL_SIZE.h"
# include "CTRL_GENARR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid

#ifdef ALLOW_COST_HFLUXM
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i, j
      INTEGER bi, bj
      INTEGER iarr
      _RL     locfc,tmpC
CEOP

C     iarr = 1 is set in data.ctrl (xx_gentim2d_file(1) = 'xx_qnet').
      iarr = 1
C     By default xx_gentim2d_period(1)=0, so that control parameter
C     xx_gentim2d(:,:,:,:,1) is the mean heat flux perturbation. Here we
C     penalize deviations from the first guess = 0.
      tmpC = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          tmpC = tmpC + maskC(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _GLOBAL_SUM_RL( tmpC , myThid )
      IF ( tmpC.GT.0. ) tmpC = 1. _d 0 / tmpC

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        locfc = 0. _d 0
        DO j=1,sNy
         DO i=1,sNx
          locfc = locfc + tmpC*maskC(i,j,1,bi,bj)*
     &        whfluxm(i,j,bi,bj)*
     &       (
     &        xx_gentim2d(i,j,bi,bj,iarr)
     &       )**2
         ENDDO
        ENDDO

        objf_hflux_tut(bi,bj) = locfc
c       print*,'objf_hflux_tut =',locfc

       ENDDO
      ENDDO

#endif /* ALLOW_COST_HFLUXM */
      RETURN
      END
