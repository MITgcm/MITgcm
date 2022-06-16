#include "COST_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE COST_HFLUX( myThid )
C     *==========================================================*
C     | SUBROUTINE COST_HFLUX
C     | o the subroutine computes the cost function relative to
C     |   surface hflux optimization
C     *==========================================================*

       IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "cost.h"
#include "ctrl_weights.h"
#include "FFIELDS.h"

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST_HFLUXM
C     ========= Local variables =========================
      integer i, j
      integer bi, bj
      _RL     locfc,tmpC

      tmpC = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         do j=1,sNy
           do i=1,sNx
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
     &         whfluxm(i,j,bi,bj)*
     &        (
     &         Qnetm(i,j,bi,bj)
     &        )**2
          ENDDO
         ENDDO

         objf_hflux_tut(bi,bj) = locfc
c        print*,'objf_hflux_tut =',locfc

       ENDDO
      ENDDO

#endif /* ALLOW_COST_HFLUXM */
      RETURN
      END
