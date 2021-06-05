#include "THSICE_OPTIONS.h"
c#ifdef ALLOW_AUTODIFF
c# include "AUTODIFF_OPTIONS.h"
c#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif

      SUBROUTINE THSICE_COST_TEST( myTime, myIter, myThid )

c     ==================================================================
c     SUBROUTINE thsice_cost_test
c     ==================================================================
c
c     o Compute sea-ice cost function.  The following options can be
c       selected with data.thsice (THSICE_PARM02) variable thsice_cost_ice_flag
c
c     thsice_cost_ice_flag = 1
c     - compute mean sea-ice volume
c       costIceStart < myTime < costIceEnd
c
c     thsice_cost_ice_flag = 2
c     - compute mean sea-ice area
c       costIceStart < myTime < costIceEnd
c
c     thsice_cost_ice_flag = 3
c     - heat content of top level plus latent heat of sea-ice
c       costIceStart < myTime < costIceEnd
c
c     thsice_cost_ice_flag = 4
c     - heat content of top level
c       costIceStart < myTime < costIceEnd
c
c     thsice_cost_ice_flag = 5
c     - heat content of top level plus sea-ice plus latent heat of snow
c       costIceStart < myTime < costIceEnd
c
c     thsice_cost_ice_flag = 6
c     - quadratic cost function measuring difference between pkg/thsice
c       AREA variable and simulated sea-ice measurements at every time
c       step.
c
c     ==================================================================
c     SUBROUTINE thsice_cost_test
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#ifdef ALLOW_THSICE_COST_TEST
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_COST.h"
#include "THSICE_VARS.h"
c#include "DYNVARS.h"
#include "cost.h"
#endif /* ALLOW_THSICE_COST_TEST */

c     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_THSICE_COST_TEST
c     == external functions ==
c     INTEGER  ILNBLNK
c     EXTERNAL ILNBLNK

c     == local variables ==
c     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj, i, j
      _RL tempVar

c     == end of interface ==

      IF ( myTime .GT. (endTime - lastinterval) ) THEN
         tempVar = 1. _d 0/
     &             ( ( 1. _d 0 + MIN(endTime-startTime,lastinterval) )
     &             / deltaTClock )

         IF ( thsice_cost_ice_flag .EQ. 1 ) THEN
c     sea-ice volume
            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j = 1,sNy
                     DO i =  1,sNx
                        objf_thsice(bi,bj) = objf_thsice(bi,bj) +
     &                       tempVar*rA(i,j,bi,bj)*iceHeight(i,j,bi,bj)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF ( thsice_cost_ice_flag .EQ. 2 ) THEN
c     sea-ice area
            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j = 1,sNy
                     DO i =  1,sNx
                        objf_thsice(bi,bj) = objf_thsice(bi,bj) +
     &                       tempVar*rA(i,j,bi,bj)*iceMask(i,j,bi,bj)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO

         ELSE
            WRITE(msgBuf,'(A)')
     &           'THSICE_COST_TEST: invalid thsice_cost_ice_flag'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R THSICE_COST_TEST'
         ENDIF
      ENDIF

#endif /* ALLOW_THSICE_COST_TEST */

      RETURN
      END
