#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_COST_TEST( myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE seaice_cost_test
C     ==================================================================
C
C     o Compute sea-ice cost function.  The following options can be
C       selected with data.seaice (SEAICE_PARM02) variable cost_ice_flag
C
C     cost_ice_flag = 1
C     - compute mean sea-ice volume
C       costIceStart < myTime < costIceEnd
C
C     cost_ice_flag = 2
C     - compute mean sea-ice area
C       costIceStart < myTime < costIceEnd
C
C     cost_ice_flag = 3
C     - heat content of top level plus latent heat of sea-ice
C       costIceStart < myTime < costIceEnd
C
C     cost_ice_flag = 4
C     - heat content of top level
C       costIceStart < myTime < costIceEnd
C
C     cost_ice_flag = 5
C     - heat content of top level plus sea-ice plus latent heat of snow
C       costIceStart < myTime < costIceEnd
C
C     cost_ice_flag = 6
C     - quadratic cost function measuring difference between pkg/seaice
C       AREA variable and simulated sea-ice measurements at every time
C       step.
C
C     ==================================================================
C
C     started: menemenlis@jpl.nasa.gov 26-Feb-2003
C
C     ==================================================================
C     SUBROUTINE seaice_cost_test
C     ==================================================================

      IMPLICIT NONE

#if (defined ALLOW_COST && defined ALLOW_COST_ICE)
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_COST.h"
#include "SEAICE.h"
#include "DYNVARS.h"
#include "cost.h"
#endif /* ALLOW_COST & ALLOW_COST_ICE */

C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if (defined ALLOW_COST && defined ALLOW_COST_ICE)
C     == local variables ==
C     msgBuf      - Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi,bj,i,j,kSrf
      _RL tempVar

C     == end of interface ==

      IF ( usingPCoords ) THEN
       kSrf = Nr
      ELSE
       kSrf = 1
      ENDIF
      IF ( myTime .GT. (endTime - lastinterval) ) THEN
         tempVar = 1. _d 0/
     &             ( ( 1. _d 0 + min(endTime-startTime,lastinterval) )
     &             / deltaTClock )

cph(
c     write(standardMessageUnit,*) 'ph-ice B ', myIter,
c    &        theta(4,4,kSrf,1,1), area(4,4,1,1), heff(4,4,1,1)
cph)
         IF ( cost_ice_flag .EQ. 1 ) THEN
C     sea-ice volume
            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j = 1,sNy
                     DO i =  1,sNx
                        objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                       tempVar * rA(i,j,bi,bj) * HEFF(i,j,bi,bj)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF ( cost_ice_flag .EQ. 2 ) THEN
C     sea-ice area
            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j = 1,sNy
                     DO i =  1,sNx
                        objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                       tempVar * rA(i,j,bi,bj) * AREA(i,j,bi,bj)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO

C     heat content of top level:
C     theta * delZ * (sea water heat capacity = 3996 J/kg/K)
C                  * (density of sea-water = 1026 kg/m^3)
C
C     heat content of sea-ice:
C     tice * heff * (sea ice heat capacity = 2090 J/kg/K)
C                 * (density of sea-ice = 910 kg/m^3)
C
C     note: to remove mass contribution to heat content,
C     which is not properly accounted for by volume converving
C     ocean model, theta and tice are referenced to freezing
C     temperature of sea-ice, here -1.96 deg C
C
C     latent heat content of sea-ice:
C     - heff * (latent heat of fusion = 334000 J/kg)
C            * (density of sea-ice = 910 kg/m^3)
C
C     latent heat content of snow:
C     - hsnow * (latent heat of fusion = 334000 J/kg)
C             * (density of snow = 330 kg/m^3)

         ELSEIF ( cost_ice_flag .EQ. 3 ) THEN
C     heat content of top level plus latent heat of sea-ice
            DO bj=myByLo(myThid),myByHi(myThid)
             DO bi=myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i =  1,sNx
                objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                 tempVar * rA(i,j,bi,bj) * (
     &                 (THETA(i,j,kSrf,bi,bj) + 1.96 _d 0 ) *
     &                 drF(kSrf) * 3996. _d 0 * 1026. _d 0 -
     &                 HEFF(i,j,bi,bj) * 334000. _d 0 * 910. _d 0 )
               ENDDO
              ENDDO
             ENDDO
            ENDDO

         ELSEIF ( cost_ice_flag .EQ. 4 ) THEN
C     heat content of top level
            DO bj=myByLo(myThid),myByHi(myThid)
             DO bi=myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i =  1,sNx
                objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                 tempVar * rA(i,j,bi,bj) * (
     &                 (THETA(i,j,kSrf,bi,bj) + 1.96 _d 0 ) *
     &                 drF(kSrf) * 3996. _d 0 * 1026. _d 0 )
               ENDDO
              ENDDO
             ENDDO
            ENDDO

         ELSEIF ( cost_ice_flag .EQ. 5 ) THEN
C     heat content of top level plus sea-ice plus latent heat of snow
            DO bj=myByLo(myThid),myByHi(myThid)
             DO bi=myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i =  1,sNx
                objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                 tempVar * rA(i,j,bi,bj) * (
     &                 (THETA(i,j,kSrf,bi,bj) + 1.96 _d 0 ) *
     &                 drF(kSrf) * 3996. _d 0 * 1026. _d 0 +
     &                 (TICES(i,j,1,bi,bj) - 273.15 _d 0 + 1.96 _d 0 ) *
     &                 HEFF(i,j,bi,bj) * 2090. _d 0 * 910. _d 0 -
     &                 HEFF(i,j,bi,bj) * 334000. _d 0 * 910. _d 0 -
     &                 HSNOW(i,j,bi,bj) * 334000. _d 0 * 330. _d 0 )
               ENDDO
              ENDDO
             ENDDO
            ENDDO

         ELSEIF ( cost_ice_flag .EQ. 6 ) THEN
C     Qadratic cost function measuring difference between pkg/seaice
C     AREA variable and simulated sea-ice measurements at every time
C     step.  For time being no measurements are read-in.  It is
C     assumed that measurements are AREA=0.5 at all times everywhere.
            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j = 1,sNy
                     DO i =  1,sNx
                        objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                       ( AREA(i,j,bi,bj) - 0.5 _d 0 ) *
     &                       ( AREA(i,j,bi,bj) - 0.5 _d 0 )
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF ( cost_ice_flag .EQ. 7 ) THEN

            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j = 1,sNy
                     DO i =  1,sNx
                        objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                       UICE(i,j,bi,bj) * UICE(i,j,bi,bj) +
     &                       VICE(i,j,bi,bj) * VICE(i,j,bi,bj)

                     ENDDO
                  ENDDO
               ENDDO
            ENDDO

         ELSE
            WRITE(msgBuf,'(A)')
     &           'SEAICE_COST_TEST: invalid cost_ice_flag'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R SEAICE_COST_TEST'
         ENDIF
      ENDIF

cph(
c     write(standardMessageUnit,*) 'ph-ice C ', myIter, objf_ice(1,1)
cph)

#endif /* ALLOW_COST & ALLOW_COST_ICE */

      RETURN
      END
