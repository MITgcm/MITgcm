#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_COST_INIT_FIXED( myThid )

C     ==================================================================
C     SUBROUTINE seaice_cost_init_fixed
C     ==================================================================
C
C     o Set contributions to the cost function and the cost function
C       itself to zero. The cost function and the individual contribu-
C       tions are defined in the header file "SEAICE_COST.h".
C
C     ==================================================================
C     SUBROUTINE seaice_cost_init_fixed
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_COST
# include "SEAICE_COST.h"
#endif
#ifdef ALLOW_CAL
# include "cal.h"
#endif

C     == routine arguments ==
      INTEGER myThid

#if (defined ALLOW_COST && defined ALLOW_COST_ICE)
#ifdef ALLOW_CAL
C     == local variables ==
      INTEGER tempDate1(4)
      INTEGER tempDate2(4)
C     == end of interface ==

      _BEGIN_MASTER( myThid )

      IF ( useCAL ) THEN
C--   Sea-ice cost function start and stop times in model seconds.
        CALL cal_FullDate  ( costIceStart1 , costIceStart2,
     &                       tempDate1     , myThid )
        CALL cal_TimePassed( modelStartDate, tempDate1    ,
     &                       tempDate2     , myThid )
        CALL cal_ToSeconds ( tempDate2 , costIceStart , myThid )
        costIceStart = costIceStart + startTime

        IF ( costIceEnd1.EQ.0 ) THEN
C-      use "modelEndDate" as default value for costIceEnd
          costIceEnd1 = modelEndDate(1)
          costIceEnd2 = modelEndDate(2)
        ENDIF
        CALL cal_FullDate  ( costIceEnd1   , costIceEnd2  ,
     &                       tempDate1     , myThid )
        CALL cal_TimePassed( modelStartDate, tempDate1    ,
     &                       tempDate2     , myThid )
        CALL cal_ToSeconds ( tempDate2 , costIceEnd , myThid )
        costIceEnd = costIceEnd + startTime
C--
      ENDIF
      _END_MASTER( myThid )

      _BARRIER

#endif /* ALLOW_CAL */
#endif /* ALLOW_COST and ALLOW_COST_ICE */

      RETURN
      END
