#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_init_fixed( mythid )

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

      implicit none

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
      integer mythid

#if (defined ALLOW_COST && defined ALLOW_COST_ICE)
C     == local variables ==
      integer tempDate1(4)
      integer tempDate2(4)
C     == end of interface ==

#ifdef ALLOW_CAL

      _BEGIN_MASTER( myThid )

      IF ( useCAL ) THEN
C--   Sea-ice cost function start and stop times in model seconds.
        call cal_FullDate  ( costIceStart1 , costIceStart2,
     &                       tempDate1     , mythid )
        call cal_TimePassed( modelStartDate, tempDate1    ,
     &                       tempDate2     , mythid )
        call cal_ToSeconds ( tempDate2 , costIceStart , mythid )
        costIceStart = costIceStart + startTime

        if ( costIceEnd1.EQ.0 ) then
C-      use "modelEndDate" as default value for costIceEnd
          costIceEnd1 = modelEndDate(1)
          costIceEnd2 = modelEndDate(2)
        endif
        call cal_FullDate  ( costIceEnd1   , costIceEnd2  ,
     &                       tempDate1     , mythid )
        call cal_TimePassed( modelStartDate, tempDate1    ,
     &                       tempDate2     , mythid )
        call cal_ToSeconds ( tempDate2 , costIceEnd , mythid )
        costIceEnd = costIceEnd + startTime
C--
      ENDIF
      _END_MASTER( mythid )

      _BARRIER

#endif /* ALLOW_CAL */
#endif /* ALLOW_COST and ALLOW_COST_ICE */

      RETURN
      END
