C $Header: /u/gcmpack/MITgcm/model/src/do_atmospheric_phys.F,v 1.5 2005/05/16 20:22:11 molod Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DO_ATMOSPHERIC_PHYS
C     !INTERFACE:
      SUBROUTINE DO_ATMOSPHERIC_PHYS(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_ATMOSPHERIC_PHYS                                 
C     | o Controlling routine for atmospheric physics and 
C     |   parameterization
C     *==========================================================*
C     | o originally, part of S/R thermodynamics & forward_step
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables
C     bi, bj
      INTEGER bi, bj

CEOP

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &   CALL DEBUG_ENTER('DO_ATMOSPHERIC_PHYS',myThid)
#endif
 
C AMM
#ifdef ALLOW_FIZHI
      if( useFIZHI) then
         CALL TIMER_START('FIZHI          [DO_ATMOSPHERIC_PHYS]',myThid)
         CALL UPDATE_OCEAN_EXPORTS ( myTime, myIter, myThid )
         CALL UPDATE_EARTH_EXPORTS ( myTime, myIter, myThid )
         CALL UPDATE_CHEMISTRY_EXPORTS ( myTime, myIter, myThid )
         CALL FIZHI_WRAPPER ( myTime, myIter, myThid )
         CALL STEP_FIZHI_FG ( myTime, myIter, myThid, dTtracerLev(1) )
         CALL TIMER_STOP ('FIZHI          [DO_ATMOSPHERIC_PHYS]',myThid)
      endif
#endif
C AMM

#ifdef ALLOW_AIM
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C       AIM - atmospheric intermediate model, physics package code.
        IF ( useAIM ) THEN
#ifdef ALLOW_DEBUG
         IF ( debugLevel .GE. debLevB ) 
     &     CALL DEBUG_CALL('AIM_DO_PHYSICS',myThid)
#endif
         CALL TIMER_START('AIM_DO_PHYSICS [DO_ATMOSPHERIC_PHYS]',myThid)
         CALL AIM_DO_PHYSICS( bi, bj, myTime, myIter, myThid )
         CALL TIMER_STOP( 'AIM_DO_PHYSICS [DO_ATMOSPHERIC_PHYS]',myThid)
        ENDIF

C--   end bi,bj loops.
       ENDDO
      ENDDO
#endif /* ALLOW_AIM */

#ifdef ALLOW_AIM
      IF ( useAIM ) THEN
        CALL AIM_AIM2DYN_EXCHANGES( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_AIM */

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &  CALL DEBUG_LEAVE('DO_ATMOSPHERIC_PHYS',myThid)
#endif

      RETURN
      END
