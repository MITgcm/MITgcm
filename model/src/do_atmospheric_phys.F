#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

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

C     !CALLING SEQUENCE:
C     DO_ATMOSPHERIC_PHYS
C       |
C       |-- UPDATE_OCEAN_EXPORTS
C       |-- UPDATE_EARTH_EXPORTS
C       |-- UPDATE_CHEMISTRY_EXPORTS
C       |-- FIZHI_WRAPPER
C       |-- STEP_FIZHI_FG
C       |-- FIZHI_UPDATE_TIME
C       |
C       |-- ATM_PHYS_DRIVER
C       |
C       |-- AIM_DO_PHYSICS

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables
C     bi, bj   :: tile indices
C     i,j,k    :: loop indices
      INTEGER bi, bj
      INTEGER i, j, k
      _RL thetaRef

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('DO_ATMOSPHERIC_PHYS',myThid)
#endif

      IF ( fluidIsAir ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE salt  = comlev1, key=ikey_dynamics, kind=isbyte
#endif
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Compute virtual potential temperature anomaly (including water vapour
C     effect), stored in rhoInSitu (similar to what is done for oceanic EOS)
          DO k=1,Nr
           IF ( select_rStar.GE.1 .OR. selectSigmaCoord.GE.1 ) THEN
C-    isothermal (theta=const) reference state
             thetaRef = thetaConst
           ELSE
C-    horizontally uniform (tRef) reference state
             thetaRef = tRef(k)
           ENDIF
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             rhoInSitu(i,j,k,bi,bj) =
     &         ( theta(i,j,k,bi,bj)
     &              *( salt(i,j,k,bi,bj)*atm_Rq + oneRL )
     &         - thetaRef )*maskC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO

        ENDDO
       ENDDO
#ifdef ALLOW_AUTODIFF
      ELSE
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              rhoInSitu(i,j,k,bi,bj) = 0.
            ENDDO
           ENDDO
          ENDDO
        ENDDO
       ENDDO
#endif /* ALLOW_AUTODIFF */
      ENDIF

#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) THEN
        CALL TIMER_START('FIZHI          [DO_ATMOSPHERIC_PHYS]',myThid)
        CALL UPDATE_OCEAN_EXPORTS ( myTime, myIter, myThid )
        CALL UPDATE_EARTH_EXPORTS ( myTime, myIter, myThid )
        CALL UPDATE_CHEMISTRY_EXPORTS ( myTime, myIter, myThid )
        CALL FIZHI_WRAPPER ( myTime, myIter, myThid )
        CALL STEP_FIZHI_FG ( myTime, myIter, myThid, dTtracerLev(1) )
        CALL FIZHI_UPDATE_TIME ( myIter, myThid, deltaTClock )
        CALL TIMER_STOP ('FIZHI          [DO_ATMOSPHERIC_PHYS]',myThid)
      ENDIF
#endif /* ALLOW_FIZHI */

#ifdef ALLOW_ATM_PHYS
C     Atmospheric Physics package - Atm_Phys - main driver
      IF ( useAtm_Phys ) THEN
        CALL TIMER_START('ATM_PHYS_DRIVER [DO_ATMOSPHERIC_PHYS]',myThid)
        CALL ATM_PHYS_DRIVER( myTime, myIter, myThid )
        CALL TIMER_STOP( 'ATM_PHYS_DRIVER [DO_ATMOSPHERIC_PHYS]',myThid)
      ENDIF
#endif /* ALLOW_ATM_PHYS */

#ifdef ALLOW_AIM
      IF ( useAIM ) THEN
C       AIM - atmospheric intermediate model, physics package code.
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('AIM_DO_PHYSICS',myThid)
#endif
        CALL TIMER_START('AIM_DO_PHYSICS [DO_ATMOSPHERIC_PHYS]',myThid)
        CALL AIM_DO_PHYSICS( myTime, myIter, myThid )
        CALL TIMER_STOP( 'AIM_DO_PHYSICS [DO_ATMOSPHERIC_PHYS]',myThid)
      ENDIF
#endif /* ALLOW_AIM */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('DO_ATMOSPHERIC_PHYS',myThid)
#endif

      RETURN
      END
