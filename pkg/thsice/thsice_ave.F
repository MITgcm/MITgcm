#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_AVE
C     !INTERFACE:
      SUBROUTINE THSICE_AVE(
     I                       bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_AVE
C     | o save values for timeaveraging
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     frwAtm  :: net fresh-water flux (E-P) to the atmosphere [kg/m2/s]
C     flxAtm  :: net heat flux from the atmosphere (+=down) [W/m2]
C     bi,bj   :: tile indices
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     myThid  :: thread number for this instance of the routine.
c     _RL frwAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL flxAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     == Local variables ==
c     LOGICAL  DIFFERENT_MULTIPLE
c     EXTERNAL DIFFERENT_MULTIPLE
c     INTEGER wrtIter

c     IF ( DIFFERENT_MULTIPLE(thSIce_diagFreq,
c    &                        myTime+deltaTClock,deltaTClock)
c    &     .AND. fluidIsWater ) THEN
c       wrtIter = myIter+1
c       CALL WRITE_LOCAL_RL('ice_flxAtm','I10',1,flxAtm,
c    &                      bi,bj,1,wrtIter,myThid)
c       CALL WRITE_LOCAL_RL('ice_frwAtm','I10',1,frwAtm,
c    &                      bi,bj,1,wrtIter,myThid)
c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

C-- Atmospheric fluxes :
       IF ( fluidIsWater ) THEN
        CALL DIAGNOSTICS_FILL(icFlxAtm,'SIflxAtm',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(icFrwAtm,'SIfrwAtm',0,1,1,bi,bj,myThid)
       ENDIF

C-- Oceanic fluxes :
c #ifndef CHECK_ENERGY_CONSERV
       CALL DIAGNOSTICS_FILL_RS(Qnet,    'SIflx2oc',0,1,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL_RS(EmPmR,   'SIfrw2oc',0,1,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL_RS(saltFlux,'SIsaltFx',0,1,1,bi,bj,myThid)
c #endif /* CHECK_ENERGY_CONSERV */

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif  /* ALLOW_THSICE */

      RETURN
      END
