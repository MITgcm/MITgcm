C $Header: /u/gcmpack/MITgcm/model/src/packages_write_pickup.F,v 1.22 2006/08/09 02:23:13 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PACKAGES_WRITE_PICKUP

C     !INTERFACE:
      SUBROUTINE PACKAGES_WRITE_PICKUP(
     I     modelEnd,
     I     myTime,
     I     myIter,
     I     myThid )

C     !DESCRIPTION:
C     Write pickup files for each package which needs it to restart.
C     This routine (S/R PACKAGES_WRITE_PICKUP) calls per-package
C     write-pickup (or checkpoint) routines.  It writes both
C     "rolling-checkpoint" files (ckptA,ckptB) and permanent checkpoint
C     files.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     modelEnd    :: Checkpoint call at end of model run.
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: Thread number for this instance of the routine.
      LOGICAL modelEnd
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     permCheckPoint :: Flag indicating whether a permanent checkpoint will
C                       be written.
C     tempCheckPoint :: Flag indicating if it is time to write a non-permanent
C                       checkpoint (that will be permanent if permCheckPoint=T)
C     oldPrc :: Temp. for holding I/O precision
C     fn     :: Temp. for building file name string.
C     lgf    :: Flag to indicate whether to use global file mode.
      LOGICAL permCheckPoint, tempCheckPoint
CEOP

      permCheckPoint = .FALSE.
      tempCheckPoint = .FALSE.
      permCheckPoint=
     &     DIFFERENT_MULTIPLE(pChkPtFreq,myTime,deltaTClock)
      tempCheckPoint=
     &     DIFFERENT_MULTIPLE( chkPtFreq,myTime,deltaTClock)

#ifdef ALLOW_CAL
      IF ( useCAL ) THEN
         CALL CAL_TIME2DUMP( pChkPtFreq, deltaTClock,
     U                       permCheckPoint,
     I                       myTime, myIter, myThid )
         CALL CAL_TIME2DUMP( chkPtFreq,  deltaTClock,
     U                       tempCheckPoint,
     I                       myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_CAL */

      IF (
     &     ( .NOT.modelEnd .AND. (permCheckPoint.OR.tempCheckPoint) )
     &     .OR.
     &     ( modelEnd .AND. .NOT.(permCheckPoint.OR.tempCheckPoint) )
     &     ) THEN

        CALL PACKAGES_WRITE_PICKUP_NOW(
     &       permCheckPoint, myTime, myIter, myThid )

      ENDIF

      RETURN
      END


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PACKAGES_WRITE_PICKUP_NOW

C     !INTERFACE:
      SUBROUTINE PACKAGES_WRITE_PICKUP_NOW(
     I     permCheckPoint,
     I     myTime,
     I     myIter,
     I     myThid )

C     !DESCRIPTION:
C     Write pickup files for each package which needs it to restart and
C     do it NOW.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     permCheckPoint  :: Checkpoint is permanent
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: Thread number for this instance of the routine.
      LOGICAL permCheckPoint
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C     == Local variables ==
C     oldPrc :: Temp. for holding I/O precision
C     fn     :: Temp. for building file name string.
C     lgf    :: Flag to indicate whether to use global file mode.
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      LOGICAL lgf
CEOP

C     Going to really do some IO. Make everyone except master thread wait.
      _BARRIER
C     _BEGIN_MASTER( myThid )

      prec = precFloat64
      lgf = globalFile

C     Create suffix to pass on to package pickup routines
      IF ( permCheckPoint ) THEN
        WRITE(fn,'(I10.10)') myIter
      ELSE
        WRITE(fn,'(A)') checkPtSuff(nCheckLev)
      ENDIF

#ifdef ALLOW_CD_CODE
      IF (useCDscheme) THEN
        CALL CD_CODE_WRITE_CHECKPOINT(
     &       prec, lgf, permCheckPoint, myIter, myThid)
      ENDIF
#endif /* ALLOW_CD_CODE */

#ifdef  ALLOW_OBCS
C     SPK 4/9/01: Open boundary checkpointing
      IF (useOBCS) THEN
        CALL OBCS_WRITE_CHECKPOINT(
     &       prec, lgf, permCheckPoint, myIter, myThid)
      ENDIF
#endif  /* ALLOW_OBCS */

#ifdef  ALLOW_SEAICE
      IF ( useSEAICE ) THEN
        CALL SEAICE_WRITE_PICKUP(
     &       prec, lgf, permCheckPoint, myIter, myThid)
      ENDIF
#endif  /* ALLOW_SEAICE */

#ifdef ALLOW_THSICE
      IF (useThSIce) THEN
        CALL THSICE_WRITE_PICKUP(
     &       prec, lgf, permCheckPoint, myIter, myThid)
      ENDIF
#endif /* ALLOW_THSICE */

#ifdef  COMPONENT_MODULE
      IF (useCoupler) THEN
        CALL CPL_WRITE_PICKUP(
     &       prec, lgf, permCheckPoint, myIter, myThid)
      ENDIF
#endif  /* COMPONENT_MODULE */

#ifdef ALLOW_FLT
C     Write restart file for floats
      IF (useFLT) THEN
        CALL FLT_RESTART(myTime, myIter, myThid)
      ENDIF
#endif

#ifdef ALLOW_LAND
C     Write pickup file for Land package:
      IF (useLand) THEN
        CALL LAND_WRITE_PICKUP(permCheckPoint,fn,
     &       myTime,myIter,myThid)
      ENDIF
#endif

#ifdef ALLOW_FIZHI
C     Write pickup file for fizhi package
      IF (usefizhi) THEN
        CALL FIZHI_WRITE_PICKUP(fn,myTime,myIter,myThid)
        CALL FIZHI_WRITE_VEGTILES(fn,0,myTime,myIter,myThid)
        CALL FIZHI_WRITE_DATETIME(myTime,myIter,myThid)
      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
C     Write pickup file for diagnostics package
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_WRITE_PICKUP(permCheckPoint,
     &       fn,myTime,myIter,myThid)
      ENDIF
#endif

#ifdef  ALLOW_GGL90
      IF ( useGGL90 ) THEN
        CALL GGL90_WRITE_CHECKPOINT(
     &       prec, lgf, permCheckPoint, myIter, myThid)
      ENDIF
#endif  /* ALLOW_GGL90 */

#ifdef ALLOW_PTRACERS
C     Write restart file for passive tracers
      IF (usePTRACERS) THEN
        CALL PTRACERS_WRITE_CHECKPOINT(permCheckPoint,
     &       fn,myIter,myTime,myThid)
      ENDIF
#endif /* ALLOW_PTRACERS */

C     _END_MASTER( myThid )
      _BARRIER

#ifdef ALLOW_OFFLINE
C     This is quick fix for A/B checkpoints since the main model
C     checkpoint routine will not be called in OFFLINE mode and will
C     thus not have the chance to set the alternating A/B suffix
      IF ( .NOT. permCheckPoint ) THEN
        _BEGIN_MASTER( myThid )
        nCheckLev = MOD(nCheckLev, maxNoChkptLev)+1
        _END_MASTER( myThid )
      ENDIF
#endif /* ALLOW_OFFLINE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
