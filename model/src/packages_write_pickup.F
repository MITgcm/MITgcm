C $Header: /u/gcmpack/MITgcm/model/src/packages_write_pickup.F,v 1.1 2003/12/14 23:18:49 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE PACKAGES_WRITE_PICKUP( 
     I                    modelEnd, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_WRITE_PICKUP                               
C     | o write pickup files for each package which needs it
C     |   to restart.
C     *==========================================================*
C     | This routine (S/R PACKAGES_WRITE_PICKUP) calls 
C     | per-package write-pickup (or checkpoint) routines.
C     | o writes both "rolling-checkpoint" files (ckptA,ckptB)      
C     |   and permanent checkpoint files.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
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
C     myThid :: Thread number for this instance of the routine.
C     myIter :: Iteration number
C     myTime :: Current time of simulation ( s )
      LOGICAL modelEnd    
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

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
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER prec
      LOGICAL lgf
CEOP

      permCheckPoint = .FALSE.
      tempCheckPoint = .FALSE.
      permCheckPoint=
     &  DIFFERENT_MULTIPLE(pChkptFreq,myTime,myTime-deltaTClock)
      tempCheckPoint=
     &  DIFFERENT_MULTIPLE( ChkptFreq,myTime,myTime-deltaTClock)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF (
     &    ( .NOT.modelEnd .AND. (permCheckPoint.OR.tempCheckPoint) )
     &   .OR.
     &    ( modelEnd .AND. .NOT.(permCheckPoint.OR.tempCheckPoint) )
     &   ) THEN

C--    Going to really do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

        prec = precFloat64
        lgf = globalFile

C Create suffix to pass on to package pickup routines
         IF ( permCheckPoint ) THEN
          WRITE(fn,'(I10.10)') myIter
         ELSE
          WRITE(fn,'(A)') checkPtSuff(nCheckLev)
         ENDIF

#ifdef ALLOW_CD_CODE
        IF (useCDscheme) THEN
          CALL CD_CODE_WRITE_CHECKPOINT(
     &               prec, lgf, permCheckPoint, myIter, myThid)
        ENDIF
#endif /* ALLOW_CD_CODE */

#ifdef  ALLOW_OBCS
C SPK 4/9/01: Open boundary checkpointing
        IF (useOBCS) THEN
          CALL OBCS_WRITE_CHECKPOINT(
     &               prec, lgf, permCheckPoint, myIter, myThid)
        ENDIF
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_THSICE
        IF (useThSIce) THEN
          CALL THSICE_WRITE_CHECKPOINT(
     &               prec, lgf, permCheckPoint, myIter, myThid)
        ENDIF
#endif /* ALLOW_THSICE */

#ifdef ALLOW_FLT
C--     Write restart file for floats
        IF (useFLT) THEN
          CALL FLT_RESTART(myTime, myIter, myThid)
        ENDIF
#endif

#ifdef ALLOW_LAND
C--     Write pickup file for Lnad package:
        IF (useLand) THEN
          CALL LAND_WRITE_PICKUP(fn,myTime,myIter,myThid)
        ENDIF
#endif

       _END_MASTER( myThid )
       _BARRIER

#ifdef ALLOW_PTRACERS
C Write restart file for passive tracers
       IF (usePTRACERS) THEN
         CALL PTRACERS_WRITE_CHECKPOINT(fn,myIter,myTime,myThid)
       ENDIF
#endif /* ALLOW_PTRACERS */

      ENDIF

      RETURN
      END
