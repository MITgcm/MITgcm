#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"
      SUBROUTINE ATM2D_WRITE_PICKUP( 
     I     modelEnd, 
     I     myTime, 
     I     myIter, 
     I     myThid )

C     *==========================================================*
C     | Write pickup files for atm2d package which needs it to   |
C     |restart. It writes both "rolling-checkpoint" files (ckptA,|
C     |ckptB) and permanent checkpoint files. NOT called from    |
C     |the usual MITGCM WRITE_PICKUP routine in forward step, as |
C     |NORM_OCN_FLUXES must be done before these fluxes are ready|
C     *==========================================================*

C     Note this routine was pilfered from the MITGCM code prior to 
C     JMC's changes in 8/06.

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
C     myThid :: Thread number for this instance of the routine.
C     myIter :: Iteration number
C     myTime :: Current time of simulation ( s )
      LOGICAL modelEnd    
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

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

        IF (tempCheckPoint)   !toggle was done prematurely...
     &       nCheckLev = MOD(nCheckLev, maxNoChkptLev)+1

        CALL ATM2D_WRITE_PICKUP_NOW( 
     &       permCheckPoint, myTime, myIter, myThid )

        IF (tempCheckPoint)   !note this works for A/B chpt only
     &       nCheckLev = MOD(nCheckLev, maxNoChkptLev)+1

      ENDIF

      RETURN
      END


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"
CBOP
C     !ROUTINE: ATM2D_WRITE_PICKUP_NOW

C     !INTERFACE:
      SUBROUTINE ATM2D_WRITE_PICKUP_NOW( 
     I     permCheckPoint, 
     I     myTime, 
     I     myIter, 
     I     myThid )

C     !DESCRIPTION:
C     Write pickup files for atm2d package which needs it to restart and
C     do it NOW.

C     !USES:
      IMPLICIT NONE
#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE_VARS.h"
#include "ATM2D_VARS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     permCheckPoint  :: Checkpoint is permanent
C     myThid :: Thread number for this instance of the routine.
C     myIter :: Iteration number
C     myTime :: Current time of simulation ( s )
      LOGICAL permCheckPoint
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C     == Local variables ==
C     oldPrc :: Temp. for holding I/O precision
C     fn     :: Temp. for building file name string.
C     lgf    :: Flag to indicate whether to use global file mode.
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec, i,j
      LOGICAL lgf
CEOP

C     Going to really do some IO. Make everyone except master thread wait.
C      _BARRIER
C     _BEGIN_MASTER( myThid )

      prec = precFloat64
      lgf = globalFile
      
C     Create suffix to pass on to package pickup routines
      IF ( permCheckPoint ) THEN
       WRITE(fn,'(A,I10.10)') 'pickup_atm2d.',myIter
      ELSE
       WRITE(fn,'(A,A)') 'pickup_atm2d.',checkPtSuff(nCheckLev)
      ENDIF

      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_slp, 
     &                   1,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_qnet, 
     &                   2,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_solarnet, 
     &                   3,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_fu, 
     &                   4,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_fv, 
     &                   5,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_precip, 
     &                   6,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_evap, 
     &                   7,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_runoff, 
     &                   8,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_wspeed, 
     &                   9,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_pCO2, 
     &                   10,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_sIceLoad, 
     &                   11,myIter,myThid)

      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,sHeating, 
     &                   12,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,flxCndBt, 
     &                   13,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,pass_prcAtm, 
     &                   14,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,snowPrc, 
     &                   15,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,icFrwAtm, 
     &                   16,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,icFlxSw, 
     &                   17,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,siceAlb, 
     &                   18,myIter,myThid)

C     _END_MASTER( myThid )
C      _BARRIER

      RETURN
      END

