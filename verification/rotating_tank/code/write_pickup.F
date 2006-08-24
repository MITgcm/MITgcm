C $Header: /u/gcmpack/MITgcm/verification/rotating_tank/code/Attic/write_pickup.F,v 1.1 2006/08/24 19:00:20 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE WRITE_PICKUP ( permCheckPoint, myTime, 
     &                              myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE WRITE_PICKUP                               
C     | o Controlling routine for IO to write restart file.       
C     *==========================================================*
C     | Write model checkpoint files for use in restart.          
C     | This routine writes both "rolling-checkpoint" files       
C     | and permanent checkpoint files. A rolling checkpoint      
C     | works through a circular list of suffices. Generally the  
C     | circular list has two entries so that a rolling           
C     | checkpoint will overwrite the last rolling checkpoint     
C     | but one. This is useful for running long jobs without     
C     | filling too much disk space.                              
C     |  In a permanent checkpoint data is written suffixed by    
C     | the current timestep number. This sort of checkpoint can  
C     | be used to provided a snap-shot from which the model      
C     | can be rerun.                                             
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     permCheckPoint :: Flag indicating whether a permanent checkpoint will
C                       be written.
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
      INTEGER prec
      LOGICAL lgf
CEOP

      permCheckPoint = .FALSE.
      permCheckPoint=
     &     DIFFERENT_MULTIPLE(pChkptFreq,myTime,deltaTClock)

C--    Going to really do some IO. Make everyone except master thread wait.
       _BARRIER

C--     Write model fields
        IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup.',myIter
        ELSE
         WRITE(fn,'(A,A)') 'pickup.',checkPtSuff(nCheckLev)
        ENDIF
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,uVel,  1,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gU,    2,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gUnm1, 3,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,vVel,  4,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gV,    5,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gVnm1, 6,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,wVel, 7,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,theta, 8,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gT,    9,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gTnm1, 10,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,salt, 11,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gS,   12,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gSnm1,13,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,etaN,13*Nr+1,
     &                     myIter,myThid)
#ifdef NONLIN_FRSURF
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,etaH,13*Nr+2,
     &                     myIter,myThid)
#endif

#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         IF ( permCheckPoint ) THEN
          WRITE(fn,'(A,I10.10)') 'pickup_nh.',myIter
         ELSE
          WRITE(fn,'(A,A)') 'pickup_nh.',checkPtSuff(nCheckLev)
         ENDIF
         WRITE(fn,'(A,I10.10)') 'pickup_nh.',myIter
         CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,phi_nh,1,myIter,myThid)
         CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gW,   2,myIter,myThid)
c        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gWnm1,3,myIter,myThid)
        ENDIF
#endif
       _BARRIER

      RETURN
      END
