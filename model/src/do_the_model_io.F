C $Header: /u/gcmpack/MITgcm/model/src/do_the_model_io.F,v 1.19 2001/03/06 16:51:02 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE DO_THE_MODEL_IO(
     I          forceOutput, myCurrentTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE DO_THE_MODEL_IO                               |
C     | o Controlling routine for IO in model main time-stepping |
C     |   loop.                                                  |
C     |==========================================================|
C     | Many systems do not have thread safe IO so it is easier  |
C     | to lump everything together and do dumping of fields     |
C     | and updating of forcing terms in a single place.         |
C     | The approach to IO used here is that writes are only     |
C     | performed by thread 1 and that a process only writes out |
C     | its data ( it does not know about anyone elses data!)    |
C     | Reading on the other hand is assumed to be from a file   |
C     | containing all the data for all the processes. Only the  |
C     | portion of data of interest to this process is actually  |
C     | loaded. To work well this assumes the existence of some  |
C     | reliable tool to join datasets together at the end of a  |
C     | run - see joinds.p                                       |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     forceOutput - If .TRUE. then force I/O, indepenent of other tests
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      LOGICAL forceOutput
      INTEGER myThid
      INTEGER myIter
      _RL    myCurrentTime

C     == Local variables ==

C--   Generaly only thread 1 does IO here. It can not start until 
C--   all threads fields are ready.
      IF (
     &  DIFFERENT_MULTIPLE(dumpFreq,myCurrentTime,
     &                     myCurrentTime-deltaTClock)
     & ) THEN

       _BARRIER

C--    Write "text-plots" of certain fields
       CALL PLOT_FIELD_XYZRL( uVel , 'Current uVel  ' , 
     &                        Nr, myIter, myThid )
       CALL PLOT_FIELD_XYZRL( vVel , 'Current vVel  ' , 
     &                        Nr, myIter, myThid )
       CALL PLOT_FIELD_XYZRL( theta, 'Current theta ' , 
     &                        Nr, myIter, myThid )
       CALL PLOT_FIELD_XYRL( etaN  , 'Current etaN  ' , 
     &                        myIter, myThid )

      ENDIF

C--   Write model state to binary file
      CALL WRITE_STATE( forceOutput, myCurrentTime, myIter, myThid )

#ifdef ALLOW_TIMEAVE
C--   Do time averages
      IF (taveFreq.GT.0.) THEN
       CALL TIMEAVE_STATV_WRITE( myCurrentTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_GMREDI
C--   Do KPP diagnostics.
      IF (useGMRedi)
     & CALL GMREDI_DIAGS( myCurrentTime, myIter, myThid )
#endif

#ifdef ALLOW_KPP
C--   Do KPP diagnostics.
      IF (useKPP)
     & CALL KPP_DO_DIAGS( myCurrentTime, myIter, myThid )
#endif

      RETURN
      END
 
