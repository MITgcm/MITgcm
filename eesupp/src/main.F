C $Header: /u/gcmpack/MITgcm/eesupp/src/main.F,v 1.7 2001/02/04 14:38:43 cnh Exp $
C $Name:  $

C--   Get C preprocessor options
#include "CPP_OPTIONS.h"
#include "CPP_EEOPTIONS.h"

      PROGRAM MAIN
C     /==========================================================\
C     | PROGRAM MAIN                                             |
C     | o MAIN wrapper for MITgcm UV implementation.             |
C     |==========================================================|
C     | MAIN controls the "execution environment".               |
C     | Its main functions are                                   |
C     | 1. call procedure EEBOOT to perform execution environment|
C     |    initialisation.                                       |
C     | 2. call procedure THE_MODEL_MAIN once for each concurrent|
C     |    thread. THE_MODEL_MAIN is the user supplied top-level |
C     |    routine.                                              |
C     | 3. call procedure EEDIE to perform execution environment |
C     |    shutdown.                                             |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
C     Include all the "shared" data here. That means all common
C     blocks used in the model. On many implementations this is not
C     necessary but doing this is the safest method.
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "THE_MODEL_COMMON_BLOCKS.h"

C--   Local variables
      INTEGER myThid
      INTEGER I

C--   Set up the execution environment
C     EEBOOT loads a execution environment parameter file 
C     ( called "eedata" by default ) and sets variables 
C     accordingly.
      CALL EEBOOT

C--   Trap errors 
      IF ( eeBootError ) THEN
       fatalError = .TRUE.
       GOTO 999
      ENDIF

C--   Start nThreads concurrent threads.   
C     Note: We do a fiddly check here. The check is performed
C           by CHECK_THREADS. CHECK_THREADS does a count
C           of all the threads. If after ten seconds it has not
C           found nThreads threads are running it flags an
C           error. This traps the case in which the input 
C           parameter nThreads is different from the actual 
C           number of concurrent threads the OS gives us. This
C           case causes a deadlock if we do not trap it here.
#include "MAIN_PDIRECTIVES1.h"
      DO I=1,nThreads
        myThid = I

C--     Do check to see if there are nThreads threads running
        IF ( .NOT. eeBootError ) THEN
         CALL CHECK_THREADS( myThid )
        ENDIF

C--     Invoke nThreads instances of the numerical model
        IF ( .NOT. eeBootError ) THEN
         CALL THE_MODEL_MAIN(myThid)
        ENDIF

C--     Each threads sets flag indicating it is done
        threadIsComplete(myThid) = .TRUE.
        IF ( .NOT. eeBootError ) THEN
         _BARRIER
        ENDIF
      ENDDO
#include "MAIN_PDIRECTIVES2.h"

  999 CONTINUE
C--   Shut down execution environment
      CALL EEDIE

C--   Write closedown status
      IF ( fatalError ) THEN
       STOP 'ABNORMAL END: PROGRAM MAIN'
      ELSE
       STOP 'NORMAL END'
      ENDIF
C
      END
