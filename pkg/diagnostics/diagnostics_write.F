C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_write.F,v 1.9 2005/02/06 23:19:02 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

      SUBROUTINE DIAGNOSTICS_WRITE ( myTime, myIter, myThid ) 
C***********************************************************************
C  Purpose
C  -------
C    Output sequence for the (multiple) diagnostics output files
C
C  Arguments  Description
C  ----------------------
C     myTime :: Current time of simulation ( s )
C     myIter :: Current Iteration Number
C     myThid :: my thread Id number
C***********************************************************************
       IMPLICIT NONE
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      _RL     myTime
      INTEGER myIter, myThid

c Local variables
c ===============
      INTEGER   n
      INTEGER   myItM1, wrIter
      LOGICAL   dump2fileNow

      IF ( myIter.NE.nIter0 ) THEN
        myItM1 = myIter - 1

C***********************************************************************
C***   Check to see IF its time for Diagnostic Output                ***
C***********************************************************************

        DO n = 1,nlists
          IF ( freq(n).LT.0 ) THEN
            dump2fileNow = MOD(myItM1,-freq(n)) .EQ. INT(-freq(n)/2)
C--     write snap-shot with suffix = myIter to be consistent with 
C       time-average diagnostics (e.g., freq=-1 & freq=1):
c           wrIter = myIter
C--     write snap-shot with suffix = myIter-1 to be consistent with
C       state-variable time-step:
            wrIter = myItM1
          ELSE
            dump2fileNow = MOD(myIter,freq(n)) .EQ. 0
            wrIter = myIter
          ENDIF
          IF ( dump2fileNow ) THEN
            CALL DIAGNOSTICS_OUT(n,wrIter,myThid)
          ENDIF
        ENDDO

C-      wait for everyone before setting arrays to zero: 
        _BARRIER
        DO n = 1,nlists
          IF ( freq(n).LT.0 ) THEN
            dump2fileNow = MOD(myItM1,-freq(n)) .EQ. INT(-freq(n)/2)
          ELSE
            dump2fileNow = MOD(myIter,freq(n)) .EQ. 0
          ENDIF
          IF ( dump2fileNow ) CALL CLRINDX(n,myThid)
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

      RETURN 
      END
