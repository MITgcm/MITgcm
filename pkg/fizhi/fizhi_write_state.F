C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_write_state.F,v 1.10 2009/08/27 18:00:01 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
CBOP
C     !ROUTINE: FIZHI_WRITE_STATE
C     !INTERFACE:
      SUBROUTINE FIZHI_WRITE_STATE( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FIZHI_WRITE_STATE
C     | o Writes current state of fizhi package to an output file
C     | o     Called from do_the_model_io
C     *==========================================================*
C     \ev

C     !USES:
CEOP
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"
#include "fizhi_land_coms.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime  :: current time
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (
     &     DIFFERENT_MULTIPLE(dumpFreq,myTime,deltaTClock)
     &  .OR. dumpInitAndLast.AND.( myTime.EQ.endTime .OR.
     &                             myTime.EQ.startTime  )
     &   ) THEN

C--   Write fields:
       prec = writeBinaryPrec
       WRITE( fn, '(A,I10.10)' ) 'fizhi_U.', myIter
       CALL WRITE_REC_3D_RL( fn,prec, Nrphys, uphy, 1, myIter,myThid )
       WRITE( fn, '(A,I10.10)' ) 'fizhi_V.', myIter
       CALL WRITE_REC_3D_RL( fn,prec, Nrphys, vphy, 1, myIter,myThid )
       WRITE( fn, '(A,I10.10)' ) 'fizhi_T.', myIter
       CALL WRITE_REC_3D_RL( fn,prec, Nrphys, thphy, 1, myIter,myThid )
       WRITE( fn, '(A,I10.10)' ) 'fizhi_S.', myIter
       CALL WRITE_REC_3D_RL( fn,prec, Nrphys, sphy, 1, myIter,myThid )

      ENDIF

      RETURN
      END
