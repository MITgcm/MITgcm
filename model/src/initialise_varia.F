C $Header: /u/gcmpack/MITgcm/model/src/initialise_varia.F,v 1.9 2001/03/06 17:19:25 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INITIALISE_VARIA(myThid)
C     /==========================================================\
C     | SUBROUTINE INITIALISE_VARIA                              |
C     | o Set the initial conditions for dynamics variables      |
C     |   and time dependent arrays                              |
C     |==========================================================|
C     | This routine reads/writes  data from an input file and   |
C     | from various binary files.                               |
C     | Each thread invokes an instance of this routine as does  |
C     | each process in a multi-process parallel environment like|
C     | MPI.                                                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_OBCS
#include "DYNVARS.h"
#endif

C     == Routine arguments ==
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
      INTEGER bi,bj,K,iMin,iMax,jMin,jMax

      _BARRIER
C--   Initialise model fields.
C     Starting values of U, V, W, temp., salt. and tendency terms
C     are set here. Fields are either set to default or read from
C     stored files.
      CALL INI_FIELDS( myThid )
      _BARRIER

C-- IMPORTANT : Need to activate the following call to restart from 
C     a pickup file written by MITgcmUV_checkpoint34 or earlier.
C     IF ( startTime .NE. 0. ) THEN
C       CALL THE_CORRECTION_STEP(startTime, nIter0, myThid)
C     ENDIF

C--   Initial conditions are convectively adjusted (for historical reasons)
      IF ( startTime .EQ. 0. ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           iMin=1-Olx
           iMax=sNx+Olx
           jMin=1-Oly
           jMax=sNy+Oly
           CALL CONVECTIVE_ADJUSTMENT(
     I       bi, bj, iMin, iMax, jMin, jMax,
     I       startTime, nIter0, myThid )
         ENDDO
        ENDDO
        _BARRIER
      END IF

C--   Initialize variable data for packages
      CALL PACKAGES_INIT_VARIABLES( myThid )

C--   Finally summarise the model state
      CALL STATE_SUMMARY( myThid )

#ifdef ALLOW_TIMEAVE
C--   initialise time-average arrays with initial state values
      IF (taveFreq.GT.0.) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           CALL TIMEAVE_STATVARS(startTime, nIter0, bi, bj, myThid)
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_TIMEAVE */

      RETURN
      END
