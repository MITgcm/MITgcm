C $Header: /u/gcmpack/MITgcm/model/src/initialise_varia.F,v 1.12 2001/05/29 14:01:37 adcroft Exp $
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
#include "DYNVARS.h"

C     == Routine arguments ==
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
      INTEGER bi,bj,K,iMin,iMax,jMin,jMax

      _BARRIER
C--   Initialize DYNVARS arrays (state fields + G terms: Gu,Gv,...) to zero [always]
      CALL INI_DYNVARS( myThid )

C--   Initialise model fields.
C     Starting values of U, V, W, temp., salt. and tendency terms
C     are set here. Fields are either set to default or read from
C     stored files.
      CALL INI_FIELDS( myThid )
      _BARRIER

      IF ( usePickupBeforeC35 ) THEN
C-- IMPORTANT : Need to activate the following call to restart from 
C     a pickup file written by MITgcmUV_checkpoint34 or earlier.
      IF ( startTime .NE. 0. ) THEN
       CALL THE_CORRECTION_STEP(startTime, nIter0, myThid)
      ENDIF
      ENDIF

C--   Initial conditions are convectively adjusted (for historical reasons)
      IF ( startTime .EQ. 0. ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           iMin=1-Olx
           iMax=sNx+Olx
           jMin=1-Oly
           jMax=sNy+Oly
           CALL CONVECTIVE_ADJUSTMENT_INI(
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

#ifdef EXACT_CONSERV
      IF (exactConserv) THEN
C--   Compute the initial Barotropic Flow Divergence :
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           CALL CALC_EXACT_ETA( bi,bj, uVel,vVel,
     I                          startTime, nIter0, myThid )
         ENDDO
        ENDDO
      ENDIF
#endif /* EXACT_CONSERV */ 
      
      RETURN
      END
