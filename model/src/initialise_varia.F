C $Header: /u/gcmpack/MITgcm/model/src/initialise_varia.F,v 1.2 2000/06/21 19:03:39 adcroft Exp $

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

C     == Routine arguments ==
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
#ifdef ALLOW_OBCS
      INTEGER bi,bj,K
#endif

#ifdef ALLOW_GMREDI
C--   Initialize GM/Redi parameterization
      IF (use_GMRedi) CALL GMREDI_INIT( myThid )
#endif

#ifdef ALLOW_KPP
C--   Initialize KPP vertical mixing scheme.
      IF (use_KPPmixing) THEN
       CALL KPP_INIT( myThid )
       CALL KPP_OPEN_DIAGS( myThid )
      ENDIF
#endif

C--   Initialise model fields.
C     Starting values of U, V, W, temp., salt. and tendency terms
C     are set here. Fields are either set to default or read from
C     stored files.
      CALL INI_FIELDS( myThid )
      _BARRIER

C--   Initialise time-stepping (predictor variables are carried in G)
      IF ( startTime .EQ. 0. ) 
     & CALL INI_PREDICTOR( myThid )
      _BARRIER

C--   Custom routine to set forcing fields
      CALL INI_FORCING( myThid )

#ifdef ALLOW_OBCS
C--   Calculate values on open boundaries at iter=nIter0
      IF (openBoundaries) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
           CALL SET_OBCS( K, bi, bj, startTime, myThid )
Caja       CALL CYCLE_OBCS( K, bi, bj, myThid )
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif

C--   Finally summarise the model state
      CALL STATE_SUMMARY( myThid )

      END
