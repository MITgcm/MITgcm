C $Header: /u/gcmpack/MITgcm/model/src/Attic/initialise.F,v 1.13 1999/08/30 18:29:27 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INITIALISE(myThid)
C     /==========================================================\
C     | SUBROUTINE INITIALISE                                    |
C     | o Master controlling routine for setting model initial   |
C     |   conditions.                                            |
C     |==========================================================|
C     | INITIALISE is invoked at the start of the model to set   |
C     | the model arrays. It reads data from an input file and   |
C     | from various binary files.                               |
C     | Each thread invokes an instance of this routine as does  |
C     | each process in a multi-process parallel environment like|
C     | MPI.                                                     |
C     | Notes                                                    |
C     | =====                                                    |
C     | C*P* comments indicating place holders for which code is |
C     |      presently being developed.                          |
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
      INTEGER bi,bj,K

C--   Set model parameters.
C     Parameters are set to defaults and then updates are read from
C     an input file called data.
      CALL INI_PARMS(  myThid )
      _BARRIER

#ifdef ALLOW_OBCS
C--   Open boundaries positions
      CALL INI_OBCS( myThid )
#endif
      _BARRIER

C--   Set model grid.
C     Variables defining model grid spacing are defined.
      CALL INI_GRID( myThid )
      _BARRIER

C--   Initialise map of depths
      CALL INI_DEPTHS( myThid )
      _BARRIER

C--   Derive masks, lopping factors and recipricols of quantities.
C     Volumes and areas are set according to grid and depth map.
      CALL INI_MASKS_ETC( myThid )
      _BARRIER

#ifdef INCLUDE_LAT_CIRC_FFT_FILTER_CODE
C--   Latitude circle filter initialisation
      CALL FILTER_LATCIRC_FFT_INIT(myThid)
      _BARRIER
#endif

#ifdef ALLOW_KPP
C--   Initialize kppmix vertical mixing scheme.
      CALL KMIXINIT( myThid )
      _BARRIER
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

C--   Set coriolis operators
      CALL INI_CORI( myThid )

C--   Set laplace operators for use in 2D conjugate gradient solver.
      CALL INI_CG2D( myThid )

#ifdef ALLOW_NONHYDROSTATIC
C--   Set laplace operators for use in 3D conjugate gradient solver.
      CALL INI_CG3D( myThid )
#endif

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

C--   Finally summarise the model cofiguration and state
      CALL CONFIG_SUMMARY( myThid )
      CALL STATE_SUMMARY( myThid )

      END
