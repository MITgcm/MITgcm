C $Header: /u/gcmpack/MITgcm/model/src/Attic/initialise.F,v 1.5 1998/06/30 17:21:11 cnh Exp $

#include "CPP_EEOPTIONS.h"

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

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER myThid
CEndOfInterface

C--   Set model parameters.
C     Parameters are set to defaults and then updates are read from
C     an input file called data.
      CALL INI_PARMS(  myThid )
      _BARRIER

C--   Initialise map of depths
      CALL INI_DEPTHS( myThid )
      _BARRIER

C--   Set model grid.
C     Variables defining model grid spacing are defined.
C     Volumes and areas are set according to grid and depth map.
      CALL INI_GRID( myThid )
      _BARRIER

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

C--   Set laplace operators for use in conjugate gradient solver.
      CALL INI_CG2D( myThid )

C--   Custom routine to set forcing fields
      CALL INI_FORCING( myThid )

C--   Finally summarise the model cofiguration and state
      CALL CONFIG_SUMMARY( myThid )
      CALL STATE_SUMMARY( myThid )

      END
